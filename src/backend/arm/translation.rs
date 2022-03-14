//! Translates the three code representation of a graph into an arm
//! representation, with temporaries instead of registers.
//!
//! Several special nodes are created, for calls, returns and assignment of the
//! reserved stack space for functions. These are translated into arm instructions
//! during register allocation.

use crate::{
    graph::Graph,
    intermediate::{DataRef, VarRepr},
};
use lazy_static::__Deref;

use super::{
    super::three_code::{
        BinOp, DataRefType, Function, OpSrc, Size, StatCode, StatNode, StatType, ThreeCode,
    },
    arm_graph_utils::{
        is_8_bit, link_chains, link_stats, link_two_chains, link_two_nodes, simple_node, Chain,
    },
    arm_repr::{
        ArmCode, ArmNode, CmpOp, Cond, ControlFlow, Data, DataIdent, DataType, FlexOffset,
        FlexOperand, Ident, MemOp, MemOperand, MovOp, MulOp, RegOp, Shift, Stat, Subroutine,
        Temporary,
    },
    int_constraints::ConstrainedInt,
};

use std::collections::{HashMap, HashSet};

/// A struct for tracking names of arm representation temporaries. Is used to
/// allocate new temporary identifiers, and maintain the mappings of three-code
/// temporaries to arm-temporaries.
struct TempMap(HashMap<VarRepr, Temporary>, HashSet<Temporary>);

impl TempMap {
    /// Create a new map for threecode->arm temporary translation.
    fn new() -> Self {
        Self(HashMap::new(), HashSet::new())
    }

    /// Given a three-code identifier, either get the current arm-temporary
    /// representing it, or generate a new one and return that.
    fn use_temp(&mut self, id: VarRepr) -> Ident {
        Ident::Temp(self.use_id(id))
    }

    /// generate a new temporary Id.
    fn use_id(&mut self, id: VarRepr) -> Temporary {
        match self.0.get(&id) {
            Some(t) => *t,
            None => {
                let newid = self.get_new_id();
                self.0.insert(id, newid);
                newid
            }
        }
    }

    /// Generate a new temporary identifier as a [Temporary].
    fn get_new_id(&mut self) -> Temporary {
        let newid = self.1.len() as Temporary;
        self.1.insert(newid);
        newid
    }

    /// Get a new temporary identifier.
    fn get_new_temp(&mut self) -> Ident {
        Ident::Temp(self.get_new_id())
    }

    /// Get the hashset of temporaries
    fn get_hashset(self) -> HashSet<Temporary> {
        self.1
    }
}

/// Translate a threecode representation into a program.
pub(super) fn translate_threecode(
    ThreeCode {
        functions,
        data_refs,
        graph,
        read_ref,
        code,
        int_handler,
    }: ThreeCode,
) -> ArmCode {
    let mut cfg = Graph::new();
    let int_handler = int_handler.as_ref();
    let mut temp_map = TempMap::new();
    let arm_code = ArmCode {
        data: translate_data(data_refs),
        reserved_stack: if read_ref { 1 } else { 0 },
        main: translate_routine(code, int_handler, &mut temp_map, &mut cfg),
        temps: temp_map.get_hashset(),
        subroutines: functions
            .into_iter()
            .map(|(name, fun)| (name, translate_function(fun, int_handler)))
            .collect::<HashMap<_, _>>(),
        cfg,
    };
    drop(graph);
    arm_code
}

/// Translate the data section from the threecode to the arm representation.
fn translate_data(data_refs: HashMap<DataRef, DataRefType>) -> Vec<Data> {
    data_refs
        .into_iter()
        .map(|(data_ref, data)| -> Data {
            match data {
                DataRefType::String(string) => Data(
                    convert_data_ref(data_ref),
                    vec![DataType::Ascii(
                        String::from_utf8(string).expect("Is always valid Ascii"),
                    )],
                ),
                DataRefType::Struct(types) => {
                    // change struct into types, note that all Bytes are collected into strings.
                    let (mut datas, string) = types
                        .into_iter()
                        .map(|data| match data {
                            (Size::Byte, val) => DataType::Byte(val as u8),
                            (Size::Word, val) => DataType::HalfWord(val as i16),
                            (Size::DWord, val) => DataType::Word(val),
                        })
                        .fold(
                            (vec![], vec![]),
                            |(mut datas, mut chars): (Vec<DataType>, Vec<u8>), next| {
                                if let DataType::Byte(char_val)  = next && char_val < 128 {
                                    // if it is a standard ascii character, push it
                                    chars.push(char_val);
                                    (datas, chars)
                                } else if chars.is_empty() {
                                    datas.push(next);
                                    (datas, chars)
                                } else {
                                    datas.push(DataType::Ascii(
                                        String::from_utf8(chars).expect("valid ascii"),
                                    ));
                                    (datas, vec![])
                                }
                            },
                        );

                    // place any string on the end of the data section into datas
                    if !string.is_empty() {
                        datas.push(DataType::Ascii(
                            String::from_utf8(string).expect("valid ascii"),
                        ))
                    }

                    Data(convert_data_ref(data_ref), datas)
                }
            }
        })
        .collect::<Vec<_>>()
}

fn require_label(
    node: StatNode,
    translate_map: &mut HashMap<StatNode, ArmNode>,
    graph: &mut Graph<ControlFlow>,
    start: bool,
) -> Option<ArmNode> {
    match node.get().deref() {
        StatType::Simple(precs, _, _)
        | StatType::Branch(precs, _, _, _)
        | StatType::Loop(precs)
        | StatType::Return(precs, _) => {
            // if more than one predecessor, we need a multi to allow many paths to enter.
            if precs.len() > 1 || (!precs.is_empty() && start) {
                // If more predecessors, create a label, place in the
                // map to found and used.
                let label = graph.new_node(ControlFlow::Multi(vec![], None));
                translate_map.insert(node.clone(), label.clone());
                Some(label)
            } else {
                None
            }
        }
        StatType::Dummy(_) => panic!("No dummy nodes should be in the final threecode"),
    }
}

fn translate_node_inner(
    node: StatNode,
    int_handler: Option<&String>,
    temp_map: &mut TempMap,
    translate_map: &mut HashMap<StatNode, ArmNode>,
    graph: &mut Graph<ControlFlow>,
) -> (ArmNode, Option<(ArmNode, StatNode)>) {
    match node.get().deref() {
        StatType::Simple(_, stat, succ) => {
            let Chain(start, end) = translate_statcode(stat, int_handler, graph, temp_map);
            (start, Some((end, succ.clone())))
        }
        StatType::Branch(_, opsrc, true_branch, false_branch) => {
            // Recur to get the true_branch
            let mut true_branch = translate_from_node(
                true_branch.clone(),
                int_handler,
                temp_map,
                translate_map,
                graph,
            );

            let (arm_temp, opsrc_chain) = opsrc_to_temp(opsrc, temp_map, graph);

            let cmp_chain = simple_node(
                Stat::Cmp(CmpOp::Cmp, Cond::Al, arm_temp, FlexOperand::Imm(1)),
                graph,
            );

            let branch = graph.new_node(ControlFlow::Branch(
                None,
                true_branch.clone(),
                Cond::Eq,
                None,
            ));

            let Chain(start, end) = if let Some(chain) = opsrc_chain {
                link_two_chains(chain, cmp_chain)
            } else {
                cmp_chain
            };

            link_two_nodes(end, branch.clone());
            true_branch.set_predecessor(branch.clone());
            (start, Some((branch, false_branch.clone())))
        }
        StatType::Loop(_) => {
            // Create a label, create an unconditional branch to the label, there is no end to chain to.
            let mut label = graph.new_node(ControlFlow::Multi(vec![], None));
            let branch = graph.new_node(ControlFlow::Branch(
                Some(label.clone()),
                label.clone(),
                Cond::Al,
                None,
            ));
            label.set_successor(branch);
            (label, None)
        }
        StatType::Return(_, opt_opsrc) => (
            {
                if let Some(opsrc) = opt_opsrc {
                    let (arm_temp, opsrc_chain) = opsrc_to_temp(opsrc, temp_map, graph);
                    let ret_node =
                        graph.new_node(ControlFlow::Return(None, Some(arm_temp.get_temp())));
                    if let Some(Chain(start, end)) = opsrc_chain {
                        link_two_nodes(end, ret_node);
                        start
                    } else {
                        ret_node
                    }
                } else {
                    graph.new_node(ControlFlow::Return(None, None))
                }
            },
            None,
        ),
        StatType::Dummy(_) => panic!("No dummy nodes should be in the final threecode"),
    }
}

/// Translate a single node, providing the start and end nodes.
fn translate_node(
    node: StatNode,
    int_handler: Option<&String>,
    temp_map: &mut TempMap,
    translate_map: &mut HashMap<StatNode, ArmNode>,
    graph: &mut Graph<ControlFlow>,
) -> (ArmNode, Option<(ArmNode, StatNode)>) {
    match translate_map.get(&node) {
        Some(node) => (node.clone(), None),
        None => {
            // determine if a label is needed
            let label = require_label(node.clone(), translate_map, graph, false);

            // translate the statement to get the start, and (potentially) and end.
            let (start, next) =
                translate_node_inner(node, int_handler, temp_map, translate_map, graph);

            if let Some(label_node) = label {
                link_two_nodes(label_node.clone(), start);
                (label_node, next)
            } else {
                (start, next)
            }
        }
    }
}

/// Start building up an arm graph from a given node.
fn translate_from_node(
    node: StatNode,
    int_handler: Option<&String>,
    temp_map: &mut TempMap,
    translate_map: &mut HashMap<StatNode, ArmNode>,
    graph: &mut Graph<ControlFlow>,
) -> ArmNode {
    // Translate the first node, the continue translating until there are no nodes to go to.
    let (first, next) = translate_node(node, int_handler, temp_map, translate_map, graph);

    if let Some((mut prev, mut next_stat_node)) = next {
        loop {
            let (mut trans_start, next_nodes) =
                translate_node(next_stat_node, int_handler, temp_map, translate_map, graph);

            // connect the end of the previous arm node, to the start of this translation
            trans_start.set_predecessor(prev.clone());
            prev.set_successor(trans_start.clone());

            // if there is an end, and next node. Then set prev to be the end,
            // and translate the next node. (who's start will be linkled with
            // this end).
            if let Some((trans_end, next)) = next_nodes {
                prev = trans_end;
                next_stat_node = next
            } else {
                break;
            }
        }
    }

    first
}

/// Start building up a routine (subroutine or main code body) from a start node.
fn translate_routine(
    start: StatNode,
    int_handler: Option<&String>,
    temp_map: &mut TempMap,
    graph: &mut Graph<ControlFlow>,
) -> ArmNode {
    // Normal nodes require labels for 1 or more predecessors, the first node
    // of a function requires a label if it has any predecessors.
    let mut translate_map = HashMap::new();
    let label = require_label(start.clone(), &mut translate_map, graph, true);
    let (mut start, next) =
        translate_node_inner(start, int_handler, temp_map, &mut translate_map, graph);

    let first = if let Some(mut label_node) = label {
        start.set_predecessor(label_node.clone());
        label_node.set_successor(start);
        label_node
    } else {
        start
    };

    if let Some((mut next_node, next_three)) = next {
        let mut node_to_rest =
            translate_from_node(next_three, int_handler, temp_map, &mut translate_map, graph);
        node_to_rest.set_predecessor(next_node.clone());
        next_node.set_successor(node_to_rest);
    }
    first
}

/// Translate a three-code function.
fn translate_function(
    Function {
        args,
        code,
        graph: _,
        read_ref,
    }: Function,
    int_handler: Option<&String>,
) -> Subroutine {
    let mut temp_map = TempMap::new();
    let mut cfg = Graph::new();
    Subroutine {
        args: args
            .into_iter()
            .map(|t| temp_map.use_id(t))
            .collect::<Vec<_>>(),
        start_node: translate_routine(code, int_handler, &mut temp_map, &mut cfg),
        temps: temp_map.get_hashset(),
        reserved_stack: if read_ref { 1 } else { 0 },
        cfg,
    }
}

/// Convert a [DataRef] from the threecode, to a Data Identifier for the Arm
/// Representation (Keeps the three-code and arm representations independent).
fn convert_data_ref(i: DataRef) -> DataIdent {
    i as DataIdent
}

/// place a constant in a temporary
fn const_to_reg(dst_ident: Ident, i: i32, graph: &mut Graph<ControlFlow>) -> Chain {
    if is_8_bit(i) {
        // Mov dst_ident, #i
        simple_node(
            Stat::Move(
                MovOp::Mov,
                Cond::Al,
                false,
                dst_ident,
                FlexOperand::Imm(i as u32),
            ),
            graph,
        )
    } else {
        // LDR ident, =i
        simple_node(
            Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dst_ident,
                MemOperand::Expression(i),
            ),
            graph,
        )
    }
}

/// Place a data reference (offset) into a temporary
fn dataref_to_reg(
    dst_ident: Ident,
    data_ident: DataIdent,
    offset: i32,
    temp_map: &mut TempMap,
    graph: &mut Graph<ControlFlow>,
) -> Chain {
    if offset == 0 {
        // LDR dst_ident, =data_ident
        simple_node(
            Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                false,
                dst_ident,
                MemOperand::Label(data_ident),
            ),
            graph,
        )
    } else if offset >= -4095 && offset <= 4095 {
        // LDR arm_temp, =dataref
        // LDR arm_temp, [arm_temp, #offset]
        link_stats(
            vec![
                Stat::MemOp(
                    MemOp::Ldr,
                    Cond::Al,
                    false,
                    dst_ident,
                    MemOperand::Label(data_ident),
                ),
                Stat::MemOp(
                    MemOp::Ldr,
                    Cond::Al,
                    false,
                    dst_ident,
                    MemOperand::PreIndex(dst_ident, FlexOffset::Expr(ConstrainedInt::from(offset))),
                ),
            ],
            graph,
        )
    } else {
        // LDR arm_temp, =dataref
        // LDR other_temp, =offset
        // ADD arm_temp, arm_temp, other_temp
        // note: no overflow checking for offset
        let other_temp = temp_map.get_new_temp();
        link_stats(
            vec![
                Stat::MemOp(
                    MemOp::Ldr,
                    Cond::Al,
                    false,
                    dst_ident,
                    MemOperand::Label(data_ident),
                ),
                Stat::MemOp(
                    MemOp::Ldr,
                    Cond::Al,
                    false,
                    other_temp,
                    MemOperand::Expression(offset),
                ),
                Stat::ApplyOp(
                    RegOp::Add,
                    Cond::Al,
                    false,
                    dst_ident,
                    dst_ident,
                    FlexOperand::ShiftReg(other_temp, None),
                ),
            ],
            graph,
        )
    }
}

/// Move an opsrc into a temporary.
fn opsrc_to_temp(
    opsrc: &OpSrc,
    temp_map: &mut TempMap,
    graph: &mut Graph<ControlFlow>,
) -> (Ident, Option<Chain>) {
    match opsrc {
        OpSrc::Const(i) => {
            // Load the constant into a temporary
            let new_temp = temp_map.get_new_temp();
            (new_temp, Some(const_to_reg(new_temp, *i, graph)))
        }
        OpSrc::DataRef(dataref, offset) => {
            let new_temp = temp_map.get_new_temp();

            (
                new_temp,
                Some(dataref_to_reg(
                    new_temp,
                    convert_data_ref(*dataref),
                    *offset,
                    temp_map,
                    graph,
                )),
            )
        }
        OpSrc::Var(var) => (temp_map.use_temp(*var), None),
        OpSrc::ReadRef => {
            let new_temp = temp_map.get_new_temp();
            (
                new_temp,
                Some(simple_node(Stat::AssignStackWord(new_temp), graph)),
            )
        }
    }
}

/// Translates a three-code statement, returning the start and end nodes of the
/// statement's graph.
fn translate_statcode(
    code: &StatCode,
    int_handler: Option<&String>,
    graph: &mut Graph<ControlFlow>,
    temp_map: &mut TempMap,
) -> Chain {
    match code {
        StatCode::Assign(three_temp, opsrc) => {
            let arm_temp = temp_map.use_temp(*three_temp);
            match opsrc {
                OpSrc::Const(i) => const_to_reg(arm_temp, *i, graph),
                OpSrc::DataRef(dataref, offset) => dataref_to_reg(
                    arm_temp,
                    convert_data_ref(*dataref),
                    *offset,
                    temp_map,
                    graph,
                ),
                OpSrc::Var(other_three_temp) => {
                    // Mov arm_tep other_arm_temp
                    let other_arm_temp = temp_map.use_temp(*other_three_temp);
                    simple_node(
                        Stat::Move(
                            MovOp::Mov,
                            Cond::Al,
                            false,
                            arm_temp,
                            FlexOperand::ShiftReg(other_arm_temp, None),
                        ),
                        graph,
                    )
                }
                OpSrc::ReadRef => simple_node(Stat::AssignStackWord(arm_temp), graph),
            }
        }
        StatCode::AssignOp(three_temp_dst, first_op, binop, second_op) => {
            let arm_dst_temp = temp_map.use_temp(*three_temp_dst);
            let mut nodes = Vec::new();

            let mut opsrc_to_reg = |opsrc: &OpSrc| {
                let (arm_temp, opsrc_chain) = opsrc_to_temp(opsrc, temp_map, graph);
                if let Some(chain) = opsrc_chain {
                    nodes.push(chain);
                }
                arm_temp
            };

            let left_reg = opsrc_to_reg(first_op);
            let right_reg = opsrc_to_reg(second_op);

            let mut compare_op = |cond: Cond, graph| {
                let othertemp = temp_map.get_new_temp();
                // Move 0 into the register, then compare, if equal set to 1
                // MOV arm_dst_temp, #0
                // CMP left_reg, right_reg
                // MOV(cond) arm_dst_temp, #1
                link_stats(
                    vec![
                        Stat::Move(MovOp::Mov, Cond::Al, false, othertemp, FlexOperand::Imm(0)),
                        Stat::Cmp(
                            CmpOp::Cmp,
                            Cond::Al,
                            left_reg,
                            FlexOperand::ShiftReg(right_reg, None),
                        ),
                        Stat::Move(MovOp::Mov, cond, false, othertemp, FlexOperand::Imm(1)),
                        Stat::Move(
                            MovOp::Mov,
                            Cond::Al,
                            false,
                            arm_dst_temp,
                            FlexOperand::ShiftReg(othertemp, None),
                        ),
                    ],
                    graph,
                )
            };

            let basic_apply_op = |op: RegOp, graph| {
                simple_node(
                    Stat::ApplyOp(
                        op,
                        Cond::Al,
                        false,
                        arm_dst_temp,
                        left_reg,
                        FlexOperand::ShiftReg(right_reg, None),
                    ),
                    graph,
                )
            };

            nodes.push(match binop {
                BinOp::Add => {
                    // Perform the addition operation, if there is an overflow
                    // handler, then if overflow occurs, branch to it

                    match int_handler {
                        Some(overflow_handler) => {
                            // ADDS arm_dst_reg, left_reg, right_reg
                            // BLVS overflow_handler
                            let addition = simple_node(
                                Stat::ApplyOp(
                                    RegOp::Add,
                                    Cond::Al,
                                    true,
                                    arm_dst_temp,
                                    left_reg,
                                    FlexOperand::ShiftReg(right_reg, None),
                                ),
                                graph,
                            );
                            let check =
                                simple_node(Stat::Link(Cond::Vs, overflow_handler.clone()), graph);
                            link_two_chains(addition, check)
                        }
                        None => {
                            // ADD arm_dst_temp, left_reg, right_reg
                            simple_node(
                                Stat::ApplyOp(
                                    RegOp::Add,
                                    Cond::Al,
                                    false,
                                    arm_dst_temp,
                                    left_reg,
                                    FlexOperand::ShiftReg(right_reg, None),
                                ),
                                graph,
                            )
                        }
                    }
                }
                BinOp::Sub => {
                    match int_handler {
                        Some(overflow_handler) => {
                            // SUBS arm_dst_temp, left_reg, right_reg
                            // BLVS overflow_handler
                            let subtraction = simple_node(
                                Stat::ApplyOp(
                                    RegOp::Sub,
                                    Cond::Al,
                                    true,
                                    arm_dst_temp,
                                    left_reg,
                                    FlexOperand::ShiftReg(right_reg, None),
                                ),
                                graph,
                            );
                            let check =
                                simple_node(Stat::Link(Cond::Vs, overflow_handler.clone()), graph);
                            link_two_chains(subtraction, check)
                        }
                        None => {
                            // SUB arm_dst_temp, left_reg, right_reg
                            simple_node(
                                Stat::ApplyOp(
                                    RegOp::Sub,
                                    Cond::Al,
                                    false,
                                    arm_dst_temp,
                                    left_reg,
                                    FlexOperand::ShiftReg(right_reg, None),
                                ),
                                graph,
                            )
                        }
                    }
                }
                BinOp::Mul => {
                    match int_handler {
                        Some(overflow_fun) => {
                            // SMULL holder_temp, arm_dst_temp, left_reg, right_reg
                            // CMP arm_dst_temp, holder_temp, ASR #31
                            // BLNE overflow_fun
                            let holder_temp = temp_map.get_new_temp();
                            link_stats(
                                vec![
                                    Stat::MulOp(
                                        MulOp::SMulL,
                                        Cond::Al,
                                        true,
                                        arm_dst_temp,
                                        holder_temp,
                                        left_reg,
                                        right_reg,
                                    ),
                                    Stat::Cmp(
                                        CmpOp::Cmp,
                                        Cond::Al,
                                        holder_temp,
                                        FlexOperand::ShiftReg(
                                            arm_dst_temp,
                                            Some(Shift::Asr(31.into())),
                                        ),
                                    ),
                                    Stat::Link(Cond::Ne, overflow_fun.clone()),
                                ],
                                graph,
                            )
                        }
                        None => {
                            // MUL arm_dst_temp, left_reg, right_reg
                            simple_node(
                                Stat::Mul(Cond::Al, false, arm_dst_temp, left_reg, right_reg),
                                graph,
                            )
                        }
                    }
                }
                BinOp::Div =>
                // CALL to the standard library function for division
                {
                    simple_node(
                        Stat::Call(
                            String::from("__aeabi_idiv"),
                            Some(arm_dst_temp.get_temp()),
                            vec![left_reg.get_temp(), right_reg.get_temp()],
                        ),
                        graph,
                    )
                }
                BinOp::Mod =>
                // CALL to the standard library function for modulus
                {
                    simple_node(
                        Stat::Call(
                            String::from("__modsi3"),
                            Some(arm_dst_temp.get_temp()),
                            vec![left_reg.get_temp(), right_reg.get_temp()],
                        ),
                        graph,
                    )
                }
                BinOp::Eq => compare_op(Cond::Eq, graph),
                BinOp::Ne => compare_op(Cond::Ne, graph),
                BinOp::Gt => compare_op(Cond::Gt, graph),
                BinOp::Gte => compare_op(Cond::Ge, graph),
                BinOp::Lt => compare_op(Cond::Lt, graph),
                BinOp::Lte => compare_op(Cond::Le, graph),
                BinOp::And => basic_apply_op(RegOp::And, graph),
                BinOp::Or => basic_apply_op(RegOp::Orr, graph),
                BinOp::Xor => basic_apply_op(RegOp::Eor, graph),
            });

            link_chains(nodes).expect("Had more than one statement")
        }
        // LDR(size is bytes) three_temp, [temp_ptr]
        StatCode::Load(three_temp, opsrc, size) => {
            // load the opsrc, potentially using more/other instructions
            let (memoperand, opsrc_chain) = match opsrc {
                OpSrc::Const(i) => (MemOperand::Expression(*i), None),
                OpSrc::Var(temp_ptr) => (MemOperand::Zero(temp_map.use_temp(*temp_ptr)), None),
                OpSrc::DataRef(dref, 0) => (MemOperand::Label(convert_data_ref(*dref)), None),
                OpSrc::DataRef(dref, offset) => {
                    let new_temp = temp_map.get_new_temp();
                    (
                        MemOperand::Zero(new_temp),
                        Some(dataref_to_reg(
                            new_temp,
                            convert_data_ref(*dref),
                            *offset,
                            temp_map,
                            graph,
                        )),
                    )
                }
                OpSrc::ReadRef => {
                    let new_temp = temp_map.get_new_temp();
                    (
                        MemOperand::Zero(new_temp),
                        Some(simple_node(Stat::AssignStackWord(new_temp), graph)),
                    )
                }
            };

            let load = simple_node(
                Stat::MemOp(
                    MemOp::Ldr,
                    Cond::Al,
                    size == &Size::Byte,
                    temp_map.use_temp(*three_temp),
                    memoperand,
                ),
                graph,
            );

            // if a preamble is required, link it
            if let Some(chain) = opsrc_chain {
                link_two_chains(chain, load)
            } else {
                load
            }
        }
        // STR(size is bytes) three_temp, [temp_ptr]
        StatCode::Store(temp_ptr, opsrc, size) => {
            let (arm_temp, opsrc_chain) = opsrc_to_temp(opsrc, temp_map, graph);

            let store = simple_node(
                Stat::MemOp(
                    MemOp::Str,
                    Cond::Al,
                    size == &Size::Byte,
                    arm_temp,
                    MemOperand::Zero(temp_map.use_temp(*temp_ptr)),
                ),
                graph,
            );

            // if a preamble is required, link it
            if let Some(chain) = opsrc_chain {
                link_two_chains(chain, store)
            } else {
                store
            }
        }
        // Creates a dummy call node for use when allocating registers
        StatCode::Call(ret_temp, fun_name, args) => simple_node(
            Stat::Call(
                fun_name.clone(),
                Some(temp_map.use_id(*ret_temp)),
                args.iter().map(|t| temp_map.use_id(*t)).collect::<Vec<_>>(),
            ),
            graph,
        ),
        // Creates a dummy call for a void function
        StatCode::VoidCall(fun_name, args) => simple_node(
            Stat::Call(
                fun_name.clone(),
                None,
                args.iter().map(|t| temp_map.use_id(*t)).collect::<Vec<_>>(),
            ),
            graph,
        ),
    }
}
