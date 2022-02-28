//! Translates the three code representation of a graph into an arm
//! representation, with temporaries instead of registers.
use crate::backend::arm::arm_repr::{CmpOp, MemOp, MemOperand, MulOp};

use super::{
    super::{
        super::graph::Graph,
        super::intermediate::{DataRef, VarRepr},
        three_code::{BinOp, Function, OpSrc, Size, StatCode, StatNode, ThreeCode},
    },
    arm_repr::{
        ArmNode, Cond, ControlFlow, Data, DataIdent, DataKind, FlexOffset, FlexOperand, Ident,
        MovOp, Program, RegOp, Shift, Stat, Subroutine, Temporary,
    },
    int_constraints::ConstrainedInt,
};
use std::{
    collections::{HashMap, HashSet},
    ops::DerefMut,
    panic,
};

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
        Ident::Temp(match self.0.get(&id) {
            Some(t) => *t,
            None => {
                let newid = self.get_new_id();
                self.0.insert(id, newid);
                newid
            }
        })
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
) -> Program {
    todo!()
}

/// Translate the data section from the threecode to the arm representation.
fn translate_data(data_refs: HashMap<DataRef, Vec<u8>>) -> Vec<Data> {
    data_refs
        .into_iter()
        .map(|(data_ref, content)| {
            Data(
                convert_data_ref(data_ref),
                DataKind::Ascii(
                    String::from_utf8(content).expect("String must be composed of utf8"),
                ),
            )
        })
        .collect::<Vec<_>>()
}

/// Holds the state of a StatNode in the threecode.
enum TransState {
    /// Node has not been translated, the following arm nodes want to use it
    /// as their successor.
    Waiting(Vec<ArmNode>),
    /// The translation has been created, this is the arm node.
    Created(ArmNode),
}

/// A hashmap of the state of StatNodes used as destinations in branches.
/// It is used to determine which nodes to create.
struct NodeTracker(HashMap<StatNode, TransState>);

impl NodeTracker {
    /// If the translation exists, get it. Else add to the waiting list for
    /// successor to be added.
    fn wait_for_translation(&mut self, from: ArmNode, to: StatNode) -> Option<ArmNode> {
        match self.0.get_mut(&to) {
            Some(TransState::Waiting(waiting_nodes)) => {
                waiting_nodes.push(from);
                None
            }
            Some(TransState::Created(node)) => Some(node.clone()),
            None => {
                self.0.insert(to, TransState::Waiting(vec![from]));
                None
            }
        }
    }

    /// Check if a translation already exists. If one does, then return it, else
    /// None.
    fn check_translation(&self, to: StatNode) -> Option<ArmNode> {
        match self.0.get(&to) {
            Some(TransState::Created(node)) => Some(node.clone()),
            _ => None,
        }
    }

    /// Set the translation for a given node, updating any successors if they
    /// are waiting. Panics if attempting to add a translation for a node
    /// already created.
    fn add_translation(&mut self, three: StatNode, arm: ArmNode) {
        if let Some(entry) = self.0.get_mut(&three) {
            match entry {
                TransState::Waiting(waiting_nodes) => {
                    for node in waiting_nodes {
                        node.set_successor(arm.clone())
                    }
                }
                TransState::Created(_) => {
                    panic!("Cannot add a translation for a node that already exists")
                }
            }
        }

        self.0.insert(three, TransState::Created(arm));
    }

    /// Get the next node being waited on, to translate.
    fn get_next_to_translate(&self) -> Option<StatNode> {
        self.0
            .iter()
            .find(|(three, transstate)| matches!(transstate, TransState::Waiting(_)))
            .map(|(three, _)| three.clone())
    }
}

/// Start building up an arm graph from a given node.
fn translate_from_node(
    node: StatNode,
    int_handler: Option<String>,
    graph: &mut Graph<ControlFlow>,
) -> ArmNode {
    todo!()
}

/// Translate a three-code function.
fn translate_function(
    Function {
        args,
        code,
        read_ref,
    }: Function,
    graph: &mut Graph<ControlFlow>,
) -> Subroutine {
    todo!()
}

/// Link two pairs of node chain starts and ends together.
/// ```text
/// 1. (start <-> leftmiddle) (rightmiddle <-> end)
/// 2. start <-> leftmiddle <-> rightmiddle -> end
/// 3. start <-> end
/// ```
fn chain_two_nodes(
    (start, mut leftmiddle): (ArmNode, ArmNode),
    (mut rightmiddle, end): (ArmNode, ArmNode),
) -> (ArmNode, ArmNode) {
    leftmiddle.set_successor(rightmiddle.clone());
    rightmiddle.set_predecessor(leftmiddle);
    (start, end)
}

/// Given tuples of start and end nodes, chain them together.
/// ```text
/// 1. [(1 <-> 2), (3 <-> 4), ..., (n-1 <-> n)]
/// 2. 1 <-> 2 <-> 3 <-> 4 <-> ... <-> n-1 <-> n
/// 3. (1 <-> n)
/// ```
fn chain_nodes(nodes: Vec<(ArmNode, ArmNode)>) -> Option<(ArmNode, ArmNode)> {
    nodes.into_iter().reduce(chain_two_nodes)
}

/// Given a vector of statements, connect them together in a simple chain within
/// the graph. If no statements are provided returns a None, otherwise a some of
/// the start and end node.
fn link_stats(stats: Vec<Stat>, graph: &mut Graph<ControlFlow>) -> Option<(ArmNode, ArmNode)> {
    chain_nodes(
        stats
            .into_iter()
            .map(|stat| simple_node(stat, graph))
            .collect::<Vec<_>>(),
    )
}

/// Create a single simple node from a statement, returning the node as a pair
/// of node references.
fn simple_node(stat: Stat, graph: &mut Graph<ControlFlow>) -> (ArmNode, ArmNode) {
    let node = graph.new_node(ControlFlow::Simple(None, stat, None));
    (node.clone(), node)
}

/// Convert a [DataRef] from the threecode, to a Data Identifier for the Arm
/// Representation (Keeps the three-code and arm representations independent).
fn convert_data_ref(i: DataRef) -> DataIdent {
    i as DataIdent
}

/// Determines if an integer is a left logical shifted 8 bit pattern.
fn is_shifted_8_bit(i: i32) -> bool {
    ((32 - i.leading_zeros()) - i.trailing_zeros()) <= 8
}

/// place a constant in a temporary
fn const_to_reg(dst_ident: Ident, i: i32, graph: &mut Graph<ControlFlow>) -> (ArmNode, ArmNode) {
    if is_shifted_8_bit(i) {
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
) -> (ArmNode, ArmNode) {
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
                    MemOperand::PreIndex(
                        dst_ident,
                        FlexOffset::Expr(ConstrainedInt::from(offset)),
                        false,
                    ),
                ),
            ],
            graph,
        )
        .expect("More that one statement")
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
        .expect("More that one statement")
    }
}

/// Translates a three-code statement, returning the start and end nodes of the
/// statement's graph.
fn translate_statcode(
    code: &StatCode,
    int_handler: Option<String>,
    graph: &mut Graph<ControlFlow>,
    temp_map: &mut TempMap,
    overflow: Option<String>,
) -> (ArmNode, ArmNode) {
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
            }
        }
        StatCode::AssignOp(three_temp_dst, first_op, binop, second_op) => {
            let arm_dst_temp = temp_map.use_temp(*three_temp_dst);
            let mut nodes = Vec::new();

            let mut opsrc_to_reg = |opsrc: &OpSrc| {
                match opsrc {
                    OpSrc::Const(i) => {
                        // Load the constant into a temporary
                        let new_temp = temp_map.get_new_temp();
                        nodes.push(const_to_reg(new_temp, *i, graph));
                        new_temp
                    }
                    OpSrc::DataRef(dataref, offset) => {
                        let new_temp = temp_map.get_new_temp();
                        nodes.push(dataref_to_reg(
                            new_temp,
                            convert_data_ref(*dataref),
                            *offset,
                            temp_map,
                            graph,
                        ));
                        new_temp
                    }
                    OpSrc::Var(var) => temp_map.use_temp(*var),
                }
            };

            let left_reg = opsrc_to_reg(first_op);
            let right_reg = opsrc_to_reg(second_op);

            let compare_op = |cond: Cond, graph| {
                // Move 0 into the register, then compare, if equal set to 1
                // MOV arm_dst_temp, #0
                // CMP left_reg, right_reg
                // MOV(cond) arm_dst_temp, #1
                link_stats(
                    vec![
                        Stat::Move(
                            MovOp::Mov,
                            Cond::Al,
                            false,
                            arm_dst_temp,
                            FlexOperand::Imm(0),
                        ),
                        Stat::Cmp(
                            CmpOp::Cmp,
                            Cond::Al,
                            left_reg,
                            FlexOperand::ShiftReg(right_reg, None),
                        ),
                        Stat::Move(MovOp::Mov, cond, false, arm_dst_temp, FlexOperand::Imm(1)),
                    ],
                    graph,
                )
                .expect("Multiple Statements Used")
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

                    match overflow {
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
                            let check = simple_node(Stat::Link(Cond::Vs, overflow_handler), graph);
                            chain_two_nodes(addition, check)
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
                    match overflow {
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
                            let check = simple_node(Stat::Link(Cond::Vs, overflow_handler), graph);
                            chain_two_nodes(subtraction, check)
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
                    match overflow {
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
                                        holder_temp,
                                        arm_dst_temp,
                                        left_reg,
                                        right_reg,
                                    ),
                                    Stat::Cmp(
                                        CmpOp::Cmp,
                                        Cond::Al,
                                        arm_dst_temp,
                                        FlexOperand::ShiftReg(
                                            holder_temp,
                                            Some(Shift::Asr(31.into())),
                                        ),
                                    ),
                                    Stat::Link(Cond::Ne, overflow_fun),
                                ],
                                graph,
                            )
                            .expect("Multiple statements used")
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
                            Cond::Al,
                            String::from("__aeabi_idiv"),
                            Some(arm_dst_temp),
                            vec![left_reg, right_reg],
                        ),
                        graph,
                    )
                }
                BinOp::Mod =>
                // CALL to the standard library function for modulus
                {
                    simple_node(
                        Stat::Call(
                            Cond::Al,
                            String::from("__aeabi_idivmod"),
                            Some(arm_dst_temp),
                            vec![left_reg, right_reg],
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

            chain_nodes(nodes).expect("Had more than one statement")
        }
        // LDR(size is bytes) three_temp, [temp_ptr]
        StatCode::Load(three_temp, temp_ptr, size) => simple_node(
            Stat::MemOp(
                MemOp::Ldr,
                Cond::Al,
                size == &Size::Byte,
                temp_map.use_temp(*three_temp),
                MemOperand::Zero(temp_map.use_temp(*temp_ptr)),
            ),
            graph,
        ),
        // STR(size is bytes) three_temp, [temp_ptr]
        StatCode::Store(three_temp, temp_ptr, size) => simple_node(
            Stat::MemOp(
                MemOp::Str,
                Cond::Al,
                size == &Size::Byte,
                temp_map.use_temp(*three_temp),
                MemOperand::Zero(temp_map.use_temp(*temp_ptr)),
            ),
            graph,
        ),
        // Creates a dummy call node for use when allocating registers
        StatCode::Call(ret_temp, fun_name, args) => simple_node(
            Stat::Call(
                Cond::Al,
                fun_name.clone(),
                Some(temp_map.use_temp(*ret_temp)),
                args.iter()
                    .map(|t| temp_map.use_temp(*t))
                    .collect::<Vec<_>>(),
            ),
            graph,
        ),
        // Creates a dummy call for a void function
        StatCode::VoidCall(fun_name, args) => simple_node(
            Stat::Call(
                Cond::Al,
                fun_name.clone(),
                None,
                args.iter()
                    .map(|t| temp_map.use_temp(*t))
                    .collect::<Vec<_>>(),
            ),
            graph,
        ),
    }
}

impl ArmNode {
    /// Set the predecessor arm node, for Simple, Branch and Return the start
    /// node is set, for Multi the predecessor is added to the predecessors list.
    fn set_predecessor(&mut self, predecessor: ArmNode) {
        match self.get_mut().deref_mut() {
            ControlFlow::Simple(pre, _, _)
            | ControlFlow::Branch(pre, _, _, _)
            | ControlFlow::Return(pre, _) => {
                let _ = pre.insert(predecessor);
            }
            ControlFlow::Multi(pres, _) => pres.push(predecessor),
            ControlFlow::Removed => panic!("Cannot add a predecessor to a deleted node"),
        }
    }

    /// Set the successor node to an arm node.
    fn set_successor(&mut self, successor: ArmNode) {
        match self.get_mut().deref_mut() {
            ControlFlow::Simple(_, _, succ)
            | ControlFlow::Branch(_, _, _, succ)
            | ControlFlow::Multi(_, succ) => {
                let _ = succ.insert(successor);
            }
            ControlFlow::Return(_, _) => panic!("There are no nodes after a return"),
            ControlFlow::Removed => panic!("There are no nodes connected to a removed node"),
        }
    }
}

mod tests {
    use super::*;
    use lazy_static::__Deref;

    fn check_simple_chain(stats: Vec<Stat>, start: ArmNode, end: ArmNode) {
        let mut prev_node: Option<ArmNode> = None;
        let mut current_node = Some(start);

        for stat in stats.into_iter() {
            match current_node {
                Some(node) => match node.get().deref() {
                    ControlFlow::Simple(prev, curr, next) => {
                        assert_eq!(prev, &prev_node);
                        prev_node = Some(node.clone());
                        assert_eq!(curr, &stat);
                        current_node = next.clone();
                    }
                    _ => panic!("Was not a simple node"),
                },
                None => panic!("Expected a node but there was none"),
            }
        }
    }

    #[test]
    fn can_link_simple_statements() {
        let example_stat = Stat::ApplyOp(
            RegOp::Add,
            Cond::Al,
            false,
            Ident::Temp(0),
            Ident::Temp(0),
            FlexOperand::Imm(7),
        );
        let stats = vec![
            Stat::ApplyOp(
                RegOp::Add,
                Cond::Al,
                false,
                Ident::Temp(0),
                Ident::Temp(0),
                FlexOperand::Imm(7),
            ),
            Stat::ApplyOp(
                RegOp::Sub,
                Cond::Ge,
                false,
                Ident::Temp(1),
                Ident::Temp(1),
                FlexOperand::Imm(8),
            ),
            Stat::ApplyOp(
                RegOp::Sub,
                Cond::Lt,
                false,
                Ident::Temp(7),
                Ident::Temp(8),
                FlexOperand::ShiftReg(Ident::Temp(1), Some(Shift::Lsl(3.into()))),
            ),
            Stat::ApplyOp(
                RegOp::Adc,
                Cond::Lt,
                true,
                Ident::Temp(7),
                Ident::Temp(8),
                FlexOperand::ShiftReg(Ident::Temp(2), Some(Shift::Lsl(3.into()))),
            ),
        ];

        let mut graph = Graph::new();

        let (start, end) =
            link_stats(stats.clone(), &mut graph).expect("unexpectedly failed to link statements");
        check_simple_chain(stats, start, end)
    }

    #[test]
    #[should_panic]
    fn can_check_for_linked_chains() {
        let mut graph = Graph::new();

        let false_start = graph.new_node(ControlFlow::Simple(
            None,
            Stat::Call(Cond::Al, String::from("hello"), None, vec![]),
            None,
        ));
        let false_end = graph.new_node(ControlFlow::Simple(
            None,
            Stat::Call(Cond::Al, String::from("world"), None, vec![]),
            None,
        ));

        check_simple_chain(
            vec![
                Stat::Call(Cond::Al, String::from("hello"), None, vec![]),
                Stat::Call(Cond::Al, String::from("world"), None, vec![]),
            ],
            false_start,
            false_end,
        )
    }

    #[test]
    fn detects_left_shifted_8_bits() {
        assert!(is_shifted_8_bit(0b10000001 as i32));
        assert!(is_shifted_8_bit(0b10111 as i32));
        assert!(!is_shifted_8_bit(0b1000000111 as i32));
    }
}
