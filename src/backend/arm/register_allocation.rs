//! Register allocation for the arm representation, using live ranges and
//! 'time-till-use' decision for spilling.
//!
//! An allocation state is used to keep track of register usage, and
//! create/maintain the stack frame.
//!
//! Register allocation makes use of live ranges to determine when temporaries
//! 'die' and hence free up registers.

use super::{
    allocation_state::AllocationState,
    arm_graph_utils::{link_optional_chains, link_two_nodes, Chain},
    arm_repr::{
        ArmCode, ArmNode, Cond, ControlFlow, FlexOffset, FlexOperand, Ident, MemOp, MemOperand,
        Stat, Subroutine, Temporary,
    },
    live_ranges::{get_live_ranges, LiveRanges},
};
use crate::graph::Graph;
use std::{
    collections::{HashMap, HashSet},
    ops::DerefMut,
};

/// Update the program, allocating registers for every instruction.
///
/// Invariants:
/// - Temporary sets must contain all temporaries used in each routine
/// - All instructions must use temporaries as identifiers
/// - Live ranges must be correct for livein.
pub fn allocate_registers(program: ArmCode) -> ArmCode {
    // generate the live ranges for the program.
    let live_ranges = get_live_ranges(&program);

    // translate main program block:
    let ArmCode {
        data,
        reserved_stack,
        main,
        temps,
        subroutines: functions,
        mut cfg,
    } = program;

    ArmCode {
        data,
        reserved_stack,
        main: allocate_for_routine(main, &[], reserved_stack, &temps, &live_ranges, &mut cfg),
        temps,
        subroutines: functions
            .into_iter()
            .map(
                |(
                    name,
                    Subroutine {
                        args,
                        start_node,
                        temps,
                        reserved_stack,
                        mut cfg,
                    },
                )| {
                    let start_node = allocate_for_routine(
                        start_node,
                        &args,
                        reserved_stack,
                        &temps,
                        &live_ranges,
                        &mut cfg,
                    );
                    (
                        name,
                        Subroutine {
                            args,
                            start_node,
                            temps,
                            reserved_stack,
                            cfg,
                        },
                    )
                },
            )
            .collect::<HashMap<String, Subroutine>>(),
        cfg,
    }
}

/// From the start node of a given routine, generate the stack frame
/// (instructions and initial allocation state), and then translate all
/// connecting arm nodes using the provided live ranges and temporaries.
fn allocate_for_routine(
    start_node: ArmNode,
    args: &[Temporary],
    reserved_stack: u8,
    temps_set: &HashSet<Temporary>,
    live_ranges: &LiveRanges,
    graph: &mut Graph<ControlFlow>,
) -> ArmNode {
    let mut state_map = HashMap::new();
    let (alloc_state, Chain(new_start, stack_setup_end)) =
        AllocationState::create_stack_frame(args, reserved_stack, temps_set, graph);

    // connect up the start:
    // start_node.set_predecessor(stack_setup_end.clone());
    // stack_setup_end.set_successor(start_node.clone());
    link_two_nodes(stack_setup_end, start_node.clone());

    let mut current = Some((start_node, alloc_state));

    // while there are still nodes to be translated, translate the nodes using the appropriate state.
    while let Some((current_node, current_state)) = current {
        translate_from_node(
            current_node,
            current_state,
            &mut state_map,
            live_ranges,
            graph,
        );

        current = get_next_node(&mut state_map);
    }

    new_start
}

/// Get the next node to translate from, using a translation map from nodes to
/// states & if they have already been translated.
fn get_next_node(
    state_map: &mut HashMap<ArmNode, (AllocationState, bool)>,
) -> Option<(ArmNode, AllocationState)> {
    for (node, (state, translated)) in state_map.iter_mut() {
        if !*translated {
            *translated = true;
            return Some((node.clone(), state.clone()));
        }
    }
    None
}

/// Starting from a given node, translate all nodes connected linearly (direct
/// successors - for branch only the false-branch). Add indirect successors to
/// the trans_map.
fn translate_from_node(
    mut current_node: ArmNode,
    mut alloc_state: AllocationState,
    state_map: &mut HashMap<ArmNode, (AllocationState, bool)>,
    live_ranges: &LiveRanges,
    graph: &mut Graph<ControlFlow>,
) {
    loop {
        // get the live in and live out
        let (livein, liveout) = &live_ranges[&current_node];

        // update the registers for current live_in
        alloc_state.update_live(&livein);

        // a helper closure for checking conditions
        let load_dst = |cond: &Cond| cond != &Cond::Al;

        // Translate the node, translating all identifiers (temporary) to registers
        // and linking up any necessary instructions before & after, returning the
        // next node to check and the end of the statement to link up with any next nodes.
        let (mut next, mut new_end): (ArmNode, ArmNode) = match current_node
            .clone()
            .get_mut()
            .deref_mut()
        {
            ControlFlow::Simple(Some(prev), Stat::Call(fun_name, ret, args), next) => {
                let Chain(mut start, mut end) =
                    alloc_state.call(fun_name.clone(), ret, args, &liveout, graph);

                start.set_predecessor(prev.clone());
                prev.replace_successor(current_node.clone(), start);

                alloc_state.update_live(&liveout);

                // as we entirely replace the 'call' node, we set the successors and
                // predecessors to point to the ends of our chain of statements
                if let Some(next_node) = next {
                    end.set_successor(next_node.clone());
                    next_node.replace_predecessor(current_node.clone(), end.clone());
                    (next_node.clone(), end)
                } else {
                    return;
                }
            }
            ControlFlow::Simple(Some(prev), Stat::AssignStackWord(dst_ident), next_entry) => {
                // single destination, this node is removed from the graph and replaced with assignment

                let dst_t = dst_ident.get_temp();
                alloc_state.update_live(&liveout);
                let (reg, get_reg_chain) =
                    alloc_state.move_temp_into_reg(dst_t, false, &[], &liveout, graph);
                let set_address_chain = alloc_state.assign_stack_reserved(reg, graph);

                let Chain(mut start, mut end) =
                    link_optional_chains(vec![get_reg_chain, Some(set_address_chain)])
                        .expect("Must assign stack reserve");

                prev.replace_successor(current_node.clone(), start.clone());
                start.set_predecessor(prev.clone());

                if let Some(next_node) = next_entry {
                    next_node.replace_predecessor(current_node.clone(), end.clone());
                    end.set_successor(next_node.clone());
                    (next_node.clone(), end)
                } else {
                    return;
                }
            }
            ControlFlow::Simple(prev_entry, stat, next_entry) => {
                let chain_before: Option<Chain> = match stat {
                    Stat::MemOp(
                        MemOp::Ldr,
                        cond,
                        _,
                        dst_ident,
                        MemOperand::PreIndex(arg1_ident, FlexOffset::ShiftReg(_, arg2_ident, _)),
                    )
                    | Stat::Mul(cond, _, dst_ident, arg1_ident, arg2_ident)
                    | Stat::ApplyOp(
                        _,
                        cond,
                        _,
                        dst_ident,
                        arg1_ident,
                        FlexOperand::ShiftReg(arg2_ident, _),
                    )
                    | Stat::SatOp(_, cond, dst_ident, arg1_ident, arg2_ident) => {
                        // Arguments can be the same and match the destination.
                        let dst_t = dst_ident.get_temp();
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();

                        // ensure both arguments are in registers, and leave
                        // the arguments alone if they are already in a register.
                        let (arg1_reg, arg1_chain) =
                            alloc_state.move_temp_into_reg(arg1_t, true, &[arg2_t], &livein, graph);
                        let (arg2_reg, arg2_chain) =
                            alloc_state.move_temp_into_reg(arg2_t, true, &[arg1_t], &livein, graph);

                        // update the live temps, this means that if either argument
                        // dies after this statement, their register can be used as
                        // the destination register.
                        alloc_state.update_live(&liveout);

                        // Use any free register, if arg1 or arg2 are live after the instruction, we do not want to use their register.
                        let (dst_reg, dst_chain) = alloc_state.move_temp_into_reg(
                            dst_t,
                            load_dst(cond),
                            &[arg1_t, arg2_t],
                            &liveout,
                            graph,
                        );

                        // assign the register identifiers
                        *dst_ident = Ident::Reg(dst_reg);
                        *arg1_ident = Ident::Reg(arg1_reg);
                        *arg2_ident = Ident::Reg(arg2_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain, dst_chain])
                    }
                    Stat::MemOp(MemOp::Ldr, cond, _, dst_ident, MemOperand::Zero(arg_ident))
                    | Stat::Move(_, cond, _, dst_ident, FlexOperand::ShiftReg(arg_ident, _))
                    | Stat::MemOp(
                        MemOp::Ldr,
                        cond,
                        _,
                        dst_ident,
                        MemOperand::PreIndex(arg_ident, FlexOffset::Expr(_)),
                    )
                    | Stat::ApplyOp(_, cond, _, dst_ident, arg_ident, _) => {
                        // A destination and argument, these can be the same register.
                        let dst_t = dst_ident.get_temp();
                        let arg_t = arg_ident.get_temp();

                        // place the arg in a register
                        let (arg_reg, arg_chain) =
                            alloc_state.move_temp_into_reg(arg_t, true, &[], &livein, graph);

                        // update the live temps, this means that if the argument
                        // dies after this statement, its register can be used.
                        alloc_state.update_live(&liveout);

                        // find and place the dst in a register
                        let (dst_reg, dst_chain) = alloc_state.move_temp_into_reg(
                            dst_t,
                            load_dst(cond),
                            &[arg_t],
                            &liveout,
                            graph,
                        );

                        *arg_ident = Ident::Reg(arg_reg);
                        *dst_ident = Ident::Reg(dst_reg);

                        link_optional_chains(vec![arg_chain, dst_chain])
                    }
                    Stat::MulA(cond, _, dst_ident, arg1_ident, arg2_ident, arg3_ident) => {
                        // three arguments and one destination
                        let dst_t = dst_ident.get_temp();
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();
                        let arg3_t = arg3_ident.get_temp();

                        // ensure both arguments are in registers, and leave
                        // the arguments alone if they are already in a register.
                        let (arg1_reg, arg1_chain) = alloc_state.move_temp_into_reg(
                            arg1_t,
                            true,
                            &[arg2_t, arg3_t],
                            &livein,
                            graph,
                        );
                        let (arg2_reg, arg2_chain) = alloc_state.move_temp_into_reg(
                            arg2_t,
                            true,
                            &[arg1_t, arg3_t],
                            &livein,
                            graph,
                        );
                        let (arg3_reg, arg3_chain) = alloc_state.move_temp_into_reg(
                            arg3_t,
                            true,
                            &[arg1_t, arg2_t],
                            &livein,
                            graph,
                        );

                        // update with the liveout range
                        alloc_state.update_live(&liveout);

                        // find a destination, assign register
                        let (dst_reg, dst_chain) = alloc_state.move_temp_into_reg(
                            dst_t,
                            load_dst(cond),
                            &[arg1_t, arg2_t, arg3_t],
                            &liveout,
                            graph,
                        );

                        *dst_ident = Ident::Reg(dst_reg);
                        *arg1_ident = Ident::Reg(arg1_reg);
                        *arg2_ident = Ident::Reg(arg2_reg);
                        *arg3_ident = Ident::Reg(arg3_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain, arg3_chain, dst_chain])
                    }
                    Stat::MulOp(_, cond, _, dst1_ident, dst2_ident, arg1_ident, arg2_ident) => {
                        // two destinations and two arguments

                        let dst1_t = dst1_ident.get_temp();
                        let dst2_t = dst2_ident.get_temp();
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();

                        // assign the argument registers
                        let (arg1_reg, arg1_chain) =
                            alloc_state.move_temp_into_reg(arg1_t, true, &[arg2_t], &livein, graph);
                        let (arg2_reg, arg2_chain) =
                            alloc_state.move_temp_into_reg(arg2_t, true, &[arg1_t], &livein, graph);

                        // update with the liveout range
                        alloc_state.update_live(&liveout);

                        // assign the argument registers
                        let (dst1_reg, dst1_chain) = alloc_state.move_temp_into_reg(
                            dst1_t,
                            load_dst(cond),
                            &[arg1_t, arg2_t],
                            &liveout,
                            graph,
                        );
                        let (dst2_reg, dst2_chain) = alloc_state.move_temp_into_reg(
                            dst2_t,
                            load_dst(cond),
                            &[arg1_t, arg2_t, dst1_t],
                            &liveout,
                            graph,
                        );

                        *dst1_ident = Ident::Reg(dst1_reg);
                        *dst2_ident = Ident::Reg(dst2_reg);
                        *arg1_ident = Ident::Reg(arg1_reg);
                        *arg2_ident = Ident::Reg(arg2_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain, dst1_chain, dst2_chain])
                    }

                    Stat::Move(_, cond, _, dst_ident, _)
                    | Stat::MemOp(
                        MemOp::Ldr,
                        cond,
                        _,
                        dst_ident,
                        MemOperand::Label(_) | MemOperand::Expression(_),
                    ) => {
                        // single destination
                        let dst_t = dst_ident.get_temp();

                        // kill unused variables before determining destination
                        alloc_state.update_live(&liveout);

                        let (reg, chain) = alloc_state.move_temp_into_reg(
                            dst_t,
                            load_dst(cond),
                            &[],
                            &liveout,
                            graph,
                        );
                        *dst_ident = Ident::Reg(reg);
                        chain
                    }
                    Stat::ReadCPSR(dst_ident) => {
                        // single destination
                        let dst_t = dst_ident.get_temp();

                        // kill unused variables before determining destination
                        alloc_state.update_live(&liveout);

                        let (reg, chain) =
                            alloc_state.move_temp_into_reg(dst_t, false, &[], &liveout, graph);
                        *dst_ident = Ident::Reg(reg);
                        chain
                    }
                    Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg1_ident,
                        MemOperand::PreIndex(arg2_ident, FlexOffset::Expr(_)),
                    )
                    | Stat::MemOp(MemOp::Str, _, _, arg1_ident, MemOperand::Zero(arg2_ident))
                    | Stat::Cmp(_, _, arg1_ident, FlexOperand::ShiftReg(arg2_ident, _)) => {
                        // two arguments
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();

                        // assign the argument registers
                        let (arg1_reg, arg1_chain) =
                            alloc_state.move_temp_into_reg(arg1_t, true, &[arg2_t], &livein, graph);
                        let (arg2_reg, arg2_chain) =
                            alloc_state.move_temp_into_reg(arg2_t, true, &[arg1_t], &livein, graph);

                        alloc_state.update_live(&liveout);

                        *arg1_ident = Ident::Reg(arg1_reg);
                        *arg2_ident = Ident::Reg(arg2_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain])
                    }
                    Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg1_ident,
                        MemOperand::PreIndex(arg2_ident, FlexOffset::ShiftReg(_, arg3_ident, _)),
                    ) => {
                        // two arguments
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();
                        let arg3_t = arg3_ident.get_temp();

                        // assign the argument registers
                        let (arg1_reg, arg1_chain) = alloc_state.move_temp_into_reg(
                            arg1_t,
                            true,
                            &[arg2_t, arg3_t],
                            &livein,
                            graph,
                        );
                        let (arg2_reg, arg2_chain) = alloc_state.move_temp_into_reg(
                            arg2_t,
                            true,
                            &[arg1_t, arg3_t],
                            &livein,
                            graph,
                        );
                        let (arg3_reg, arg3_chain) = alloc_state.move_temp_into_reg(
                            arg3_t,
                            true,
                            &[arg1_t, arg2_t],
                            &livein,
                            graph,
                        );

                        *arg1_ident = Ident::Reg(arg1_reg);
                        *arg2_ident = Ident::Reg(arg2_reg);
                        *arg3_ident = Ident::Reg(arg3_reg);

                        alloc_state.update_live(&liveout);

                        link_optional_chains(vec![arg1_chain, arg2_chain, arg3_chain])
                    }
                    Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg_ident,
                        MemOperand::Label(_) | MemOperand::Expression(_),
                    )
                    | Stat::Cmp(_, _, arg_ident, FlexOperand::Imm(_)) => {
                        // single argument used (a store)
                        let arg_t = arg_ident.get_temp();

                        let (arg1_reg, arg_chain) =
                            alloc_state.move_temp_into_reg(arg_t, true, &[], &livein, graph);

                        alloc_state.update_live(&liveout);

                        *arg_ident = Ident::Reg(arg1_reg);

                        arg_chain
                    }

                    Stat::Push(_, arg_ident) => {
                        // push some temporary to the stack
                        let arg_t = arg_ident.get_temp();

                        let (arg1_reg, arg_chain) =
                            alloc_state.move_temp_into_reg(arg_t, true, &[], &livein, graph);

                        *arg_ident = Ident::Reg(arg1_reg);

                        alloc_state.update_live(&liveout);

                        alloc_state.push_sp();

                        arg_chain
                    }
                    Stat::Pop(cond, dst_ident) => {
                        // pop from the stack into some temporary
                        let dst_t = dst_ident.get_temp();
                        alloc_state.update_live(&liveout);
                        let (dst_reg, dst_chain) = alloc_state.move_temp_into_reg(
                            dst_t,
                            load_dst(cond),
                            &[],
                            &liveout,
                            graph,
                        );

                        *dst_ident = Ident::Reg(dst_reg);

                        alloc_state.pop_sp();

                        dst_chain
                    }
                    Stat::Link(_, _) => None,
                    _ => None, // No temporaries, so no chain
                };

                // if new chains were created, then make them the predecessors to this node.
                if let Some(Chain(mut before_start, mut before_end)) = chain_before {
                    let mut prev_node =
                        prev_entry.clone().expect("All nodes must have a successor");
                    // connect prev <-> before_start <-> before_end <-> this node
                    prev_node.replace_successor(current_node.clone(), before_start.clone());
                    before_start.set_predecessor(prev_node);

                    *prev_entry = Some(before_end.clone());
                    before_end.set_successor(current_node.clone())
                }

                if let Some(next_node) = next_entry {
                    (next_node.clone(), current_node)
                } else {
                    return;
                }
            }
            ControlFlow::Branch(_, branch_next, _, next) => {
                // check if branch next is already state set, and if so, conform.
                // if not, set a state for it.

                if let Some((conform_state, _)) = state_map.get(branch_next) {
                    // if there is some state to conform to, then alter from the current alloc state, to the new one, insert instructions.
                    if let Some(Chain(mut conform_start, mut conform_end)) =
                        alloc_state.clone().match_state(conform_state, graph)
                    {
                        branch_next.replace_predecessor(current_node.clone(), conform_end.clone());
                        conform_end.set_successor(branch_next.clone());

                        conform_start.set_predecessor(current_node.clone());
                        *branch_next = conform_start
                    }
                } else {
                    // No state yet exists, hence branch_next has the current state, and is not yet translated
                    state_map.insert(branch_next.clone(), (alloc_state.clone(), false));
                }

                if let Some(next_node) = next {
                    (next_node.clone(), current_node)
                } else {
                    // cap unconditional jumps with a literal pool
                    *next = Some(graph.new_node(ControlFlow::Ltorg(Some(current_node))));
                    return;
                }
            }
            ControlFlow::Ltorg(_) => return,
            ControlFlow::Return(Some(prev), ret) => {
                let mut start = alloc_state.subroutine_return(ret, graph);

                start.set_predecessor(prev.clone());
                prev.replace_successor(current_node, start);

                return;
            }
            ControlFlow::Multi(_, next) => {
                // this instruction has no temporaries, however we must keep
                // track that we have translated this (other nodes go here)
                state_map.insert(current_node.clone(), (alloc_state.clone(), true));

                // no other instructions are needed, so we simply move to the next node.
                if let Some(next_node) = next {
                    (next_node.clone(), current_node)
                } else {
                    return;
                }
            }
            ControlFlow::Removed => {
                panic!("Removed nodes must not be present in the traversable part of the graph")
            }
            _ => panic!("No previous node, hence the function's invariant is broken"),
        };

        // check if the next node has a state associated, and needs to be translated in another pass.
        if let Some((conform_alloc_state, _)) = state_map.get(&next) {
            if let Some(Chain(mut chain_start, mut chain_end)) =
                alloc_state.match_state(conform_alloc_state, graph)
            {
                next.replace_predecessor(new_end.clone(), chain_end.clone());
                chain_start.set_predecessor(new_end.clone());
                new_end.replace_successor(next.clone(), chain_start);
                chain_end.set_successor(next.clone());
            }

            return;
        }

        current_node = next;
    }
}

impl Ident {
    /// used to extract temporaries for temporary idents. Used when a mutable
    /// reference to the entire ident is required, so no immutable references to
    /// the temporary value inside the ident can be made.
    pub fn get_temp(&self) -> Temporary {
        if let Ident::Temp(t) = self {
            *t
        } else {
            panic!("cannot get temporary id from a non-temporary id")
        }
    }
}
