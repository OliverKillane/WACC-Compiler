//! Register allocation for the arm representation, using live ranges and
//! 'time-till-use' decision for spilling.
//!
//! An allocation state is used to keep track of register usage, and
//! create/maintain the stack frame.

use std::{
    collections::{HashMap, HashSet},
    ops::DerefMut,
};

use crate::graph::Graph;

use super::{
    allocation_state::AllocationState,
    arm_graph_utils::{link_optional_chains, Chain},
    arm_repr::{
        ArmCode, ArmNode, ControlFlow, FlexOffset, FlexOperand, Ident, MemOp, MemOperand, Stat,
        Subroutine, Temporary,
    },
    live_ranges::{get_live_ranges, LiveRanges},
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
                    },
                )| {
                    (
                        name,
                        Subroutine {
                            args,
                            start_node: allocate_for_routine(
                                start_node,
                                &[],
                                reserved_stack,
                                &temps,
                                &live_ranges,
                                &mut cfg,
                            ),
                            temps,
                            reserved_stack,
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
    mut start_node: ArmNode,
    args: &[Temporary],
    reserved_stack: u8,
    temps_set: &HashSet<Temporary>,
    live_ranges: &LiveRanges,
    graph: &mut Graph<ControlFlow>,
) -> ArmNode {
    let mut state_map = HashMap::new();
    let (alloc_state, Chain(new_start, mut stack_setup_end)) =
        AllocationState::create_stack_frame(args, reserved_stack, temps_set, graph);

    // connect up the start:
    start_node.set_predecessor(stack_setup_end.clone());
    stack_setup_end.set_successor(start_node.clone());

    translate_from_node(
        start_node.clone(),
        alloc_state,
        &mut state_map,
        live_ranges,
        graph,
    );

    new_start
}

/// Starting from a given node, translate all temporaries to registers.
/// - Traverses from 'node', this must not have already been translated.
/// - state_map contains labels where a state already exists, and must be conformed to.
/// - alloc_state holds the current state of the stack and registers
/// - graph contains all arm nodes.
/// - every node must have a predecessor.
fn translate_from_node(
    mut current_node: ArmNode,
    mut alloc_state: AllocationState,
    state_map: &mut HashMap<ArmNode, AllocationState>,
    live_ranges: &LiveRanges,
    graph: &mut Graph<ControlFlow>,
) {
    loop {
        // get the live in and live out
        let livein = live_ranges.get_livein(&current_node.clone());
        let liveout = live_ranges.get_liveout(&current_node.clone());

        // update the registers for current live_in
        alloc_state.update_live(&livein);

        let mut next: ArmNode = match current_node.clone().get_mut().deref_mut() {
            ControlFlow::Simple(Some(prev), Stat::Call(fun_name, ret, args), next) => {
                let Chain(mut start, mut end) =
                    alloc_state.call(fun_name.clone(), ret, args, &liveout, graph);

                // as we entirely replace the 'call' node, we set the successors and
                // predecessors to point to the ends of our chain of statements
                if let Some(next_node) = next {
                    end.set_successor(next_node.clone());
                    next_node.replace_predecessor(current_node.clone(), end)
                }

                start.set_predecessor(prev.clone());
                prev.replace_successor(current_node.clone(), start);

                if let Some(next_node) = next {
                    next_node.clone()
                } else {
                    return;
                }
            }
            ControlFlow::Simple(prev_entry, stat, next_entry) => {
                let chain_before: Option<Chain> = match stat {
                    Stat::MemOp(
                        MemOp::Ldr,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        MemOperand::PreIndex(
                            arg1_ident @ Ident::Temp(_),
                            FlexOffset::ShiftReg(_, arg2_ident @ Ident::Temp(_), _),
                        ),
                    )
                    | Stat::Mul(
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        arg1_ident @ Ident::Temp(_),
                        arg2_ident @ Ident::Temp(_),
                    )
                    | Stat::ApplyOp(
                        _,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        arg1_ident @ Ident::Temp(_),
                        FlexOperand::ShiftReg(arg2_ident @ Ident::Temp(_), _),
                    )
                    | Stat::SatOp(
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        arg1_ident @ Ident::Temp(_),
                        arg2_ident @ Ident::Temp(_),
                    ) => {
                        // Arguments can be the same and match the destination.

                        let dst_t = dst_ident.get_temp();
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg1_ident.get_temp();

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
                            false,
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
                    Stat::MemOp(
                        MemOp::Ldr,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        MemOperand::Zero(arg_ident @ Ident::Temp(_)),
                    )
                    | Stat::MemOp(
                        MemOp::Ldr,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        MemOperand::PreIndex(arg_ident @ Ident::Temp(_), FlexOffset::Expr(_)),
                    )
                    | Stat::Move(
                        _,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        FlexOperand::ShiftReg(arg_ident @ Ident::Temp(_), _),
                    )
                    | Stat::Cmp(
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        FlexOperand::ShiftReg(arg_ident @ Ident::Temp(_), _),
                    )
                    | Stat::ApplyOp(
                        _,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        arg_ident @ Ident::Temp(_),
                        _,
                    ) => {
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
                        let (dst_reg, dst_chain) =
                            alloc_state.move_temp_into_reg(dst_t, false, &[arg_t], &liveout, graph);

                        *arg_ident = Ident::Reg(arg_reg);
                        *dst_ident = Ident::Reg(dst_reg);

                        link_optional_chains(vec![arg_chain, dst_chain])
                    }
                    Stat::MulA(
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        arg1_ident @ Ident::Temp(_),
                        arg2_ident @ Ident::Temp(_),
                        arg3_ident @ Ident::Temp(_),
                    ) => {
                        // three arguments and one destination
                        let dst_t = dst_ident.get_temp();
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();
                        let arg3_t = arg2_ident.get_temp();

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
                            &[arg3_t, arg2_t],
                            &livein,
                            graph,
                        );

                        // update with the liveout range
                        alloc_state.update_live(&liveout);

                        // find a destination, assign register
                        let (dst_reg, dst_chain) = alloc_state.move_temp_into_reg(
                            dst_t,
                            false,
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
                    Stat::MulOp(
                        _,
                        _,
                        _,
                        dst1_ident @ Ident::Temp(_),
                        dst2_ident @ Ident::Temp(_),
                        arg1_ident @ Ident::Temp(_),
                        arg2_ident @ Ident::Temp(_),
                    ) => {
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
                            false,
                            &[arg1_t, arg2_t],
                            &livein,
                            graph,
                        );
                        let (dst2_reg, dst2_chain) = alloc_state.move_temp_into_reg(
                            dst2_t,
                            false,
                            &[arg1_t, arg2_t, dst1_t],
                            &livein,
                            graph,
                        );

                        *dst1_ident = Ident::Reg(dst1_reg);
                        *dst2_ident = Ident::Reg(dst2_reg);
                        *arg1_ident = Ident::Reg(arg1_reg);
                        *arg2_ident = Ident::Reg(arg2_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain, dst1_chain, dst2_chain])
                    }

                    Stat::Move(_, _, _, dst_ident @ Ident::Temp(_), _)
                    | Stat::Cmp(_, _, dst_ident @ Ident::Temp(_), _)
                    | Stat::MemOp(
                        MemOp::Ldr,
                        _,
                        _,
                        dst_ident @ Ident::Temp(_),
                        MemOperand::Label(_) | MemOperand::Expression(_),
                    )
                    | Stat::ReadCPSR(dst_ident @ Ident::Temp(_)) => {
                        // single destination
                        let dst_t = dst_ident.get_temp();

                        // kill unused variables before determining destination
                        alloc_state.update_live(&liveout);

                        let (reg, chain) =
                            alloc_state.move_temp_into_reg(dst_t, false, &[], &[], graph);
                        *dst_ident = Ident::Reg(reg);
                        chain
                    }
                    Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg1_ident @ Ident::Temp(_),
                        MemOperand::PreIndex(arg2_ident @ Ident::Temp(_), FlexOffset::Expr(_)),
                    )
                    | Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg1_ident @ Ident::Temp(_),
                        MemOperand::Zero(arg2_ident @ Ident::Temp(_)),
                    ) => {
                        // two arguments
                        let arg1_t = arg1_ident.get_temp();
                        let arg2_t = arg2_ident.get_temp();

                        // assign the argument registers
                        let (arg1_reg, arg1_chain) =
                            alloc_state.move_temp_into_reg(arg1_t, true, &[arg2_t], &livein, graph);
                        let (arg2_reg, arg2_chain) =
                            alloc_state.move_temp_into_reg(arg2_t, true, &[arg1_t], &livein, graph);

                        *arg1_ident = Ident::Reg(arg1_reg);
                        *arg2_ident = Ident::Reg(arg2_reg);

                        link_optional_chains(vec![arg1_chain, arg2_chain])
                    }
                    Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg1_ident @ Ident::Temp(_),
                        MemOperand::PreIndex(
                            arg2_ident @ Ident::Temp(_),
                            FlexOffset::ShiftReg(_, arg3_ident @ Ident::Temp(_), _),
                        ),
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

                        link_optional_chains(vec![arg1_chain, arg2_chain, arg3_chain])
                    }
                    Stat::MemOp(
                        MemOp::Str,
                        _,
                        _,
                        arg_ident @ Ident::Temp(_),
                        MemOperand::Label(_) | MemOperand::Expression(_),
                    ) => {
                        // single argument used (a store)
                        let arg_t = arg_ident.get_temp();

                        let (arg1_reg, arg_chain) =
                            alloc_state.move_temp_into_reg(arg_t, true, &[], &livein, graph);

                        *arg_ident = Ident::Reg(arg1_reg);

                        arg_chain
                    }

                    Stat::Push(_, arg_ident @ Ident::Temp(_)) => {
                        // push to the stack
                        let arg_t = arg_ident.get_temp();
                        let (arg1_reg, arg_chain) =
                            alloc_state.move_temp_into_reg(arg_t, true, &[], &livein, graph);

                        *arg_ident = Ident::Reg(arg1_reg);

                        alloc_state.push_sp();

                        arg_chain
                    }
                    Stat::Pop(_, arg_ident @ Ident::Temp(_)) => {
                        // push to the stack
                        let arg_t = arg_ident.get_temp();
                        let (arg1_reg, arg_chain) =
                            alloc_state.move_temp_into_reg(arg_t, true, &[], &livein, graph);

                        *arg_ident = Ident::Reg(arg1_reg);

                        alloc_state.pop_sp();

                        arg_chain
                    }
                    Stat::Link(_, _) => None,
                    Stat::AssignStackWord(dst_ident @ Ident::Temp(_)) => {
                        // single destination
                        let dst_t = dst_ident.get_temp();

                        let (reg, get_reg_chain) =
                            alloc_state.move_temp_into_reg(dst_t, false, &[], &[], graph);
                        let set_address_chain = alloc_state.assign_stack_reserved(reg, graph);

                        *dst_ident = Ident::Reg(reg);

                        link_optional_chains(vec![get_reg_chain, Some(set_address_chain)])
                    }
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
                    next_node.clone()
                } else {
                    return;
                }
            }
            ControlFlow::Branch(_, branch_next, _, next) => {
                // check if the current node has had its registers allocated already
                if let Some(conform_alloc_state) = state_map.get(branch_next) {
                    if let Some(Chain(mut chain_start, mut chain_end)) =
                        alloc_state.match_state(conform_alloc_state, graph)
                    {
                        let mut next_node = branch_next.clone();

                        chain_start.set_predecessor(current_node.clone());
                        *branch_next = chain_start;

                        chain_end.set_successor(branch_next.clone());
                        next_node.replace_predecessor(current_node, chain_end)
                    }

                    return;
                }

                translate_from_node(
                    branch_next.clone(),
                    alloc_state.clone(),
                    state_map,
                    live_ranges,
                    graph,
                );

                if let Some(next_node) = next {
                    next_node.clone()
                } else {
                    // cap unconditional jumps with a literal pool
                    *next = Some(graph.new_node(ControlFlow::Ltorg(Some(current_node))));
                    return;
                }
            }
            ControlFlow::Ltorg(_) => return,
            ControlFlow::Return(Some(prev), ret) => {
                let start = alloc_state.subroutine_return(ret, graph);

                prev.replace_successor(current_node.clone(), start);

                return;
            }
            ControlFlow::Multi(_, next) => {
                // this instruction has no temporaries, however we must keep
                // track that we have translated this (other nodes go here)
                state_map.insert(current_node.clone(), alloc_state.clone());

                // no other instructions are needed, so we simpy move to the next node.
                if let Some(next_node) = next {
                    next_node.clone()
                } else {
                    return;
                }
            }
            ControlFlow::Removed => {
                panic!("Removed nodes must not be present in the traversable part of the graph")
            }
            _ => panic!("No previous node, hence the function's invaliant is broken"),
        };

        // check if the current node has had its registers allocated already
        if let Some(conform_alloc_state) = state_map.get(&next) {
            if let Some(Chain(mut chain_start, mut chain_end)) =
                alloc_state.match_state(conform_alloc_state, graph)
            {
                chain_start.set_predecessor(current_node.clone());
                current_node.replace_successor(next.clone(), chain_start);

                chain_end.set_successor(next.clone());
                next.replace_predecessor(current_node, chain_end);
            }

            return;
        }

        current_node = next;
    }
}

impl Ident {
    /// used to extract temporaries
    pub fn get_temp(&self) -> Temporary {
        if let Ident::Temp(t) = self {
            *t
        } else {
            panic!("cannot get temporary id from a non-temporary id")
        }
    }
}
