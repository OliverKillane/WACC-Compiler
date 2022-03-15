//! Optimising direct recursion into branching.
//!
//! Any statement of form:
//! ```text
//! function(param_1, ..., param_n)
//! ...
//! x = recursion(arg_1, ..., arg_n)
//! return x
//! ```
//! Can be optimised into
//! ```text
//! function(param_1, ..., param_n)
//!
//! param_1 = arg_1
//! ...
//! param_n = arg_n
//! jump to start of function
//! ```
//!
//! This can also be done for void functions.

use super::three_code::{Function, OpSrc, StatCode, StatNode, StatType, ThreeCode};
use crate::{graph::Graph, intermediate::VarRepr};
use lazy_static::__Deref;
use std::{
    collections::{HashMap, HashSet},
    ops::DerefMut,
};

pub(super) fn tail_call_optimise(mut threecode: ThreeCode) -> ThreeCode {
    threecode.functions = threecode
        .functions
        .into_iter()
        .map(|(name, function)| {
            let opt_function = tail_call_opt(&name, function);
            (name, opt_function)
        })
        .collect::<HashMap<_, _>>();
    threecode
}

/// Perform tail call optimisation on a given function.
fn tail_call_opt(
    fun_name: &str,
    Function {
        args,
        code,
        read_ref,
        mut graph,
    }: Function,
) -> Function {

    for node in get_return_nodes(code.clone()) {
        let (prevs, return_temp) = match node.get().deref() {
            StatType::Return(prevs, return_temp) => (prevs.clone(), *return_temp),
            _ => panic!("Was not a return node"),
        };

        for start_node in prevs {
            backtraverse(
                HashSet::from([node.clone()]),
                &node,
                start_node.clone(),
                return_temp,
                fun_name,
                &code,
                &args,
                &mut graph,
            )
        }
    }

    Function {
        args,
        code,
        read_ref,
        graph,
    }
}

/// Find all the return nodes in the function, returning node references in a vector.
fn get_return_nodes(start_node: StatNode) -> Vec<StatNode> {
    let mut return_nodes = vec![];
    find_return(start_node, &mut HashSet::new(), &mut return_nodes);
    return_nodes
}

/// A depth first traversal over a graph to find all return nodes.
fn find_return(
    mut current_node: StatNode,
    visited: &mut HashSet<StatNode>,
    returns: &mut Vec<StatNode>,
) {
    while visited.insert(current_node.clone()) {
        let next_node = match current_node.get().deref() {
            super::three_code::StatType::Simple(_, _, next) => next.clone(),
            super::three_code::StatType::Branch(_, _, branch_true, branch_false) => {
                find_return(branch_true.clone(), visited, returns);
                branch_false.clone()
            }
            super::three_code::StatType::Loop(_) => return,
            super::three_code::StatType::Return(_, _) => {
                returns.push(current_node.clone());
                return;
            }
            super::three_code::StatType::Dummy(_) => {
                panic!("No Dummy nodes should be in the final threecode representation.")
            }
        };

        current_node = next_node;
    }
}

/// Traverse back from a node, attempting to find recursive calls to link to the
/// start.
///
/// Continues to search recursively until at a visited node, a side-effect-ful
/// instruction (involves the return variables, is a call or a store).
///
/// Records the last instruction from which there are multiple predecessors
/// (from which we will remove one).
///
/// Converts the recursive call into a jump to the start node.
#[allow(clippy::too_many_arguments)]
fn backtraverse(
    mut visited: HashSet<StatNode>, // nodes previously visited
    prev_cut: &StatNode,            // the node to cut from the prev-node, if jumping
    start_node: StatNode,           // the node to traverse back from
    mut return_temp: Option<VarRepr>,
    fun_name: &str, // name of the function - to check for recursion
    fun_start: &StatNode,
    fun_params: &[VarRepr],
    graph: &mut Graph<StatType>,
) {
    // set the current node
    let mut current_node = start_node.clone();

    // determine if in visited
    while visited.insert(current_node.clone()) {
        let prevs = match current_node.get().deref() {
            StatType::Dummy(_) => {
                panic!("Dummy has no previous node, hence cannot traverse back to one")
            }
            StatType::Loop(_) => {
                panic!("Loop has no previous node, hence cannot traverse back to one")
            }
            StatType::Return(_, _) => {
                panic!("Return has no previous node, hence cannot traverse back to one")
            }
            StatType::Branch(_, _, _, _) => return, // cannot determine if call will be used past a branch
            StatType::Simple(prevs, stat, _) => {
                // can now check if the statement is sideeffectful, or a call
                match stat {
                    StatCode::Store(_, _, _) => return, // stores are side effectual (memory) so cannot safely continue
                    StatCode::Assign(dst, OpSrc::Var(other_dst)) => {
                        // if the destination is the return we are tracking, then we can switch to another variable.
                        if let Some(ret_t) = return_temp && dst == &ret_t {
                            return_temp = Some(*other_dst);
                        }
                    }
                    StatCode::Assign(dst, _)
                    | StatCode::AssignOp(dst, _, _, _)
                    | StatCode::Load(dst, _, _) => {
                        // if we have a return, then we must check it is not changed.
                        if let Some(ret_t) = &return_temp && dst == ret_t {
                            return
                        }
                    }
                    StatCode::Call(ret, name, args) => {
                        if let Some(ret_t) = &return_temp && ret == ret_t && name == fun_name {
                            // if a recursive call, assigning to the variable, then we can replace it.
                            substitute_call(args, fun_params, fun_start.clone(), current_node.clone(), prevs, graph);
                            prev_cut.get_mut().deref_mut().remove_incoming(&start_node)
                        }
                        return;
                    }
                    StatCode::VoidCall(name, args) => {
                        if return_temp.is_none() && name == fun_name {
                            // if a recursive call, assigning to the variable, then we can replace it.
                            substitute_call(
                                args,
                                fun_params,
                                fun_start.clone(),
                                current_node.clone(),
                                prevs,
                                graph,
                            );
                            prev_cut.get_mut().deref_mut().remove_incoming(&start_node)
                        }
                        return;
                    }
                }
                prevs.clone()
            }
        };

        // if there is a single predecessor, we can continue our loop,
        // if there is more than one, we have reached a new point to
        // cut from. Otherwise we cannot check any further.
        current_node = if prevs.is_empty() {
            return;
        } else if prevs.len() == 1 {
            prevs[0].clone()
        } else {
            for prev_node in prevs {
                backtraverse(
                    visited.clone(),
                    &current_node,
                    prev_node.clone(),
                    return_temp,
                    fun_name,
                    fun_start,
                    fun_params,
                    graph,
                )
            }
            return;
        }
    }
}

/// Given the arguments, and parameters, assign, then link to the start node
/// (to_node) for the function.
/// ```text
/// (to_node) and [(Param_1, Arg_1), ..., (Param_n, Arg_n)]
/// (assign Param_n = Arg_n) <-> ... <-> (assign Param_1 = Arg_1) <-> to_node
/// ```
fn generate_call_jump(
    param_args: Vec<(&VarRepr, &VarRepr)>,
    mut to_node: StatNode,
    graph: &mut Graph<StatType>,
) -> StatNode {
    for (param, arg) in param_args {
        if param != arg {
            let next_node = graph.new_node(StatType::new_simple(
                StatCode::Assign(*param, OpSrc::Var(*arg)),
                to_node.clone(),
            ));
            to_node
                .get_mut()
                .deref_mut()
                .add_incoming(next_node.clone());
            to_node = next_node;
        }
    }
    to_node
}

/// Use args, parameters and link back to start node, replacing predecessor
/// node references accordingly.
fn substitute_call(
    args: &[VarRepr],
    fun_params: &[VarRepr],
    fun_start: StatNode,
    replace_node: StatNode,
    prevs: &[StatNode],
    graph: &mut Graph<StatType>,
) {
    let loop_node = generate_call_jump(
        fun_params.iter().zip(args.iter()).collect::<Vec<_>>(),
        fun_start,
        graph,
    );

    for node in prevs {
        node.get_mut()
            .deref_mut()
            .substitute_child(&replace_node, &loop_node);
        loop_node.get_mut().deref_mut().add_incoming(node.clone());
    }
}
