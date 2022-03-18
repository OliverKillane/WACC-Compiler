//! Removal of dead code (no side effects, is not part of routine).
//!
//! Any code which does not affect memory, control flow, or is a call/return and
//! that does not create values used in any such side-effectual operations is
//! considered dead and can be removed.
//!
//! Function inlining greatly helps this optimisation as dead code removal
//! cannot yet peer into and remove function calls, even if the result/return value
//! is not side-effectual.

use std::{
    collections::{HashMap, HashSet},
    ops::DerefMut,
    rc::Rc,
};

use super::{
    data_flow::dataflow_analysis,
    three_code::{OpSrc, StatCode, StatNode, StatType, ThreeCode},
};
use crate::intermediate::VarRepr;
use lazy_static::__Deref;
use rayon::prelude::*;

/// Remove all dead code from a threecode program
pub(super) fn remove_dead_code(mut threecode: ThreeCode) -> ThreeCode {
    threecode.functions = threecode
        .functions
        .into_par_iter()
        .map(|(name, mut function)| {
            function.code = remove_dead_code_from(function.code);
            (name, function)
        })
        .collect::<HashMap<_, _>>();
    threecode.code = remove_dead_code_from(threecode.code);
    threecode
}

/// Remove all dead code from a given function.
fn remove_dead_code_from(start_node: StatNode) -> StatNode {
    remove_dead_nodes(
        get_dead_nodes(dataflow_analysis(
            &start_node,
            initialise_node,
            update_node,
            false,
        )),
        start_node,
    )
}

/// From the data flow analysis, determine the dead nodes that should be removed.
fn get_dead_nodes(code_analysis: HashMap<StatNode, (CodeAnalysis, CodeAnalysis)>) -> Vec<StatNode> {
    let mut dead_nodes = vec![];

    for (node, (_, out_set)) in code_analysis {
        let (defs, _, sideeeffects) = get_node_effects(&node);

        if !sideeeffects {
            if let Some(def_var) = defs && out_set.contains(&def_var) {

            } else {
               dead_nodes.push(node)
            }
        }
    }

    dead_nodes
}

/// remove the dead nodes from the graph, ensuring all nodes are linked correctly,
/// and altering the start node if necessary.
fn remove_dead_nodes(dead_nodes: Vec<StatNode>, mut start_node: StatNode) -> StatNode {
    for dead_node in dead_nodes {
        match dead_node.get().deref() {
            StatType::Simple(_, StatCode::Call(_, _, _) | StatCode::VoidCall(_, _), _) => {
                panic!("Calls are side-effectual, cannot be dead.")
            }
            StatType::Branch(_, _, _, _) => panic!("Branches are side-effectual, cannot be dead."),
            StatType::Loop(_) => panic!("Loops are side-effectual, cannot be dead."),
            StatType::Return(_, _) => panic!("Returns are side-effectual, cannot be dead."),
            StatType::Dummy(_) => {
                panic!("No dummy nodes can be in the graph at dead code analysis")
            }
            StatType::Simple(prevs, _, succ) => {
                if dead_node == start_node {
                    // remove the predecessor reference to the start_node, and set the
                    // start_node as the successor.
                    //
                    // D* <-> S
                    //        S*

                    succ.get_mut().deref_mut().remove_incoming(&dead_node);
                    start_node = succ.clone()
                } else {
                    // 1. substitute previous node successors with successor node.
                    // 2. Remove dead_node form successor's predecessors.
                    // 3. Add dead_node's predecessors to successor's predecessors.

                    for prev_node in prevs {
                        prev_node
                            .get_mut()
                            .deref_mut()
                            .substitute_child(&dead_node, succ);
                        succ.get_mut().deref_mut().add_incoming(prev_node.clone());
                    }

                    succ.get_mut().deref_mut().remove_incoming(&dead_node);
                }
            }
        }
    }

    start_node
}

/// From a [threecode node](StatNode) get the defined variable, variables used,
/// and if the statement is directly sideeffectful.
///
/// Side-effectual nodes are:
/// - Store  (changes memory)
/// - Call   (may result in stores, interaction with I/O)
/// - Return (Returns a value, function must still return)
/// - Branch (Affects control flow of the function)
///
/// For the threecode representation, there can only be one definition per
/// statement, though there can be many uses.
fn get_node_effects(node: &StatNode) -> (Option<VarRepr>, Vec<VarRepr>, bool) {
    match node.get().deref() {
        StatType::Simple(_, stat, _) => {
            let mut uses = vec![];

            let mut add_use = |src: &OpSrc| {
                if let OpSrc::Var(t) = src {
                    uses.push(*t)
                }
            };

            let (dst, sideeffect) = match stat {
                StatCode::Assign(dst, arg) | StatCode::Load(dst, arg, _) => {
                    add_use(arg);
                    (Some(*dst), false)
                }
                StatCode::Store(arg_1, arg_2, _) => {
                    add_use(arg_1);
                    uses.push(*arg_2);
                    (None, true)
                }
                StatCode::AssignOp(dst, arg_1, _, arg_2, _) => {
                    add_use(arg_1);
                    add_use(arg_2);
                    (Some(*dst), false)
                }
                StatCode::Call(dst, _, args) => {
                    uses.extend_from_slice(args);
                    (Some(*dst), true)
                }
                StatCode::VoidCall(_, args) => {
                    uses.extend_from_slice(args);
                    (None, true)
                }
            };

            (dst, uses, sideeffect)
        }
        StatType::Return(_, Some(src)) | StatType::Branch(_, src, _, _) => (
            None,
            if let OpSrc::Var(t) = src {
                vec![*t]
            } else {
                vec![]
            },
            true,
        ),
        StatType::Return(_, None) | StatType::Loop(_) => (None, vec![], true),
        StatType::Dummy(_) => panic!("No dummy nodes should be present at dead code removal"),
    }
}

type CodeAnalysis = Rc<HashSet<VarRepr>>;

/// initialise a set, if the statement is has side-effects then its uses are
/// propagated to its live-in set.
fn initialise_node(node: &StatNode) -> (CodeAnalysis, CodeAnalysis) {
    let (_, uses, sideeffect) = get_node_effects(node);

    (
        if sideeffect {
            Rc::new(HashSet::from_iter(uses.into_iter()))
        } else {
            Rc::new(HashSet::new())
        },
        Rc::new(HashSet::new()),
    )
}

/// Using the current node, as well as its live-in and live-out temporaries,
/// propagate required/behaviour changing temporaries.
fn update_node(
    _: Vec<&CodeAnalysis>,
    in_set: &CodeAnalysis,
    node: &StatNode,
    out_set: &CodeAnalysis,
    succ_live_ins: Vec<&CodeAnalysis>,
) -> (CodeAnalysis, CodeAnalysis, bool) {
    let (def, uses, sideeffect) = get_node_effects(node);

    // The new out set is composed of the union of all the in-sets of the node's successors.
    let new_out_set = if succ_live_ins.len() == 1 {
        // if only one node, we can share
        succ_live_ins[0].clone()
    } else {
        // iterate through all successor's live ins, add each to new set.
        Rc::new(
            succ_live_ins
                .into_iter()
                .fold(HashSet::new(), |mut liveout, livein| {
                    livein.iter().for_each(|var| {
                        liveout.insert(*var);
                    });
                    liveout
                }),
        )
    };

    // get the side-effecting variables before the node
    let mut side_effect_in = if sideeffect {
        HashSet::from_iter(uses.clone().into_iter())
    } else {
        HashSet::new()
    };

    for var in new_out_set.iter() {
        // if the definition was side effectful, then the uses that produce that
        // definition are also.
        if let Some(def_var) = def && &def_var == var {
            for used in uses.iter() {
                side_effect_in.insert(*used);
            }
        } else {
            side_effect_in.insert(*var);
        }
    }

    let new_in_set = Rc::new(side_effect_in);

    // check for a change in set contents.
    let updated = in_set != &new_in_set || out_set != &new_out_set;
    (new_in_set, new_out_set, updated)
}
