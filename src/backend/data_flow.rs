//! Data flow analysis for use in live range, dead code and constant propagation.
//!
//! This module abstracts dataflow analysis to work over any types of in/out sets,
//! initializers and update functions, on any graph node.

use crate::graph::{Deleted, NodeRef};
use std::collections::{HashMap, HashSet};

pub(super) trait DataflowNode: Deleted + Sized {
    fn incoming(&self) -> Vec<&NodeRef<Self>>;
    fn outgoing(&self) -> Vec<&NodeRef<Self>>;
}

/// Sorts the nodes quasi-topologically (pre-order if forward_traversal and post-order otherwise).
fn topo_sort<Node: Deleted + DataflowNode>(
    sorted: &mut Vec<NodeRef<Node>>,
    node: &NodeRef<Node>,
    visited: &mut HashSet<NodeRef<Node>>,
    forward_traversal: bool,
) {
    if visited.contains(node) {
        return;
    }
    visited.insert(node.clone());
    if forward_traversal {
        sorted.push(node.clone());
    }
    for next_node in (&*node.get()).outgoing() {
        topo_sort(sorted, next_node, visited, forward_traversal);
    }
    if !forward_traversal {
        sorted.push(node.clone());
    }
}

/// Performs the dataflow analysis on the code. The arguments are as follows:
///  - The initial node of the code.
///  - Initialization function for initialization of the in and out sets of
///    the node.
///  - Update function for updating the in and out sets based on the out sets
///    of node's predecessors, the in set of the node, the node itself,
///    the out set of the node and the in sets of the successor nodes.
pub(super) fn dataflow_analysis<SIn: Eq, SOut: Eq, Node: Deleted + DataflowNode, Init, Update>(
    root: &NodeRef<Node>,
    mut initialize: Init,
    mut update: Update,
    forward_traversal: bool,
) -> HashMap<NodeRef<Node>, (SIn, SOut)>
where
    Init: FnMut(&NodeRef<Node>) -> (SIn, SOut),
    Update: FnMut(Vec<&SOut>, &SIn, &NodeRef<Node>, &SOut, Vec<&SIn>) -> (SIn, SOut, bool),
{
    let mut topo_sorted = Vec::new();
    topo_sort(
        &mut topo_sorted,
        root,
        &mut HashSet::new(),
        forward_traversal,
    );
    let mut analysis_map = topo_sorted
        .iter()
        .map(|node| (node.clone(), initialize(node)))
        .collect::<HashMap<_, _>>();
    #[cfg(debug_assertions)]
    for node in &topo_sorted {
        for incoming_node in (&*node.get()).incoming() {
            if !analysis_map.contains_key(incoming_node) {
                panic!("Root node is not a root or the incoming nodes are ill-defined")
            }
        }
    }
    loop {
        let mut updated = false;
        for node in &topo_sorted {
            let (set_in, set_out) = &analysis_map[node];
            let incoming_outs = (&*node.get())
                .incoming()
                .iter()
                .map(|incoming_node| &analysis_map[incoming_node].1)
                .collect();
            let outgoing_ins = (&*node.get())
                .outgoing()
                .iter()
                .map(|outgoing_node| &analysis_map[outgoing_node].0)
                .collect();
            let (set_in, set_out, updated_node) =
                update(incoming_outs, set_in, node, set_out, outgoing_ins);
            analysis_map.insert(node.clone(), (set_in, set_out));
            updated |= updated_node;
        }
        if !updated {
            break;
        }
    }
    analysis_map
}
