use super::{StatNode, StatType};
use std::collections::{HashMap, HashSet};

/// Sorts the nodes quasi-topologically (pre-order if forward_traversal and post-order otherwise).
fn topo_sort(
    sorted: &mut Vec<StatNode>,
    node: &StatNode,
    visited: &mut HashSet<StatNode>,
    forward_traversal: bool,
) {
    if visited.contains(node) {
        return;
    }
    if forward_traversal {
        sorted.push(node.clone());
    }
    match &*node.get() {
        StatType::Simple(_, _, next_node) => {
            topo_sort(sorted, next_node, visited, forward_traversal)
        }
        StatType::Branch(_, _, true_node, false_node) => {
            topo_sort(sorted, true_node, visited, forward_traversal);
            topo_sort(sorted, false_node, visited, forward_traversal);
        }
        _ => {}
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
pub(in super::super) fn dataflow_analysis<SIn: Eq, SOut: Eq, Init, Update>(
    root: StatNode,
    mut initialize: Init,
    mut update: Update,
    forward_traversal: bool,
) -> HashMap<StatNode, (SIn, SOut)>
where
    Init: FnMut(&StatNode) -> (SIn, SOut),
    Update: FnMut(Vec<&SOut>, SIn, &StatNode, SOut, Vec<&SIn>) -> (SIn, SOut, bool),
{
    let mut topo_sorted = Vec::new();
    topo_sort(
        &mut topo_sorted,
        &root,
        &mut HashSet::new(),
        forward_traversal,
    );
    let mut analysis_map = topo_sorted
        .iter()
        .map(|node| (node.clone(), initialize(node)))
        .collect::<HashMap<_, _>>();
    loop {
        let mut updated = false;
        for node in &topo_sorted {
            let (set_in, set_out) = analysis_map.remove(node).unwrap();
            let incoming_outs = node
                .get()
                .incoming()
                .iter()
                .map(|incoming_node| &analysis_map[incoming_node].1)
                .collect();
            let successors_ins = node
                .get()
                .successors()
                .iter()
                .map(|successor_node| &analysis_map[successor_node].0)
                .collect();
            let (set_in, set_out, updated_node) =
                update(incoming_outs, set_in, node, set_out, successors_ins);
            analysis_map.insert(node.clone(), (set_in, set_out));
            updated |= updated_node;
        }
        if !updated {
            break;
        }
    }
    analysis_map
}
