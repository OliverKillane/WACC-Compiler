use rayon::iter::{IntoParallelIterator, ParallelIterator};

use super::three_code::*;
use crate::{
    backend::data_flow::DataflowNode,
    graph::{Deleted, Graph},
};
use std::collections::LinkedList;

/// Performs the same branch reduction on a single graph.
fn same_branch_opt_graph(graph: &mut Graph<StatType>, code: &mut StatNode) {
    let mut needless_branches = LinkedList::new();
    for node in &*graph {
        if let StatType::Branch(_, _, true_node, false_node) = &*node.get() && true_node == false_node {
            needless_branches.push_back(node.clone());
        }
    }
    while let Some(node) = needless_branches.pop_front() {
        let (incoming, next_node) = if let StatType::Branch(incoming, _, true_node, false_node) =
            node.set(StatType::deleted())
        {
            assert_eq!(true_node, false_node);
            (incoming, true_node)
        } else {
            panic!("Expected a branch node")
        };
        if *code == node {
            *code = next_node.clone()
        }
        if next_node == node {
            node.set(StatType::Loop(incoming));
            continue;
        }
        for incoming_node in &incoming {
            incoming_node.get_mut().substitute_child(&node, &next_node);
            next_node.get_mut().add_incoming(incoming_node.clone());
            if let StatType::Branch(_, _, true_node, false_node) = &*incoming_node.get() && true_node == false_node {
                needless_branches.push_back(node.clone());
            }
        }
        graph.remove_node(node);
    }
    for node in &*graph {
        let incoming = (&*node.get())
            .incoming()
            .into_iter()
            .filter(|&incoming_node| !matches!(&*incoming_node.get(), StatType::Dummy(_)))
            .cloned()
            .collect();
        (&mut *node.get_mut()).set_incoming(incoming);
    }
    assert!(needless_branches.is_empty());
}

/// Applies an optimization so that nodes pointing with both branches to the same
/// successor node get removed.
pub(super) fn same_branch_optimization(mut three_code: ThreeCode) -> ThreeCode {
    same_branch_opt_graph(&mut three_code.graph, &mut three_code.code);
    three_code.functions = three_code
        .functions
        .into_par_iter()
        .map(|(fname, mut function)| {
            same_branch_opt_graph(&mut function.graph, &mut function.code);
            (fname, function)
        })
        .collect();
    three_code
}
