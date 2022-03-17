use rayon::iter::{IntoParallelIterator, ParallelIterator};

use super::{data_flow::DataflowNode, three_code::*};
use crate::graph::{Deleted, Graph};
use std::collections::{HashSet, LinkedList};

fn live_node_dfs(node: &StatNode, visited: &mut HashSet<StatNode>) {
    if visited.contains(node) {
        return;
    }
    visited.insert(node.clone());
    for outgoing_node in (&*node.get()).outgoing() {
        live_node_dfs(outgoing_node, visited);
    }
}

fn const_branch_opt_graph(graph: &mut Graph<StatType>, code: &mut StatNode) {
    let needless_branches = graph
        .iter()
        .filter(|node| matches!(&*node.get(), StatType::Branch(_, OpSrc::Const(_), _, _)))
        .collect::<LinkedList<_>>();
    for node in needless_branches {
        let (incoming, next_node) = match node.set(StatType::deleted()) {
            StatType::Branch(incoming, OpSrc::Const(1), true_node, _) => (incoming, true_node),
            StatType::Branch(incoming, OpSrc::Const(_), _, false_node) => (incoming, false_node),
            _ => panic!("Not a needless branch"),
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
        }
        graph.remove_node(node);
    }
    let mut live_nodes = HashSet::new();
    live_node_dfs(code, &mut live_nodes);
    let dead_nodes = graph
        .iter()
        .filter(|node| !live_nodes.contains(node))
        .collect::<LinkedList<_>>();
    for dead_node in dead_nodes {
        graph.remove_node(dead_node);
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
}

pub(super) fn const_branch_optimization(mut three_code: ThreeCode) -> ThreeCode {
    three_code.functions = three_code
        .functions
        .into_par_iter()
        .map(|(fname, mut function)| {
            const_branch_opt_graph(&mut function.graph, &mut function.code);
            (fname, function)
        })
        .collect();
    const_branch_opt_graph(&mut three_code.graph, &mut three_code.code);
    three_code
}
