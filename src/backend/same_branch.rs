use super::three_code::*;
use crate::graph::{Deleted, Graph};
use std::collections::LinkedList;

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
        if &*code == &node {
            *code = next_node.clone()
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
    assert!(needless_branches.is_empty());
}

/// Applies an optimization so that nodes pointing with both branches to the same
/// successor node get removed.
pub(super) fn same_branch_optimization(
    ThreeCode {
        functions,
        data_refs,
        mut graph,
        read_ref,
        mut code,
        int_handler,
    }: ThreeCode,
) -> ThreeCode {
    let functions = functions
        .into_iter()
        .map(
            |(
                fname,
                Function {
                    mut graph,
                    args,
                    mut code,
                    read_ref,
                },
            )| {
                same_branch_opt_graph(&mut graph, &mut code);
                (
                    fname,
                    Function {
                        graph,
                        args,
                        code,
                        read_ref,
                    },
                )
            },
        )
        .collect();
    same_branch_opt_graph(&mut graph, &mut code);
    ThreeCode {
        functions,
        data_refs,
        graph,
        read_ref,
        code,
        int_handler,
    }
}
