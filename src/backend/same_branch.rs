use super::three_code::*;
use crate::graph::Deleted;
use std::collections::{HashMap, HashSet, LinkedList};

fn map_closure(mapping: &mut HashMap<StatNode, StatNode>) {
    let mut visited = HashSet::new();
    let mapped_nodes = mapping.keys().cloned().collect::<LinkedList<_>>();
    for mut node in mapped_nodes {
        if visited.contains(&node) {
            continue;
        }
        let mut mapping_chain = vec![node.clone()];
        while let Some(next_node) = mapping.get(&node) {
            assert!(!visited.contains(next_node));
            mapping_chain.push(next_node.clone());
            node = next_node.clone();
        }
        for mapped_node in mapping_chain {
            visited.insert(mapped_node.clone());
            mapping.insert(mapped_node, node.clone());
        }
    }
}

pub(super) fn same_branch_optimization(
    ThreeCode {
        functions,
        data_refs,
        mut graph,
        read_ref,
        code,
        int_handler,
    }: ThreeCode,
) -> ThreeCode {
    let mut removed_node_mapping = HashMap::new();
    let mut needless_branches = LinkedList::new();
    for node in &graph {
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
        removed_node_mapping.insert(node.clone(), next_node.clone());
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
    map_closure(&mut removed_node_mapping);
    let functions = functions
        .into_iter()
        .map(
            |(
                fname,
                Function {
                    args,
                    code,
                    read_ref,
                },
            )| {
                (
                    fname,
                    Function {
                        args,
                        code: removed_node_mapping.get(&code).unwrap_or(&code).clone(),
                        read_ref,
                    },
                )
            },
        )
        .collect();
    ThreeCode {
        functions,
        data_refs,
        graph,
        read_ref,
        code,
        int_handler,
    }
}
