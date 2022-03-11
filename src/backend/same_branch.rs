use std::collections::{HashMap, LinkedList};

use crate::graph::Deleted;

use super::three_code::*;

pub(super) fn same_branch_optimization(
    ThreeCode {
        functions: _,
        data_refs: _,
        graph: graph,
        read_ref: _,
        code: _,
        int_handler: _,
    }: ThreeCode,
) -> ThreeCode {
    let mut removed_node_mapping = HashMap::new();
    let needless_branches = LinkedList::new();
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
        removed_node_mapping.insert(node, next_node);
    }
    assert!(needless_branches.is_empty());
}
