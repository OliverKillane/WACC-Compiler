//! Utility functions to allow easy manipulation of the arm graph for
//! translation and final register allocation.

use lazy_static::__Deref;

use super::arm_repr::{ArmNode, ControlFlow, Stat};
use crate::graph::Graph;
use std::ops::DerefMut;

/// A chain from one start armnode to and end armnode.
#[derive(Debug)]
pub struct Chain(pub ArmNode, pub ArmNode);

/// Link two pairs of node chain starts and ends together.
/// ```text
/// 1. (start <-> leftmiddle) (rightmiddle <-> end)
/// 2. start <-> leftmiddle <-> rightmiddle -> end
/// 3. start <-> end
/// ```
pub fn link_two_chains(Chain(start, leftmiddle): Chain, Chain(rightmiddle, end): Chain) -> Chain {
    link_two_nodes(leftmiddle, rightmiddle);
    Chain(start, end)
}

pub fn link_opt_to_chain(opt: Option<Chain>, chain: Chain) -> Chain {
    if let Some(other_c) = opt {
        link_two_chains(other_c, chain)
    } else {
        chain
    }
}

/// Given a vector of optional chains, link the present chains in order.
/// ```text
/// [Some(A <-> B), Some(C <-> D), None, Some(E <-> F), None]
/// [A <-> B, C <-> D, E <-> F]
/// A <-> B <-> C <-> D <-> E <-> F
/// Some(A <-> F)
/// ```
pub fn link_optional_chains(opt_chains: Vec<Option<Chain>>) -> Option<Chain> {
    opt_chains.into_iter().flatten().reduce(link_two_chains)
}

/// Given tuples of start and end nodes, chain them together.
/// ```text
/// 1. [(1 <-> 2), (3 <-> 4), ..., (n-1 <-> n)]
/// 2. 1 <-> 2 <-> 3 <-> 4 <-> ... <-> n-1 <-> n
/// 3. (1 <-> n)
/// ```
pub fn link_chains(nodes: Vec<Chain>) -> Option<Chain> {
    nodes.into_iter().reduce(link_two_chains)
}

/// Given a vector of statements, connect them together in a simple chain within
/// the graph. There must be at least one [stat](Stat).
pub fn link_stats(stats: Vec<Stat>, graph: &mut Graph<ControlFlow>) -> Chain {
    link_chains(
        stats
            .into_iter()
            .map(|stat| simple_node(stat, graph))
            .collect::<Vec<_>>(),
    )
    .expect("Must have at least one statement when linking")
}

/// Create a single simple node from a statement, returning the node as a pair
/// of node references.
pub fn simple_node(stat: Stat, graph: &mut Graph<ControlFlow>) -> Chain {
    let node = graph.new_node(ControlFlow::Simple(None, stat, None));
    Chain(node.clone(), node)
}

/// Link two nodes together
pub fn link_two_nodes(mut start: ArmNode, mut end: ArmNode) {
    start.set_successor(end.clone());
    end.set_predecessor(start);
}

/// Determines if an integer is an  8 bit pattern (can be used in immediate operand).
pub fn is_8_bit(i: i32) -> bool {
    (0..256).contains(&i)
}

impl ArmNode {
    /// Set the predecessor arm node, for Simple, Branch and Return the start
    /// node is set, for Multi the predecessor is added to the predecessors list.
    pub fn set_predecessor(&mut self, predecessor: Self) {
        match self.get_mut().deref_mut() {
            ControlFlow::Simple(pre, _, _)
            | ControlFlow::Branch(pre, _, _, _)
            | ControlFlow::Ltorg(pre)
            | ControlFlow::Return(pre, _) => {
                let _ = pre.insert(predecessor);
            }
            ControlFlow::Multi(pres, _) => pres.push(predecessor),
            ControlFlow::Removed => panic!("Cannot add a predecessor to a deleted node"),
        }
    }

    /// Set the successor node to an arm node.
    pub fn set_successor(&mut self, successor: Self) {
        match self.get_mut().deref_mut() {
            ControlFlow::Simple(_, _, succ)
            | ControlFlow::Branch(_, _, _, succ)
            | ControlFlow::Multi(_, succ) => {
                let _ = succ.insert(successor);
            }
            ControlFlow::Return(_, _) => panic!("There are no nodes after a return"),
            ControlFlow::Ltorg(_) => panic!("There are no nodes after a literal pool"),
            ControlFlow::Removed => panic!("There are no nodes connected to a removed node"),
        }
    }

    pub fn replace_successor(&mut self, successor: Self, new_successor: Self) {
        match self.get_mut().deref_mut() {
            ControlFlow::Simple(_, _, succ) | ControlFlow::Multi(_, succ) => {
                if &Some(successor) == succ {
                    let _ = succ.insert(new_successor);
                } else {
                    panic!("incorrect simple successor!")
                }
            }
            ControlFlow::Branch(_, succ1, _, succ2) => {
                if succ1 == &successor {
                    *succ1 = new_successor
                } else if &Some(successor) == succ2 {
                    let _ = succ2.insert(new_successor);
                } else {
                    panic!("incorrect branch successor!")
                }
            }
            ControlFlow::Ltorg(_) => panic!("There are no nodes after a return"),
            ControlFlow::Return(_, _) => panic!("There are no nodes after a literal pool"),
            ControlFlow::Removed => panic!("There are no nodes connected to a removed node"),
        }
    }

    pub fn replace_predecessor(&mut self, predecessor: Self, new_predecessor: Self) {
        match self.get_mut().deref_mut() {
            ControlFlow::Simple(pre, _, _)
            | ControlFlow::Ltorg(pre)
            | ControlFlow::Branch(pre, _, _, _)
            | ControlFlow::Return(pre, _) => {
                if &Some(predecessor) == pre {
                    let _ = pre.insert(new_predecessor);
                } else {
                    panic!("Attempted replace a predecessor that was not there - return")
                }
            }
            ControlFlow::Multi(pres, _) => {
                for pre in pres.iter_mut() {
                    if pre == &predecessor {
                        *pre = new_predecessor;
                        return;
                    }
                }
                panic!("Attempted replace a predecessor that was not there - multi")
            }
            ControlFlow::Removed => panic!("Cannot replace a predecessor to a removed node."),
        }
    }

    pub fn remove_successor(&mut self) {
        match self.get_mut().deref_mut() {
            ControlFlow::Simple(_, _, succ)
            | ControlFlow::Branch(_, _, _, succ)
            | ControlFlow::Multi(_, succ) => *succ = None,
            ControlFlow::Ltorg(_) => panic!("literal pool has no successor"),
            ControlFlow::Return(_, _) => panic!("return statement has no successor"),
            ControlFlow::Removed => panic!("cannot replace successor for a removed node"),
        }
    }

    pub fn remove_predecessor(&mut self, predecessor: &Self) {
        match self.get_mut().deref_mut() {
            ControlFlow::Simple(prev, _, _) |
            ControlFlow::Branch(prev, _, _, _) |
            ControlFlow::Ltorg(prev) |
            ControlFlow::Return(prev, _) => {
                if let Some(prev_node) = prev && prev_node == predecessor {
                    *prev = None
                } else {
                    panic!("Could not remove, predecessor was not true predecessor")
                }
            },
            ControlFlow::Multi(precs, _) => precs.retain(|other_node| other_node != predecessor),
            ControlFlow::Removed => panic!("cannot remove predecessor of removed node"),
        }
    }

    pub fn has_successor(&self, other: &Self) -> bool {
        match self.get().deref() {
            ControlFlow::Simple(_, _, succ) | ControlFlow::Multi(_, succ) => {
                if let Some(node) = succ {
                    node == other
                } else {
                    false
                }
            }
            ControlFlow::Branch(_, true_branch, _, succ) => {
                if let Some(node) = succ {
                    node == other || other == true_branch
                } else {
                    other == true_branch
                }
            }
            ControlFlow::Ltorg(_) | ControlFlow::Return(_, _) => false,
            ControlFlow::Removed => panic!("Cannot check successor of a removed node"),
        }
    }

    pub fn has_predecessor(&self, other: &Self) -> bool {
        match self.get().deref() {
            ControlFlow::Simple(pre, _, _)
            | ControlFlow::Return(pre, _)
            | ControlFlow::Branch(pre, _, _, _)
            | ControlFlow::Ltorg(pre) => {
                if let Some(node) = pre {
                    node == other
                } else {
                    false
                }
            }
            ControlFlow::Multi(pres, _) => pres.contains(other),
            ControlFlow::Removed => panic!("Cannot check predecessor of a removed node"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        super::arm_repr::{Cond, FlexOperand, Ident, RegOp, Shift},
        *,
    };
    use lazy_static::__Deref;

    fn check_simple_chain(stats: Vec<Stat>, start: ArmNode) {
        let mut prev_node: Option<ArmNode> = None;
        let mut current_node = Some(start);

        for stat in stats.into_iter() {
            match current_node {
                Some(node) => match node.get().deref() {
                    ControlFlow::Simple(prev, curr, next) => {
                        assert_eq!(prev, &prev_node);
                        prev_node = Some(node.clone());
                        assert_eq!(curr, &stat);
                        current_node = next.clone();
                    }
                    _ => panic!("Was not a simple node"),
                },
                None => panic!("Expected a node but there was none"),
            }
        }
    }

    #[test]
    fn can_link_simple_statements() {
        let stats = vec![
            Stat::ApplyOp(
                RegOp::Add,
                Cond::Al,
                false,
                Ident::Temp(0),
                Ident::Temp(0),
                FlexOperand::Imm(7),
            ),
            Stat::ApplyOp(
                RegOp::Sub,
                Cond::Ge,
                false,
                Ident::Temp(1),
                Ident::Temp(1),
                FlexOperand::Imm(8),
            ),
            Stat::ApplyOp(
                RegOp::Sub,
                Cond::Lt,
                false,
                Ident::Temp(7),
                Ident::Temp(8),
                FlexOperand::ShiftReg(Ident::Temp(1), Some(Shift::Lsl(3.into()))),
            ),
            Stat::ApplyOp(
                RegOp::Adc,
                Cond::Lt,
                true,
                Ident::Temp(7),
                Ident::Temp(8),
                FlexOperand::ShiftReg(Ident::Temp(2), Some(Shift::Lsl(3.into()))),
            ),
        ];

        let mut graph = Graph::new();

        let Chain(start, _) = link_stats(stats.clone(), &mut graph);
        check_simple_chain(stats, start)
    }

    #[test]
    #[should_panic]
    fn can_check_for_linked_chains() {
        let mut graph = Graph::new();

        let false_start = graph.new_node(ControlFlow::Simple(
            None,
            Stat::Call(String::from("hello"), None, vec![]),
            None,
        ));

        check_simple_chain(
            vec![
                Stat::Call(String::from("hello"), None, vec![]),
                Stat::Call(String::from("world"), None, vec![]),
            ],
            false_start,
        )
    }

    #[test]
    fn detects_left_shifted_8_bits() {
        assert!(is_8_bit(0b10000001 as i32));
        assert!(is_8_bit(0b10111 as i32));
        assert!(!is_8_bit(0b1000000111 as i32));
    }
}
