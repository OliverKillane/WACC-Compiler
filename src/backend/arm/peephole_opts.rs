//! A collection of peephole optimisation that can be performed on the arm
//! representation.

use crate::graph::Graph;

use super::arm_repr::{ArmNode, Cond, ControlFlow, FlexOperand, MovOp, Stat};
use std::ops::DerefMut;

/// Remove all redundant move instructions that are of form `Mov Rn, Rn` by
/// connecting the predecessor and successor nodes together.
pub fn remove_self_moves(mut start_node: ArmNode, cfg: &mut Graph<ControlFlow>) -> ArmNode {
    for node in cfg.iter() {
        if let ControlFlow::Simple(
            prev,
            Stat::Move(MovOp::Mov, Cond::Al, _, dst_ident, FlexOperand::ShiftReg(arg_ident, None)),
            next,
        ) = node.get_mut().deref_mut()
        {
            if dst_ident == arg_ident {
                match (prev, next) {
                    (Some(prev_node), Some(next_node)) => {
                        if prev_node != &node && next_node != &node {
                            prev_node.replace_successor(node.clone(), next_node.clone());
                            next_node.replace_predecessor(node.clone(), prev_node.clone());
                            if node == start_node {
                                start_node = next_node.clone();
                            }
                        }
                    }
                    (None, Some(next_node)) => {
                        if next_node != &node && node == start_node {
                            start_node = next_node.clone();
                            next_node.remove_predecessor(&node);
                        }
                    }
                    _ => (),
                }
            }
        }
    }
    start_node
}
