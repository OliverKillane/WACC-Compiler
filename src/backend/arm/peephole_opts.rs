//! A collection of peephole optimisation that can be performed on the arm 
//! representation.

use std::ops::DerefMut;
use super::{ArmCode, arm_repr::{ControlFlow, Stat, FlexOperand}};

/// Remove all move instructions that are of form Mov Rn, Rn.
pub fn remove_self_moves(program : ArmCode) -> ArmCode {
    for node in program.cfg.iter() {
        if let ControlFlow::Simple(Some(prev), Stat::Move(_, _, _, dst_ident, FlexOperand::ShiftReg(arg_ident, _)), next) = node.get_mut().deref_mut() {
            if dst_ident == arg_ident {
                // this move is redundant, remove it from the graph
                if let Some(mut next_node) = next.clone() {
                    prev.replace_successor(node.clone(), next_node.clone());
                    next_node.replace_predecessor(node.clone(), prev.clone())
                } else {
                    prev.remove_successor()
                }
            }
        }
    }

    program
}