//! A collection of peephole optimisation that can be performed on the arm
//! representation.

use super::{
    arm_repr::{ControlFlow, FlexOperand, MovOp, Stat},
    ArmCode,
};
use rayon::prelude::*;
use std::ops::DerefMut;

/// Remove all redundant move instructions that are of form `Mov Rn, Rn` by
/// connecting the predecessor and successor nodes together.
pub fn remove_self_moves(program: ArmCode) -> ArmCode {
    program
        .subroutines
        .par_iter()
        .map(|(_, sub)| &sub.cfg)
        .chain(rayon::iter::once(&program.cfg))
        .for_each(|graph| {
            for node in graph.iter() {
                if let ControlFlow::Simple(
                    Some(prev),
                    Stat::Move(MovOp::Mov, _, _, dst_ident, FlexOperand::ShiftReg(arg_ident, _)),
                    next,
                ) = node.get_mut().deref_mut()
                {
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
        });

    program
}
