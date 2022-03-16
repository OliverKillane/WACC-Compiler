//! Creates a graph of live ranges for every node in the control flow graph of
//! the arm representation.
//!
//! These are then used for register allocation.
//!
//! This implementation makes use of set-sharing, i.e, when the only one
//! livein-set contributes to a livein, they share the same memory/set.
//!
//! This pass also determines the distance till next use for the temporaries in
//! the live-range sets for use in spilling (always spill furthest away if
//! necessary).
//!
//! Live ranges consider all basic instruction types in the arm representation
//! (as well as special nodes such as call and return). They also consider
//! conditional instructions (where destination is not always overwritten)

use crate::backend::data_flow::dataflow_analysis;

use super::arm_repr::{
    ArmCode, ArmNode, Cond, ControlFlow, FlexOffset, FlexOperand, Ident, MemOp, MemOperand, Stat,
    Temporary,
};
use lazy_static::__Deref;
use rayon::prelude::*;
use std::{cmp::min, collections::HashMap, sync::Arc};

type LiveAnalysis = Arc<HashMap<Temporary, usize>>;
fn construct_live_in(node: &ArmNode, live_out: LiveAnalysis) -> LiveAnalysis {
    let mut live_out = (&*live_out).clone();
    let mut defs = vec![];
    let mut uses = vec![];
    (&*node.get()).get_defs_and_uses(&mut defs, &mut uses);

    for temp in defs {
        live_out.remove(&temp);
    }
    for distance in live_out.values_mut() {
        *distance += 1;
    }
    for temp in uses {
        live_out.insert(temp, 0);
    }
    Arc::new(live_out)
}

fn live_init(node: &ArmNode) -> (LiveAnalysis, LiveAnalysis) {
    let live_out = Arc::new(HashMap::new());
    (construct_live_in(node, live_out.clone()), live_out)
}

fn live_update(
    _: Vec<&LiveAnalysis>,
    live_in: &LiveAnalysis,
    node: &ArmNode,
    live_out: &LiveAnalysis,
    succ_live_in: Vec<&LiveAnalysis>,
) -> (LiveAnalysis, LiveAnalysis, bool) {
    let (new_live_in, new_live_out) = if succ_live_in.len() == 1 {
        if succ_live_in[0] == live_out {
            return (live_in.clone(), live_out.clone(), false);
        } else {
            let new_live_out = succ_live_in[0].clone();
            (construct_live_in(node, new_live_out.clone()), new_live_out)
        }
    } else {
        let new_live_out = Arc::new(succ_live_in.into_iter().fold(
            HashMap::new(),
            |mut live_out, live_in| {
                for (&live_in_var, &new_distance) in live_in.iter() {
                    live_out
                        .entry(live_in_var)
                        .and_modify(|distance| *distance = min(*distance, new_distance))
                        .or_insert(new_distance);
                }
                live_out
            },
        ));
        (construct_live_in(node, new_live_out.clone()), new_live_out)
    };
    let updated = live_in != &new_live_in || live_out != &new_live_out;
    (new_live_in, new_live_out, updated)
}

pub type LiveRanges = HashMap<ArmNode, (Vec<Temporary>, Vec<Temporary>)>;
pub fn get_live_ranges(arm_code: &ArmCode) -> LiveRanges {
    dataflow_analysis(&arm_code.main, live_init, live_update, false)
        .into_par_iter()
        .chain(arm_code.subroutines.par_iter().flat_map(|(_, subroutine)| {
            dataflow_analysis(&subroutine.start_node, live_init, live_update, false).into_par_iter()
        }))
        .map(|(node, (live_in, live_out))| {
            let mut live_in = live_in
                .iter()
                .map(|(&temp, &distance)| (temp, distance))
                .collect::<Vec<_>>();
            let mut live_out = live_out
                .iter()
                .map(|(&temp, &distance)| (temp, distance))
                .collect::<Vec<_>>();
            live_in.sort_by_key(|(_, distance)| *distance);
            live_out.sort_by_key(|(_, distance)| *distance);
            (
                node,
                (
                    live_in.into_iter().map(|(temp, _)| temp).collect(),
                    live_out.into_iter().map(|(temp, _)| temp).collect(),
                ),
            )
        })
        .collect()
}

/// Trait used to enforce all components of the arm representation can have
/// temporary definitions and uses found.
///
/// If an instruction is conditional, it does not define the temporary (uses
/// still need to be propagated).
trait DefsUses {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>);
}

impl DefsUses for FlexOperand {
    fn get_defs_and_uses(&self, _: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        if let FlexOperand::ShiftReg(Ident::Temp(t), _) = self {
            uses.push(*t)
        }
    }
}

impl DefsUses for MemOperand {
    fn get_defs_and_uses(&self, _: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        match self {
            MemOperand::Zero(Ident::Temp(t)) | MemOperand::PreIndex(Ident::Temp(t), _) => {
                uses.push(*t)
            }
            _ => (),
        }
    }
}

impl DefsUses for FlexOffset {
    fn get_defs_and_uses(&self, _: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        if let FlexOffset::ShiftReg(_, Ident::Temp(t), _) = self {
            uses.push(*t)
        }
    }
}

impl DefsUses for Stat {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        let add_temps = |id: &Ident, vec: &mut Vec<Temporary>| {
            if let Ident::Temp(tid) = id {
                vec.push(*tid)
            } else {
                panic!("Expected a temporary value")
            }
        };
        match self {
            Stat::ApplyOp(_, cond, _, dst, arg1, flexop) => {
                if cond == &Cond::Al {
                    add_temps(dst, defs);
                }
                add_temps(arg1, uses);
                flexop.get_defs_and_uses(defs, uses)
            }
            Stat::Mul(cond, _, dst, arg1, arg2) => {
                if cond == &Cond::Al {
                    add_temps(dst, defs);
                }
                add_temps(arg1, uses);
                add_temps(arg2, uses)
            }
            Stat::MulA(cond, _, dstlo, dsthi, arg1, arg2) => {
                if cond == &Cond::Al {
                    add_temps(dsthi, defs);
                    add_temps(dstlo, defs);
                }
                add_temps(arg1, uses);
                add_temps(arg2, uses)
            }
            Stat::MulOp(_, cond, _, dstlo, dsthi, arg1, arg2) => {
                if cond == &Cond::Al {
                    add_temps(dsthi, defs);
                    add_temps(dstlo, defs);
                }
                add_temps(arg1, uses);
                add_temps(arg2, uses)
            }
            Stat::Move(_, cond, _, dst, flexop) => {
                if cond == &Cond::Al {
                    add_temps(dst, defs);
                }
                flexop.get_defs_and_uses(defs, uses)
            }
            Stat::Cmp(_, _, arg, flexop) => {
                add_temps(arg, uses);
                flexop.get_defs_and_uses(defs, uses)
            }
            Stat::SatOp(_, cond, dst, arg1, arg2) => {
                if cond == &Cond::Al {
                    add_temps(dst, defs);
                }
                add_temps(arg1, uses);
                add_temps(arg2, uses)
            }
            Stat::ReadCPSR(dst) => add_temps(dst, defs),
            Stat::MemOp(op, cond, _, ident, memop) => {
                match op {
                    MemOp::Ldr => {
                        if cond == &Cond::Al {
                            add_temps(ident, defs);
                        }
                    }
                    MemOp::Str => add_temps(ident, uses),
                }
                memop.get_defs_and_uses(defs, uses);
            }
            Stat::Push(_, arg) => add_temps(arg, uses),
            Stat::Pop(cond, dst) => {
                if cond == &Cond::Al {
                    add_temps(dst, defs);
                }
            }
            Stat::Link(_, _) => (),
            Stat::Call(_, dst, args) => {
                if let Some(dst) = dst {
                    defs.push(*dst)
                }
                args.iter().for_each(|arg| uses.push(*arg))
            }
            Stat::AssignStackWord(Ident::Temp(dst)) => defs.push(*dst),
            _ => (),
        }
    }
}

impl DefsUses for ControlFlow {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        match self {
            ControlFlow::Simple(_, stat, _) => stat.get_defs_and_uses(defs, uses),
            ControlFlow::Return(_, Some(arg)) => uses.push(*arg),
            _ => (),
        }
    }
}

impl DefsUses for ArmNode {
    fn get_defs_and_uses(&self, defs: &mut Vec<Temporary>, uses: &mut Vec<Temporary>) {
        self.get().deref().get_defs_and_uses(defs, uses)
    }
}
