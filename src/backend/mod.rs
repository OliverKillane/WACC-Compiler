//! # The WACC compiler Backend
//!
//! ## Representations
//! The two main representations used are the architecure-agnostic [threecode](ThreeCode)
//! upon which high-level optimisation are performed, and the lower-level arm
//! representation (which is initially temporaries upon which register allocation
//! using live ranges & next use distance is done).
//!
//! ## Optimisations
//! The main optimisations performed are:
//! - Inlining
//! - Constant Propagation
//! - Tail Call Optimisation
//! - Dead Code Removal
//! - Same Branch Removal
//!
//! These are done on the threecode graph, as this means any architecture specific
//! backend we develop requires no code changes to have these optimizations (just
//! need to translate from threecode)
//!
//! ## Assembly Generation
//! We translate the threecode to arm with temporaries, and live range analysis
//! (implemented using [data flow analysis](data_flow)) to allocate registers,
//! stack slots, and convert faux instructions (call, return) into arm instructions.
//!
//! ## Display
//! The backend can produce the display for intermediate code generated for both
//! the threecode, and arm with temporaries.

mod arm;
mod const_branch;
mod const_prop;
mod data_flow;
mod dead_code;
mod inlining;
mod same_branch;
mod tail_call_optimisation;
mod three_code;

use self::{
    const_prop::prop_consts, dead_code::remove_dead_code,
    tail_call_optimisation::tail_call_optimise,
};
use crate::intermediate::Program;
use arm::ArmResult;
use const_branch::const_branch_optimization;
use inlining::inline;
use same_branch::same_branch_optimization;
use three_code::ThreeCode;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Options {
    pub dead_code_removal: bool,
    pub const_propagation: bool,
    pub const_branch: bool,
    pub inlining: Option<usize>,
    pub tail_call: bool,
    pub show_arm_temp_rep: bool,
    pub show_three_code: bool,
    pub show_optimised_three_code: bool,
}

pub struct BackendOutput {
    pub assembly: String,
    pub intermediates: Vec<(String, String)>,
}

const CONST_LOOP_ITER_COUNT: u32 = 3;
/// Compiles the given program into an arm32 assembly
pub fn compile(program: Program, options: Options) -> BackendOutput {
    let mut three_code = ThreeCode::from((program, &options));
    let mut intermediates = vec![];

    if options.show_three_code {
        intermediates.push(("Unoptimised ThreeCode".to_string(), three_code.to_string()))
    }

    if let Some(instructions_limit) = options.inlining {
        three_code = inline(three_code, instructions_limit);
    }

    if options.tail_call {
        if options.const_branch {
            three_code = const_branch_optimization(three_code);
        }
        three_code = tail_call_optimise(same_branch_optimization(three_code));
    }

    if options.const_propagation && options.dead_code_removal && options.const_branch {
        for _ in 0..CONST_LOOP_ITER_COUNT {
            three_code = prop_consts(three_code);
            three_code = const_branch_optimization(three_code);
            three_code = same_branch_optimization(three_code);
            three_code = remove_dead_code(three_code);
        }
    } else {
        if options.const_propagation {
            three_code = prop_consts(three_code);
        }
        if options.dead_code_removal {
            three_code = remove_dead_code(three_code);
        }
    }

    if options.const_branch {
        three_code = const_branch_optimization(three_code);
    }
    three_code = same_branch_optimization(three_code);

    #[cfg(debug_assertions)]
    three_code
        .check_dummy()
        .expect("There are left-over dummy nodes in the ThreeCode");

    if options.show_optimised_three_code {
        intermediates.push(("Optimised ThreeCode".to_string(), three_code.to_string()))
    }

    // the arm result can return a printable intermediate representation.
    let ArmResult(armcode, temp_rep) = ArmResult::from((three_code, &options));

    if let Some(temp_arm) = temp_rep {
        intermediates.push(("Temporary Arm".to_string(), temp_arm))
    }

    // return the final arm code, and any intermediate representations selected by options
    BackendOutput {
        assembly: armcode.to_string(),
        intermediates,
    }
}
