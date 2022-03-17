mod arm;
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
use inlining::inline;
use same_branch::same_branch_optimization;
use three_code::ThreeCode;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Options {
    pub sethi_ullman_weights: bool,
    pub dead_code_removal: bool,
    pub const_propagation: bool,
    pub inlining: Option<usize>,
    pub tail_call: bool,
    pub hoisting: bool,
    pub strength_reduction: bool,
    pub loop_unrolling: bool,
    pub common_expressions: bool,
    pub show_arm_temp_rep: bool,
    pub show_three_code: bool,
}

pub struct BackendOutput {
    pub assembly: String,
    pub intermediates: Vec<(String, String)>,
}

/// Compiles the given program into an arm32 assembly
pub fn compile(program: Program, options: Options) -> BackendOutput {
    let mut three_code = same_branch_optimization(ThreeCode::from((program, &options)));
    if let Some(instructions_limit) = options.inlining {
        three_code = same_branch_optimization(inline(three_code, instructions_limit));
    }

    if options.tail_call {
        three_code = tail_call_optimise(three_code);
    }

    if options.const_propagation {
        three_code = prop_consts(three_code);
    }

    if options.dead_code_removal {
        three_code = remove_dead_code(three_code);
    }

    three_code = same_branch_optimization(three_code);

    #[cfg(debug_assertions)]
    three_code
        .check_dummy()
        .expect("There are left-over dummy nodes in the ThreeCode");

    let mut intermediates = vec![];

    if options.show_three_code {
        intermediates.push(("ThreeCode:".to_string(), three_code.to_string()))
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
