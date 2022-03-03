mod arm;
mod three_code;

use crate::intermediate::Program;
use arm::ArmResult;
use three_code::{StatType, ThreeCode};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PropagationOpt {
    Symbolic,
    Constant,
    None,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Options {
    pub sethi_ullman_weights: bool,
    pub dead_code_removal: bool,
    pub propagation: PropagationOpt,
    pub inlining: bool,
    pub tail_call: bool,
    pub hoisting: bool,
    pub strength_reduction: bool,
    pub loop_unrolling: bool,
    pub common_expressions: bool,
    pub show_arm_temp_rep: bool,
}

pub struct BackendOutput {
    pub assembly: String,
    pub intermediates: Vec<String>,
}

/// Compiles the given program into an arm32 assembly
pub fn compile(program: Program, options: Options) -> BackendOutput {
    let three_code = ThreeCode::from((program, &options));
    #[cfg(debug_assertions)]
    for stat_type in &three_code.graph {
        if let StatType::Dummy(_) = &*stat_type.get() {
            panic!("Expected no dummy nodes in the final three code representation");
        }
    }

    // the arm result can return a printable intermediate representation.
    let ArmResult(armcode, temp_rep) = ArmResult::from((three_code, &options));

    // return the final arm code, and any intermediate representations selected by options
    BackendOutput {
        assembly: armcode.to_string(),
        intermediates: match temp_rep {
            Some(temp_rep) => vec![temp_rep],
            None => vec![],
        },
    }
}
