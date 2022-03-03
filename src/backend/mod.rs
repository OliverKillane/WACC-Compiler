mod arm;
mod three_code;

use crate::intermediate::Program;
use three_code::ThreeCode;
use arm::ArmResult;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum PropagationOpt {
    Symbolic,
    Constant,
    None,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Options {
    sethi_ullman_weights: bool,
    dead_code_removal: bool,
    propagation: PropagationOpt,
    inlining: bool,
    tail_call: bool,
    hoisting: bool,
    strength_reduction: bool,
    loop_unrolling: bool,
    common_expressions: bool,
    show_arm_temp_rep: bool
}

pub struct BackendOutput {
    assembly: String,
    intermediates: Vec<String>
}

/// Compiles the given program into an arm32 assembly
fn compile(program: Program, options: Options) -> BackendOutput {
    let three_code = ThreeCode::from((program, &options));

    // the arm result can return a printable intermediate representation.
    let ArmResult(armcode, temp_rep) = ArmResult::from((three_code, &options));
    
    // return the final arm code, and any intermediate representations selected by options
    BackendOutput{
        assembly: armcode.to_string(),
        intermediates: match temp_rep {
            Some(temp_rep) => vec![temp_rep],
            None => vec![],
        }
    }
}
