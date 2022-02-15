#![allow(unused_variables)]

mod assembly_gen;
mod graph_coloring;
mod ssa;
mod ssa_opt;
mod three_code;

use crate::intermediate::Program;
use graph_coloring::GeneralAssembly;
use ssa::StaticSingleAssignment;
use ssa_opt::optimize_ssa;
use three_code::ThreeCode;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum PropagationOpt {
    Procedural,
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
}

/// Compiles the given program into an arm32 assembly
fn compile(program: &Program, options: Options) -> String {
    let three_code = ThreeCode::from((program, &options));
    let ssa = StaticSingleAssignment::from(three_code);
    let ssa = optimize_ssa(ssa, &options);
    let general_assembly = GeneralAssembly::from(ssa);
    format!("{}", general_assembly)
}
