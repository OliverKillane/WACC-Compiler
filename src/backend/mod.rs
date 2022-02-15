mod assembly_gen;
mod graph_coloring;
mod ssa;
mod ssa_opt;
mod three_code;

use crate::intermediate::Program;
use std::fmt::Display;

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

struct ThreeCode;
struct SSA;
struct GeneralAssembly;

impl Into<ThreeCode> for (&Program, &Options) {
    fn into(self) -> ThreeCode {
        let (program, options) = self;
        todo!()
    }
}

impl Into<SSA> for ThreeCode {
    fn into(self) -> SSA {
        todo!()
    }
}

fn optimize_ssa(ssa: SSA, options: &Options) -> SSA {
    todo!()
}

impl Into<GeneralAssembly> for SSA {
    fn into(self) -> GeneralAssembly {
        todo!()
    }
}

impl Display for GeneralAssembly {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

/// Compiles the given program into an arm32 assembly
fn compile(program: &Program, options: Options) -> String {
    let three_code: ThreeCode = (program, &options).into();
    let ssa: SSA = three_code.into();
    let ssa = optimize_ssa(ssa, &options);
    let general_assembly: GeneralAssembly = ssa.into();
    format!("{}", general_assembly)
}
