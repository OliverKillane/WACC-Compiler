mod ast;
mod error;
mod modules;
mod parser;
mod semantic;
mod tests;
mod translation;

use crate::intermediate::Program;
use parser::parse;
use semantic::analyse_semantics;
use translation::translate_ast;

pub use error::Summary;
pub use modules::{GatherModulesError, InputFile};

/// Parse the source code, and then run semantic analysis.
/// If an error occurs in parsing or semantic analysis, an error summary is returned.
/// Otherwise, an intermediate representation of the program is returned.
pub fn analyse(source_code: &str) -> Result<Program, Summary> {
    let ast = parse(source_code)?;
    let (ast, function_symbol_tables, program_symbol_table) = analyse_semantics(ast)?;
    Ok(translate_ast(
        ast,
        function_symbol_tables,
        program_symbol_table,
    ))
}
