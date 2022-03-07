mod ast;
mod error;
mod parser;
mod semantic;
mod tests;
mod translation;

use crate::intermediate::Program;
pub use error::Summary;
use parser::parse;
use semantic::analyse_semantics;
use translation::translate_ast;

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
