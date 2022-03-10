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
pub use modules::{gather_modules, GatherModulesError, InputFile};

/// Parse the source code, and then run semantic analysis.
/// If an error occurs in parsing or semantic analysis, an error summary is returned.
/// Otherwise, an intermediate representation of the program is returned.
pub fn analyse<'l>(
    main_input_file: &'l InputFile,
    module_input_files: Vec<&'l InputFile>,
) -> Result<Program, Summary<'l>> {
    let ast = parse(
        main_input_file.to_parse_contents(),
        module_input_files
            .into_iter()
            .map(InputFile::to_parse_contents)
            .collect(),
    )?;
    let (ast, function_symbol_tables, program_symbol_table) = analyse_semantics(ast)?;
    Ok(translate_ast(
        ast,
        function_symbol_tables,
        program_symbol_table,
    ))
}
