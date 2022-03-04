mod error;
pub mod parser;
pub(crate) mod semantic;
mod tests;

pub use error::Summary;
use parser::parse;
use semantic::analyse_semantics;

/// Parse the source code, and then run semantic analysis.
///
/// If an error occurs in parsing, an error summary is returned.
///
/// If an
pub fn analyse(source_code: &str) -> Result<(), Vec<Summary>> {
    let ast = parse(source_code)?;
    let _analysed_ast = analyse_semantics(ast, source_code)?;
    Ok(())
}
