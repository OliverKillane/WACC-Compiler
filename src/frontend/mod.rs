#![allow(unused)]
mod ast;
mod error;
mod parser;
mod semantic;
mod tests;

pub use error::Summary;
use parser::{collect_errors, parse};
use semantic::analyse_semantics;

/// Parse the source code, and then run semantic analysis.
///
/// If an error occurs in parsing, an error summary is returned.
///
/// If an
pub fn analyse(source_code: &str) -> Result<(), Vec<Summary>> {
    let ast = parse(source_code)?;
    let analysed_ast = analyse_semantics(ast, source_code)?;
    Ok(())
}
