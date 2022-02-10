#![allow(unused)]
mod ast;
mod error;
mod parser;
mod semantic;

pub use parser::{collect_errors, parse};

use self::error::Summary;

pub fn analyse(source_code: &str) -> Result<(), Summary> {
    let program = parse(source_code)?;

    Ok(())
}
