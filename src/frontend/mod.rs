#![allow(unused)]
mod ast;
mod error;
mod parser;
mod semantic;

pub use parser::{collect_errors, parse};

use self::error::Summary;

pub fn analyse<'a>(source_code: &'a str) -> Result<(), Summary<'a>> {
    let program = parse(source_code)?;

    Ok(())
}
