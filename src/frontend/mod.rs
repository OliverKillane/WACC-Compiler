#![allow(unused)]
mod ast;
mod error;
mod parser;
mod semantic;

pub use parser::{collect_errors, m1, parse};

pub fn analyse(source_code: &str) -> Result<(), ()> {
    todo!()
}
