#![allow(unused)]
mod ast;
mod error;
mod parser;
mod semantic;

pub use parser::{parse, collect_errors};

pub fn analyse(source_code: &str) -> Result<(), ()> {
    todo!()
}
