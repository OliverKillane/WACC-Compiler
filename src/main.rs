#![warn(missing_docs)]
#![doc(html_logo_url = "https://i.imgur.com/cBcRWvM.png")]
//! The group 33 WACC Compiler project.
//!
//! Lovingly developed by:
//! - Jordan Hall
//! - Bartłomiej Cieślar\
//! - Panayiotis Gavriil
//! - Oliver Killane

use std::process::exit;

mod backend;
mod frontend;
mod intermediate;

trait ErrorType {
    fn exit_code(&self) -> i32;
}

/// Compiler main entry.
/// - Processes command line arguments controlling compiler behaviour.
/// - Halts and reports failures through returning exit codes.
fn main() {
    println!("Hello, world!");

    frontend::analyse("Source code").unwrap_or_else(|error| {
        println!("{}", error);
        exit(error.exit_code())
    });
}
