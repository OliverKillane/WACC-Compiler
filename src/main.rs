#![warn(missing_docs)]
#![doc(html_logo_url = "https://i.imgur.com/cBcRWvM.png")]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(let_else)]
#![feature(map_try_insert)]
#![feature(iter_intersperse)]
#![allow(dead_code)]
//! # The group 33 WACC Compiler project.
//!
//! Lovingly developed by:
//! - Jordan Hall
//! - Bartłomiej Cieślar
//! - Panayiotis Gavriil
//! - Oliver Killane
//!
//! ## Command Line interface
//! provides basic options for taking input file, setting output and help menus.
//!
//! for example `./compile --help`:
//! ```text
//! wacc_33 0.5.1
//! Jordan Hall, Bartłomiej Cieślar, Panayiotis Gavriil and Oliver Killane
//! A wacc compiler written is rust targeting 32 bit ARM systems
//!
//! USAGE:
//!     compile [OPTIONS] <INPUT FILE>
//!
//! ARGS:
//!     <INPUT FILE>
//!
//!
//! OPTIONS:
//!     -h, --help
//!             Print help information
//!
//!     -o, --outputpath <OUTPUT FILE>
//!             The name of the output file
//!
//!     -V, --version
//!             Print version information
//! ```

#[macro_use]
extern crate lazy_static;

mod backend;
mod frontend;
mod graph;
mod intermediate;

// #[cfg(test)]
// mod tests;

use clap::Parser;
use colored::Colorize;
use frontend::analyse;
use std::{cmp::min, fs::read_to_string, path::PathBuf, process};

/// Command line interface
#[derive(Parser, Debug)]
#[clap(author = "Jordan Hall, Bartłomiej Cieślar, Panayiotis Gavriil and Oliver Killane", about = "WACC compiler" , long_about = Some("A wacc compiler written is rust targeting 32 bit ARM systems"), version = "0.5.1")]
struct Args {
    #[clap(parse(from_os_str), value_name = "INPUT FILE")]
    filepath: PathBuf,

    #[clap(
        short,
        long,
        parse(from_os_str),
        value_name = "OUTPUT FILE",
        help = "The name of the output file"
    )]
    outputpath: Option<PathBuf>,
}

/// Exit code for a file open failure.
const COMPILE_SUCCESS: i32 = 0;
const FILE_FAILURE: i32 = 1;

/// Compiler main entry.
/// - Processes command line arguments controlling compiler behaviour.
/// - Halts and reports failures through returning exit codes.
fn main() {
    let args = Args::parse();
    let filepath = args.filepath.as_path().display().to_string();

    match read_to_string(args.filepath) {
        Ok(source_code) => match analyse(&source_code) {
            Ok(_ir) => process::exit(COMPILE_SUCCESS),
            Err(errs) => {
                let mut exit_code = i32::MAX;
                for mut err in errs {
                    err.set_filepath(filepath.clone());
                    exit_code = min(exit_code, err.get_code());
                    println!("{}", err)
                }
                process::exit(exit_code);
            }
        },
        Err(err) => {
            println!(
                "{}\n{}",
                "Error: Could not open file ".red().underline(),
                err
            );
            process::exit(FILE_FAILURE);
        }
    }
}
