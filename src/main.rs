#![warn(missing_docs)]
#![doc(html_logo_url = "https://i.imgur.com/cBcRWvM.png")]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(let_else)]
#![feature(backtrace)]
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

use backend::{compile, Options, PropagationOpt};
use clap::Parser;
use colored::Colorize;
use frontend::analyse;
use std::{cmp::min, fs::read_to_string, fs::File, io::Write, path::PathBuf, process};

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

    #[clap(
        short,
        long,
        help = "print the backend representations (arm with temporaries)"
    )]
    backend_temps: bool,

    #[clap(short, long, help = "print the intermediate representation generated")]
    ir_print: bool,
}

/// Exit code for a file open failure.
const COMPILE_SUCCESS: i32 = 0;
const FILE_FAILURE: i32 = 1;

/// Compiler main entry.
/// - Processes command line arguments controlling compiler behaviour.
/// - Halts and reports failures through returning exit codes.
#[allow(unused_variables)]
fn main() -> std::io::Result<()> {
    let Args {
        mut filepath,
        outputpath,
        backend_temps: temp_arm,
        ir_print,
    } = Args::parse();

    let filestring = filepath.as_path().display().to_string();

    match read_to_string(filestring.clone()) {
        Ok(source_code) => match analyse(&source_code) {
            Ok(ir) => {
                if ir_print {
                    println!("INTERMEDIATE REPRESENTATION:\n{}", ir);
                }

                let options = Options {
                    sethi_ullman_weights: false,
                    dead_code_removal: false,
                    propagation: PropagationOpt::None,
                    inlining: false,
                    tail_call: false,
                    hoisting: false,
                    strength_reduction: false,
                    loop_unrolling: false,
                    common_expressions: false,
                    show_arm_temp_rep: temp_arm,
                };

                let result = compile(ir, options);

                if temp_arm {
                    for temp in result.intermediates {
                        println!("{}", temp)
                    }
                }

                let mut file = if let Some(outpath) = outputpath {
                    File::create(outpath)?
                } else {
                    filepath.set_extension("s");
                    File::create(filepath)?
                };

                write!(file, "{}", result.assembly)?;

                process::exit(COMPILE_SUCCESS)
            }
            Err(mut err) => {
                err.add_input_file(&source_code, filestring);
                println!("{}", err);
                process::exit(err.get_code());
            }
        },
        Err(err) => {
            // failed to open the file
            println!(
                "{}\n{}",
                "Error: Could not open file ".red().underline(),
                err
            );
            process::exit(FILE_FAILURE)
        }
    }
}
