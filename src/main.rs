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

use backend::{compile, Options, PropagationOpt};
use clap::Parser;
use colored::Colorize;
use frontend::analyse;
use intermediate::Program;
use std::fs::File;
use std::io::prelude::*;
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

    #[clap(short, long, help = "print the arm representation with temporaries")]
    temp_arm: bool,

    #[clap(short, long, help = "print the intermediate representation generated")]
    ir_print: bool,
}

/// Exit code for a file open failure.
const COMPILE_SUCCESS: i32 = 0;
const FILE_FAILURE: i32 = 1;

/// Compiler main entry.
/// - Processes command line arguments controlling compiler behaviour.
/// - Halts and reports failures through returning exit codes.
fn main() -> std::io::Result<()> {
    let Args {
        mut filepath,
        outputpath,
        temp_arm,
        ir_print,
    } = Args::parse();

    let filestring = filepath.as_path().display().to_string();

    match read_to_string(filestring.clone()) {
        Ok(source_code) => match analyse(&source_code) {
            Ok(ir) => {
                match if cfg!(debug_assertions) {
                    ir.validate()
                } else {
                    Ok(())
                } {
                    Ok(_) => {
                        if ir_print {
                            println!("THE INTERMEDIATE REPRESENTATION:\n{}", ir);
                        }
                    }
                    Err(_) => {
                        let Program(functions, local_vars, block_graph, data_refs, int_handler) =
                            ir;
                        if ir_print {
                            panic!("INVALID INTERMEDIATE REPRESENTATION: Program{{\n\t[\n{}\t],\n\t[\n{}\t],\n\t[\n{}\t],\n\t[\n{}\t],\n\t{:?}\n}}",
                                functions.iter().map(|f| format!("\t\t{:?}\n", f)).collect::<String>(),
                                local_vars.iter().map(|v| format!("\t\t{:?}\n", v)).collect::<String>(),
                                block_graph.iter().map(|b| format!("\t\t{:?}\n", b)).collect::<String>(),
                                data_refs.iter().map(|rf| format!("\t\t{:?}\n", rf)).collect::<String>(),
                                int_handler
                            )
                        } else {
                            panic!("INVALID INTERMEDIATE REPRESENTATION");
                        }
                    }
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

                for res in result.intermediates {
                    println!("{}", res)
                }

                let mut file = if let Some(outpath) = outputpath {
                    File::create(outpath)?
                } else {
                    filepath.set_extension("s");
                    File::create(filepath)?
                };

                file.write_all(result.assembly.as_bytes())?;

                process::exit(COMPILE_SUCCESS)
            }
            Err(errs) => {
                let mut exit_code = i32::MAX;
                for mut err in errs {
                    err.set_filepath(filestring.clone());
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
