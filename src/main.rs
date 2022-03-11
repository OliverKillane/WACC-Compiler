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
mod tests;

use backend::{compile, Options, PropagationOpt};
use clap::Parser;
use frontend::{analyse, gather_modules, GatherModulesError};
use path_absolutize::Absolutize;
use pathdiff::diff_paths;
use std::{
    env::current_dir,
    fs::File,
    io::{self, Write},
    path::{Path, PathBuf},
    process,
};

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
const ENCODING_FAILURE: i32 = 2;
const MODULE_NOT_FOUND_FAILURE: i32 = 3;
const MODULE_PARSING_FAILURE: i32 = 100;

fn get_relative_path(path: &Path) -> io::Result<PathBuf> {
    Ok(diff_paths(path, current_dir()?).unwrap())
}

/// Compiler main entry.
/// - Processes command line arguments controlling compiler behaviour.
/// - Halts and reports failures through returning exit codes.
#[allow(unused_variables)]
fn main() -> io::Result<()> {
    let Args {
        filepath: mut main_file_path,
        outputpath,
        backend_temps: temp_arm,
        ir_print,
    } = Args::parse();

    let (main_file, module_files) = match gather_modules(&main_file_path) {
        Ok(files) => files,
        Err(gather_err) => {
            let (message, exit_code) = match gather_err {
                GatherModulesError::MainFileNotPresent => (
                    format!(
                        "File \'{}\' not found",
                        get_relative_path(&main_file_path.absolutize()?)?.display()
                    ),
                    FILE_FAILURE,
                ),
                GatherModulesError::MainFileNoCode => (
                    format!(
                        "File \'{}\' does not contain any code besides imports",
                        get_relative_path(&main_file_path.absolutize()?)?.display()
                    ),
                    FILE_FAILURE,
                ),
                GatherModulesError::InvalidEncoding(path) => (
                    format!(
                        "File \'{}\': unsupported file encoding",
                        get_relative_path(&path)?.display()
                    ),
                    ENCODING_FAILURE,
                ),
                GatherModulesError::InvalidModDecl(path, line, column) => (
                    format!(
                        "File {}:{}:{}: error while parsing module imports",
                        get_relative_path(&path)?.display(),
                        line,
                        column
                    ),
                    MODULE_PARSING_FAILURE,
                ),
                GatherModulesError::ModuleNotPresent(path, module) => (
                    format!(
                        "Module \'{}\' imported from \'{}\' not found",
                        get_relative_path(&module)?.display(),
                        get_relative_path(&path)?.display()
                    ),
                    MODULE_NOT_FOUND_FAILURE,
                ),
            };
            println!("{}", message);
            process::exit(exit_code);
        }
    };

    match analyse(&main_file, module_files.iter().collect()) {
        Ok(ir) => {
            if cfg!(debug_assertions) && ir.validate().is_err() {
                panic!("Invalid Intermediate Representation{}\n", ir);
            }
            if ir_print {
                println!("Intermediate Representation:\n{}", ir);
            }
            let options = Options {
                sethi_ullman_weights: false,
                dead_code_removal: false,
                propagation: PropagationOpt::None,
                inlining: Some(usize::MAX),
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
                main_file_path.set_extension("s");
                File::create(main_file_path.file_name().unwrap())?
            };

            write!(file, "{}", result.assembly)?;

            process::exit(COMPILE_SUCCESS)
        }
        Err(mut summary) => {
            summary.add_input_file(
                main_file.contents(),
                get_relative_path(&main_file.filepath)?
                    .display()
                    .to_string(),
            );
            for module_file in &module_files {
                summary.add_input_file(
                    module_file.contents(),
                    get_relative_path(&module_file.filepath)?
                        .display()
                        .to_string(),
                );
            }
            println!("{}", summary);
            process::exit(summary.get_code());
        }
    }
}
