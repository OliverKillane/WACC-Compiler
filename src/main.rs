#![warn(missing_docs)]
#![doc(html_logo_url = "https://i.imgur.com/cBcRWvM.png")]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(let_else)]
#![feature(backtrace)]
#![feature(map_try_insert)]
#![feature(iter_intersperse)]
#![feature(cow_is_borrowed)]
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
//! wacc_33 0.6.9
//! Jordan Hall, Bartłomiej Cieślar, Panayiotis Gavriil and Oliver Killane
//! WACC compiler
//!
//! USAGE:
//!     compile [OPTIONS] <FILE>
//!
//! ARGS:
//!     <FILE>    
//!
//! OPTIONS:
//!     -a, --arm-temp                  Print the backend representations (arm with temporaries)
//!         --const-prop                Enable constant propagation
//!         --dead-code                 Enable dead code elimination
//!     -f, --final-three-code          Print the three code optimised threecode of the program
//!     -h, --help                      Print help information
//!     -i, --ir-print                  Print the intermediate representation generated
//!         --inlining <MODE>           Set the function inlining mode [default: off] [possible values:
//!                                     off, low, medium, high]
//!     -o, --outputpath <FILE>         The name of the output file
//!         --tail-call                 run tail call optimisation
//!     -u, --unoptimised-three-code    Print the three code representation of the program
//!     -V, --version                   Print version information
//! ```

#[macro_use]
extern crate lazy_static;

mod backend;
mod frontend;
mod graph;
mod intermediate;
mod tests;

use backend::{compile, Options};
use clap::{ArgEnum, Parser};
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
#[derive(Parser)]
#[clap(author = "Jordan Hall, Bartłomiej Cieślar, Panayiotis Gavriil and Oliver Killane", about = "WACC compiler" , long_about = Some("A wacc compiler written is rust targeting 32 bit ARM systems"), version = "0.6.9")]
struct Args {
    #[clap(parse(from_os_str), value_name = "FILE")]
    filepath: PathBuf,

    #[clap(
        short,
        long,
        parse(from_os_str),
        value_name = "FILE",
        help = "The name of the output file"
    )]
    outputpath: Option<PathBuf>,

    #[clap(
        short,
        long,
        help = "Print the backend representations (arm with temporaries)"
    )]
    arm_temp: bool,

    #[clap(
        short,
        long,
        help = "Print the three code representation of the program"
    )]
    unoptimised_three_code: bool,

    #[clap(
        short,
        long,
        help = "Print the three code optimised threecode of the program"
    )]
    final_three_code: bool,

    #[clap(short, long, help = "Print the intermediate representation generated")]
    ir_print: bool,

    #[clap(long, help = "run tail call optimisation")]
    tail_call: bool,

    #[clap(
        long,
        arg_enum,
        default_value_t = InlineMode::Off,
        help = "Set the function inlining mode",
        value_name = "MODE"
    )]
    inlining: InlineMode,

    #[clap(long, help = "Enable constant propagation")]
    const_prop: bool,

    #[clap(long, help = "Enable constant branch optimization")]
    const_branch: bool,

    #[clap(long, help = "Enable dead code elimination")]
    dead_code: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ArgEnum)]
enum InlineMode {
    Off,
    Low,
    Medium,
    High,
}

impl From<InlineMode> for Option<usize> {
    /// Convert the inlining mode into an option of the inlining limit.
    fn from(mode: InlineMode) -> Self {
        match mode {
            InlineMode::Off => None,
            InlineMode::Low => Some(100),
            InlineMode::Medium => Some(2000),
            InlineMode::High => Some(100000),
        }
    }
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
        arm_temp,
        unoptimised_three_code,
        final_three_code,
        ir_print,
        tail_call,
        inlining,
        const_prop,
        const_branch,
        dead_code,
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
                dead_code_removal: dead_code,
                const_propagation: const_prop,
                const_branch,
                inlining: inlining.into(),
                tail_call,
                show_arm_temp_rep: arm_temp,
                show_three_code: unoptimised_three_code,
                show_optimised_three_code: final_three_code,
            };

            let result = compile(ir, options);

            for (name, temp) in result.intermediates {
                println!("{}:", name);
                println!("{}", temp)
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
