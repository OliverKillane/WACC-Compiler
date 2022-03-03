// #![cfg(test)]

use crate::frontend::analyse;
use colored::Colorize;
use glob::glob;
use lazy_static::lazy_static;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until},
    character::complete::{digit1, line_ending},
    combinator::{map, map_res, opt, recognize, value},
    multi::many1,
    sequence::{delimited, pair, preceded},
    IResult,
};
use rstest::rstest;
use std::{
    cmp::min,
    fs::{create_dir_all, read_to_string, write},
    io::prelude::*,
    process::Stdio,
    sync::atomic::AtomicUsize,
};

const FILE_FAILURE: i32 = 1;

#[derive(Clone, Debug)]
struct Behaviour(Vec<Vec<Tokens>>);

#[derive(Clone, Debug)]
enum Tokens {
    String(String),
    Address,
    Empty,
    RuntimeError,
}

fn parse_int(input: &str) -> IResult<&str, i32> {
    map_res(recognize(digit1), |s: &str| s.parse())(input)
}

fn parse_line(input: &str) -> IResult<&str, Vec<Tokens>> {
    many1(alt((
        map(is_not("#\n"), |s: &str| Tokens::String(s.to_string())),
        value(Tokens::Address, tag("#addrs#")),
        value(Tokens::Address, tag("#runtime_error#")),
    )))(input)
}

fn parse_behaviour(input: &str) -> IResult<&str, (Behaviour, Option<i32>)> {
    let (input, _) = pair(take_until("Output:\n"), tag("Output:\n"))(input)?;

    pair(
        map(
            alt((
                value(vec![vec![Tokens::Empty]], tag("# #empty#")),
                many1(delimited(tag("# "), parse_line, line_ending)),
            )),
            |v| Behaviour(v),
        ),
        opt(preceded(
            pair(take_until("Exit:\n# "), tag("Exit:\n# ")),
            parse_int,
        )),
    )(input)
}

const RUNTIME_ERRORS: [&str; 4] = [
    "NullReferenceError",
    "OverflowError",
    "DivideByZeroError",
    "ArrayIndexOutOfBoundsError",
];

impl PartialEq<&str> for Behaviour {
    fn eq(&self, other: &&str) -> bool {
        let mut iter = other.chars();
        for line in self.0.iter() {
            for token in line {
                match token {
                    Tokens::String(s) => {
                        if &iter.by_ref().take(s.len()).collect::<String>() != s {
                            return false;
                        }
                    }
                    Tokens::Address => {
                        if iter.by_ref().take(2).collect::<String>() != "0x" {
                            return false;
                        }
                        let _ = iter.by_ref().skip_while(|c| c.is_numeric());
                    }
                    Tokens::Empty => return iter.next() == None,
                    Tokens::RuntimeError => {
                        let rest = iter.as_str();
                        for err in RUNTIME_ERRORS {
                            if rest.starts_with(err) {
                                return true;
                            }
                        }
                        return false;
                    }
                }
            }
            if iter.next() != Some('\n') {
                return false;
            }
        }

        true
    }
}

#[rstest]
#[case("static/basic")]
#[case("static/expressions")]
#[case("static/pairs")]
#[case("static/variables")]
#[case("static/if")]
#[case("static/array")]
#[case("static/function")]
#[case("static/runtimeErr")]
#[case("static/scope")]
#[case("static/sequence")]
#[case("static/variables")]
#[case("static/while")]
fn examples_test(#[case] path: &str) {
    examples_dir_test(path).expect("Unable to test directory:");
}

fn examples_dir_test(dir: &str) -> Result<(), i32> {
    for file in glob(&format!("{}{}{}", "src/tests/", dir, "/**/*.wacc"))
        .unwrap()
        .map(|e| e.unwrap())
    {
        println!("{}", file.to_str().unwrap());
        let contents = read_to_string(&file).unwrap();
        let (output, exit_code) = dbg!(parse_behaviour(&contents).unwrap().1);
        compiler_test(file.to_str().unwrap(), String::new(), output, exit_code)?;
    }
    Ok(())
}
lazy_static! {
    static ref FILE_SYSTEM_ID: AtomicUsize = AtomicUsize::new(0);
}

fn compiler_test(
    filename: &str,
    input: String,
    output: Behaviour,
    exit_code: Option<i32>,
) -> Result<(), i32> {
    let assembly = match read_to_string(filename) {
        Ok(source_code) => match analyse(&source_code) {
            Ok(_ir) => Ok(".text

            .global main
            main:"
                .to_string()),
            Err(errs) => {
                let mut exit_code = i32::MAX;
                for mut err in errs {
                    err.set_filepath(filename.to_string());
                    exit_code = min(exit_code, err.get_code());
                    println!("{}", err)
                }
                Err(exit_code)
            }
        },
        Err(err) => {
            println!(
                "{}\n{}",
                "Error: Could not open file ".red().underline(),
                err
            );
            Err(FILE_FAILURE)
        }
    }?;

    let file_id = FILE_SYSTEM_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

    create_dir_all("./tmp/").unwrap();
    write(format!("./tmp/tmp{}.s", file_id), assembly.as_bytes()).expect("Unable to write file");

    let stdout = std::process::Command::new("arm-linux-gnueabi-gcc")
        .args(&[
            "-o",
            &format!("./tmp/tmp{}", file_id),
            "-mcpu=arm1176jzf-s",
            "-mtune=arm1176jzf-s",
            &format!("./tmp/tmp{}.s", file_id),
        ])
        .output()
        .expect("Unable to run program");

    dbg!(String::from_utf8_lossy(&stdout.stdout));

    assert!(stdout.status.success());

    let mut child = std::process::Command::new("qemu-arm")
        .args(&[
            "-L",
            "/usr/arm-linux-gnueabi/",
            &format!("tmp/tmp{}", file_id),
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Unable to run program");

    let mut stdin = child.stdin.take().expect("Failed to open stdin");
    let _ = std::thread::spawn(move || {
        stdin
            .write_all(input.as_bytes())
            .expect("Failed to write to stdin");
    }).join();

    let stdout = dbg!(child.wait_with_output().expect("Failed to read stdout"));

    // match dbg!(exit_code) {
    //     Some(e) => assert_eq!(stdout.status.code().unwrap(), e),
    //     None => assert!(stdout.status.success()),
    // }

    // assert_eq!(output, &String::from_utf8_lossy(&stdout.stdout).as_ref());

    Ok(())
}