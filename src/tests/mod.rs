#![cfg(test)]

use crate::backend::{compile, Options};
use crate::frontend::{analyse, gather_modules};
use glob::glob;
use indoc::indoc;
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
    fs::{create_dir_all, read_to_string, write},
    io::prelude::*,
    path::Path,
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
        value(Tokens::RuntimeError, tag("#runtime_error#")),
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
                println!("{:?}", iter.clone().collect::<String>());
                println!("{:?}", token);
                match token {
                    Tokens::String(s) => {
                        if &iter.by_ref().take(s.len()).collect::<String>() != s {
                            return false;
                        }
                    }
                    Tokens::Address => {
                        println!("{}", iter.clone().take(5).collect::<String>());
                        if iter.clone().take(5).collect::<String>() == "(nil)" {
                            let _ = iter.by_ref().take(5);
                            println!("after:{:?}", iter.clone().collect::<String>());
                            println!("Wow this worked");
                        } else {
                            if iter.by_ref().take(2).collect::<String>() != "0x" {
                                return false;
                            }
                            let mut peek = iter.clone();
                            while peek
                                .next()
                                .map(|c| c.is_numeric() || ('a'..='f').contains(&c))
                                == Some(true)
                            {
                                let _ = iter.next();
                            }
                        }
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
                println!("Failing here");
                return false;
            }
        }

        iter.next().is_none()
    }
}

#[test]
fn test_output_eq_behaviour() {
    let input = include_str!("static/expressions/charComparisonExpr.wacc");

    let expected_output = indoc! {
        "false
    true
    true
    true
    false
    false
    "
    };

    assert_eq!(parse_behaviour(input).unwrap().1 .0, expected_output);

    let input = include_str!("static/basic/skip/skip.wacc");

    let expected_output = indoc! {
        ""
    };

    assert_eq!(parse_behaviour(input).unwrap().1 .0, expected_output);

    let input = include_str!("static/pairs/printPair.wacc");

    let expected_output = "0x22150 = (10, a)\n";

    assert_eq!(parse_behaviour(input).unwrap().1 .0, expected_output);

    let input = include_str!("static/array/printRef.wacc");

    let expected_output = indoc! {
    "Printing an array variable gives an address, such as 0x234234\n"
    };

    assert_eq!(parse_behaviour(input).unwrap().1 .0, expected_output);

    let input = include_str!("static/runtimeErr/integerOverflow/intmultOverflow.wacc");

    let expected_output = indoc! {
    "2147483
    2147483000
    OverflowError: the result is too small/large to store in a 4-byte signed-integer.\n"
    };

    assert_eq!(parse_behaviour(input).unwrap().1 .0, expected_output);

    let input = include_str!("static/runtimeErr/divideByZero/divZero.wacc");

    let expected_output = indoc! {
    "DivideByZeroError: divide or modulo by zero\n"
    };

    assert_eq!(parse_behaviour(input).unwrap().1 .0, expected_output);
}

#[test]
fn behaviour_comparison() {
    // let source = "( [1, 2, 3] , [a, b, c] )\n[ 0x231f0 = (a, true), 0x23200 = (b, false) ]\n1, 2\narray, of, strings\ntrue, false, true\nxyz\n1, 2, 3\nthis is a string\ntrue\nx\n5\n";
    let source = "0x23200\n";

    let matching = Behaviour(vec![vec![Tokens::Address]]);

    assert_eq!(matching, source);
}

#[test]
fn all_types_test() {
    let behaviour = Behaviour(vec![
        vec![Tokens::String("( [1, 2, 3] , [a, b, c] )".to_string())],
        vec![
            Tokens::String("[ ".to_string()),
            Tokens::Address,
            Tokens::String(" = (a, true), ".to_string()),
            Tokens::Address,
            Tokens::String(" = (b, false) ]".to_string()),
        ],
        vec![Tokens::String("1, 2".to_string())],
        vec![Tokens::String("array, of, strings".to_string())],
        vec![Tokens::String("true, false, true".to_string())],
        vec![Tokens::String("xyz".to_string())],
        vec![Tokens::String("1, 2, 3".to_string())],
        vec![Tokens::String("this is a string".to_string())],
        vec![Tokens::String("true".to_string())],
        vec![Tokens::String("x".to_string())],
        vec![Tokens::String("5".to_string())],
    ]);
    let string = "( [1, 2, 3] , [a, b, c] )\n[ 0x231f0 = (a, true), 0x23200 = (b, false) ]\n1, 2\narray, of, strings\ntrue, false, true\nxyz\n1, 2, 3\nthis is a string\ntrue\nx\n5\n";
    assert_eq!(behaviour, string);
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
#[case("static/pairsExtended")]
#[case("static/while")]
#[case("static/voidCalls")]
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
        compiler_test(file.to_str().unwrap(), String::new(), output, exit_code);
    }
    Ok(())
}
lazy_static! {
    static ref FILE_SYSTEM_ID: AtomicUsize = AtomicUsize::new(0);
}

fn compiler_test(filename: &str, input: String, output: Behaviour, _exit_code: Option<i32>) {
    let (main_file, module_files) = gather_modules(Path::new(filename)).unwrap();

    let options = Options {
        sethi_ullman_weights: false,
        dead_code_removal: true,
        const_propagation: false,
        inlining: Some(1000),
        tail_call: true,
        hoisting: false,
        strength_reduction: false,
        loop_unrolling: false,
        common_expressions: true,
        show_arm_temp_rep: false,
        show_three_code: false,
    };
    let assembly = compile(
        analyse(&main_file, module_files.iter().collect()).unwrap(),
        options,
    )
    .assembly;

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

    assert!(stdout.status.success());

    let mut child = std::process::Command::new("qemu-arm")
        .args(&[
            "-L",
            "/usr/arm-linux-gnueabi/",
            &format!("tmp/tmp{}", file_id),
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("Unable to run program");

    let mut stdin = child.stdin.take().expect("Failed to open stdin");
    let _ = std::thread::spawn(move || {
        stdin
            .write_all(input.as_bytes())
            .expect("Failed to write to stdin");
    })
    .join();

    let stdout = dbg!(child.wait_with_output().expect("Failed to read stdout"));

    // match dbg!(exit_code) {
    //     Some(e) => assert_eq!(stdout.status.code().unwrap(), e),
    //     None => assert!(stdout.status.success()),
    // }

    assert_eq!(output, &String::from_utf8_lossy(&stdout.stdout).as_ref());
}
