#[macro_use]
use lazy_static::lazy_static;

lazy_static! {
    static ref HASHSET: HashSet<&'static str> = {
        let arr = [
            "if", "then", "else", "fi", "fst", "snd", "int", "bool", "char", "string", "pair",
            "newpair", "begin", "end", "is", "while", "do", "done", "exit", "return", "call",
            "println", "print", "skip", "read", "free", "chr", "ord", "len", "null", "false",
            "true",
        ];
        HashSet::from_iter(arr)
    };
}

const NO_WS_KEYWORDS: [Lexer; 4] = [Lexer::Int, Lexer::Bool, Lexer::Char, Lexer::String];

use core::fmt;
use nom::{
    branch::alt,
    bytes::complete::{escaped, escaped_transform, is_a, is_not, take_until, take_while},
    character::{
        complete::{
            alpha1, alphanumeric1, anychar, char, digit1, multispace0, multispace1, none_of,
            not_line_ending, one_of, space0,
        },
        is_alphabetic, is_alphanumeric,
    },
    combinator::{cond, map, map_res, not, opt, recognize, value},
    error::{context, ParseError},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use nom_supreme::{
    error::{BaseErrorKind, ErrorTree, Expectation, StackContext},
    final_parser::final_parser,
    tag::complete::tag,
    ParserExt,
};
use std::{collections::HashSet, error::Error};

use crate::frontend::ast;

pub fn comments(input: &str) -> IResult<&str, &str, ErrorTree<&str>> {
    recognize(pair(char('#'), alt((is_not("\n\r"), tag("")))))(input)
}

pub fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, ErrorTree<&str>>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, ErrorTree<&str>>,
{
    terminated(inner, many0(alt((comments, multispace1))))
}

// pub fn cond_two_parsers<'a, F1: 'a, F2: 'a, O>(first: bool, inner1: F1, inner2: F2) -> impl FnMut(&'a str) -> IResult<&'a str, O, ErrorTree<&str>>
// where
//     F1: FnMut(&'a str) -> IResult<&'a str, O, ErrorTree<&str>>,
//     F2: FnMut(&'a str) -> IResult<&'a str, O, ErrorTree<&str>>,
// {
//     move |input| {
//         if first {
//             inner1(input)
//         } else {
//             inner2(input)
//         }
//     }
// }

#[derive(PartialEq)]
pub enum Lexer {
    Fst,
    Snd,
    Int,
    Bool,
    Char,
    String,
    Begin,
    End,
    And,
    Or,
    Plus,
    Minus,
    Div,
    Mult,
    Mod,
    Gt,
    Gte,
    Eq,
    Ne,
    Lt,
    Lte,
    Pair,
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    If,
    Fi,
    Comma,
    SemiColon,
    Is,
    While,
    Do,
    Done,
    Assign,
    Then,
    Exit,
    Return,
    Call,
    Println,
    Print,
    Skip,
    Read,
    Free,
    Else,
    Newpair,
    Bang,
    True,
    False,
    Null,
    Len,
    Ord,
    Chr,
}

impl Lexer {
    pub fn parser<'a>(
        self,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, ErrorTree<&'a str>> + 'a {
        let parser = match &self {
            Self::Fst => tag("fst"),
            Self::Snd => tag("snd"),
            Self::Int => tag("int"),
            Self::Bool => tag("bool"),
            Self::Char => tag("char"),
            Self::Int => tag("int"),
            Self::String => tag("string"),
            Self::Begin => tag("begin"),
            Self::End => tag("end"),
            Self::Plus => tag("+"),
            Self::Minus => tag("-"),
            Self::Mult => tag("*"),
            Self::Div => tag("/"),
            Self::Mod => tag("%"),
            Self::And => tag("&&"),
            Self::Or => tag("||"),
            Self::Gt => tag(">"),
            Self::Gte => tag(">="),
            Self::Lt => tag("<"),
            Self::Lte => tag("<="),
            Self::Eq => tag("=="),
            Self::Ne => tag("!="),
            Self::Pair => tag("pair"),
            Self::OpenBracket => tag("["),
            Self::CloseBracket => tag("]"),
            Self::OpenParen => tag("("),
            Self::CloseParen => tag(")"),
            Self::Comma => tag(","),
            Self::If => tag("if"),
            Self::Then => tag("then"),
            Self::Else => tag("else"),
            Self::Fi => tag("fi"),
            Self::SemiColon => tag(";"),
            Self::Is => tag("is"),
            Self::While => tag("while"),
            Self::Do => tag("do"),
            Self::Done => tag("done"),
            Self::Assign => tag("="),
            Self::Call => tag("call"),
            Self::Exit => tag("exit"),
            Self::Println => tag("println"),
            Self::Print => tag("print"),
            Self::Skip => tag("skip"),
            Self::Read => tag("read"),
            Self::Free => tag("free"),
            Self::Return => tag("return"),
            Self::Newpair => tag("newpair"),
            Self::Bang => tag("!"),
            Self::True => tag("true"),
            Self::False => tag("false"),
            Self::Null => tag("null"),
            Self::Len => tag("len"),
            Self::Ord => tag("ord"),
            Self::Chr => tag("chr"),
        };

        move |input| {
            if NO_WS_KEYWORDS.contains(&self) {
                ws(terminated(parser.clone(), not(parse_ident)))(input)
                // ws(parser.clone())(input)
            } else {
                ws(parser.clone())(input)
            }
        }
    }
}

#[derive(Debug, Clone)]
struct KeywordIdentError;

impl fmt::Display for KeywordIdentError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Expected identifier, found keyword")
    }
}
impl std::error::Error for KeywordIdentError {}

pub fn parse_ident(input: &str) -> IResult<&str, &str, ErrorTree<&str>> {
    ws(map_res(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ))
        .context("identifier"),
        |s| match HASHSET.get(s) {
            None => Ok(s),
            _ => Err(KeywordIdentError),
        },
    ))(input)
}

pub fn parse_int(input: &str) -> IResult<&str, i32, ErrorTree<&str>> {
    ws(map_res(
        recognize(pair(opt(alt((char('-'), char('+')))), digit1)),
        &str::parse,
    ))(input)
}

pub fn str_delimited<'a>(
    del: &'static str,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, ErrorTree<&str>> {
    ws(delimited(
        tag(del),
        alt((
            escaped(none_of("\\\'\""), '\\', one_of("'0nt\"b\\rf").cut()),
            tag(""),
        ))
        .cut(),
        tag(del).cut(),
    ))
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     #[test]
//     #[should_panic]
//     fn parse_ident_catches_keyword() {
//         let input = "hello";
//         let output: Result<&str, ErrorTree<&str>> = final_parser(parse_ident)(input);
//         match output {
//             Err(wow) => println!("{}", wow),
//             Ok(wow) => println!("{}", wow)
//         }
//         // println!("{:?}", parse_ident(input));
//     }
// }
