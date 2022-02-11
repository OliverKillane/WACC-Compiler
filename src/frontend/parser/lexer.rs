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
    IResult, Parser,
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
    F: Parser<&'a str, O, ErrorTree<&'a str>>,
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
        let literal = match &self {
            Self::Fst => "fst",
            Self::Snd => "snd",
            Self::Int => "int",
            Self::Bool => "bool",
            Self::Char => "char",
            Self::Int => "int",
            Self::String => "string",
            Self::Begin => "begin",
            Self::End => "end",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Mult => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::And => "&&",
            Self::Or => "||",
            Self::Gt => ">",
            Self::Gte => ">=",
            Self::Lt => "<",
            Self::Lte => "<=",
            Self::Eq => "==",
            Self::Ne => "!=",
            Self::Pair => "pair",
            Self::OpenBracket => "[",
            Self::CloseBracket => "]",
            Self::OpenParen => "(",
            Self::CloseParen => ")",
            Self::Comma => ",",
            Self::If => "if",
            Self::Then => "then",
            Self::Else => "else",
            Self::Fi => "fi",
            Self::SemiColon => ";",
            Self::Is => "is",
            Self::While => "while",
            Self::Do => "do",
            Self::Done => "done",
            Self::Assign => "=",
            Self::Call => "call",
            Self::Exit => "exit",
            Self::Println => "println",
            Self::Print => "print",
            Self::Skip => "skip",
            Self::Read => "read",
            Self::Free => "free",
            Self::Return => "return",
            Self::Newpair => "newpair",
            Self::Bang => "!",
            Self::True => "true",
            Self::False => "false",
            Self::Null => "null",
            Self::Len => "len",
            Self::Ord => "ord",
            Self::Chr => "chr",
        };

        move |input| match HASHSET.get(literal) {
            None => ws(tag(literal))(input),
            Some(_) => ws(terminated(tag(literal), not(parse_ident)))(input),
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

#[derive(Debug, Clone)]
struct OutOfBoundsInt;

impl fmt::Display for OutOfBoundsInt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a 32-bit integer")
    }
}
impl std::error::Error for OutOfBoundsInt {}

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
    ws(recognize(pair(opt(alt((char('-'), char('+')))), digit1))
        .map_res_cut(|s: &str| match s.parse() {
            Err(_) => Err(OutOfBoundsInt),
            Ok(i) => Ok(i),
        })
        .context("Integer"))(input)
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
