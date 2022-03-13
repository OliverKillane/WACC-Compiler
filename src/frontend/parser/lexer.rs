//! Lexes individual string tokens and handles whitespace.
//!
//! ## Errors
//! Two new errors are introduced: [KeywordIdentError](KeywordIdentError) and
//! [OutOfBoundsInt](OutOfBoundsInt).

lazy_static! {
    static ref KEYWORD_HASHSET: HashSet<&'static str> = {
        let arr = [
            "if", "then", "else", "fi", "fst", "snd", "int", "bool", "char", "string", "pair",
            "newpair", "begin", "end", "is", "while", "do", "done", "exit", "return", "call",
            "println", "print", "skip", "read", "free", "chr", "ord", "len", "null", "false",
            "true", "mod", "void",
        ];
        HashSet::from_iter(arr)
    };
}

use core::fmt;
use nom::{
    branch::alt,
    bytes::complete::is_not,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace1, none_of, one_of},
    combinator::{map, map_res, not, opt, recognize},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser,
};
use nom_supreme::{error::ErrorTree, tag::complete::tag, ParserExt};
use std::collections::HashSet;

/// Parser for WACC comments. Returns the parsed comment on success.
pub fn comments(input: &str) -> IResult<&str, &str, ErrorTree<&str>> {
    recognize(pair(char('#'), alt((is_not("\n\r"), tag("")))))(input)
}

/// Trims trailing whitespace and potential comments.
pub fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, ErrorTree<&str>>
where
    F: Parser<&'a str, O, ErrorTree<&'a str>>,
{
    terminated(inner, many0(alt((comments, multispace1))))
}

/// All possible tokens to be lexed. Allows for a compile-time guarantee that we
/// lex every possible token.
#[derive(PartialEq)]
pub enum Lexer {
    /// "fst"
    Fst,
    /// "snd"
    Snd,
    /// "int"
    Int,
    /// "bool"
    Bool,
    /// "char"
    Char,
    /// "string"
    String,
    /// "begin"
    Begin,
    /// "end"
    End,
    /// "&&"
    And,
    /// "||"
    Or,
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "/"
    Div,
    /// "*"
    Mult,
    /// "%"
    Mod,
    /// ">"
    Gt,
    /// ">="
    Gte,
    /// "=="
    Eq,
    /// "!="
    Ne,
    /// "<"
    Lt,
    /// "<="
    Lte,
    /// "pair"
    Pair,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "if"
    If,
    /// "fi"
    Fi,
    /// ","
    Comma,
    /// ";"
    SemiColon,
    /// "is"
    Is,
    /// "while"
    While,
    /// "do"
    Do,
    /// "done"
    Done,
    /// "="
    Assign,
    /// "then"
    Then,
    /// "exit"
    Exit,
    /// "return"
    Return,
    /// "call"
    Call,
    /// "println"
    Println,
    /// "print"
    Print,
    /// "skip"
    Skip,
    /// "read"
    Read,
    /// "free"
    Free,
    /// "else"
    Else,
    /// "newpair"
    Newpair,
    /// "!"
    Bang,
    /// "true"
    True,
    /// "false"
    False,
    /// "null"
    Null,
    /// "len"
    Len,
    /// "ord"
    Ord,
    /// "chr"
    Chr,
    /// "mod"
    Module,
    /// "void"
    Void
}

impl Lexer {
    /// Returns parser for desired parser. Returned parser returns the desired token, with no
    /// trailing whitespace, and a result with an [ErrorTree](ErrorTree) if parsing the token fails.
    /// Whitespace is always consumed, but is not in the matched token.
    /// If the token exists in [KEYWORD_HASHSET](static@KEYWORD_HASHSET) then an assertion is made
    /// that no possible identifier follows before a whitespace.
    /// Example:
    /// ```
    /// let success = Lexer::Println.parser()("println");
    /// let fail = Lexer::Println.parser()("print");
    /// assert!(success.is_ok());
    /// assert!(fail.is_err());
    /// ```
    pub fn parser<'a>(
        self,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, ErrorTree<&'a str>> + 'a {
        let literal = match self {
            Lexer::Fst => "fst",
            Lexer::Snd => "snd",
            Lexer::Int => "int",
            Lexer::Bool => "bool",
            Lexer::Char => "char",
            Lexer::String => "string",
            Lexer::Begin => "begin",
            Lexer::End => "end",
            Lexer::Plus => "+",
            Lexer::Minus => "-",
            Lexer::Mult => "*",
            Lexer::Div => "/",
            Lexer::Mod => "%",
            Lexer::And => "&&",
            Lexer::Or => "||",
            Lexer::Gt => ">",
            Lexer::Gte => ">=",
            Lexer::Lt => "<",
            Lexer::Lte => "<=",
            Lexer::Eq => "==",
            Lexer::Ne => "!=",
            Lexer::Pair => "pair",
            Lexer::OpenBracket => "[",
            Lexer::CloseBracket => "]",
            Lexer::OpenParen => "(",
            Lexer::CloseParen => ")",
            Lexer::Comma => ",",
            Lexer::If => "if",
            Lexer::Then => "then",
            Lexer::Else => "else",
            Lexer::Fi => "fi",
            Lexer::SemiColon => ";",
            Lexer::Is => "is",
            Lexer::While => "while",
            Lexer::Do => "do",
            Lexer::Done => "done",
            Lexer::Assign => "=",
            Lexer::Call => "call",
            Lexer::Exit => "exit",
            Lexer::Println => "println",
            Lexer::Print => "print",
            Lexer::Skip => "skip",
            Lexer::Read => "read",
            Lexer::Free => "free",
            Lexer::Return => "return",
            Lexer::Newpair => "newpair",
            Lexer::Bang => "!",
            Lexer::True => "true",
            Lexer::False => "false",
            Lexer::Null => "null",
            Lexer::Len => "len",
            Lexer::Ord => "ord",
            Lexer::Chr => "chr",
            Lexer::Module => "mod",
            Lexer::Void => "void",
        };

        move |input| match KEYWORD_HASHSET.get(literal) {
            None => ws(tag(literal))(input),
            Some(_) => ws(terminated(tag(literal), not(parse_ident)))(input),
        }
    }
}

#[derive(Debug, Clone)]
/// Error defined for the case where an identifier was matched that also exists
/// in [KEYWORD_HASHSET](static@KEYWORD_HASHSET). This ensures that no potential
/// variable/function name can also match a keyword.
struct KeywordIdentError;

impl fmt::Display for KeywordIdentError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a non-keyword identifier")
    }
}
impl std::error::Error for KeywordIdentError {}

/// Only 32-bit integers can be parsed in WACC. However our lexer can handle
/// integers of arbitrary size. To aid this, we return this error whenever Rust
/// cannot parse the lexed integer into a i32 (32-bit integer).
#[derive(Debug, Clone)]
struct OutOfBoundsInt;

impl fmt::Display for OutOfBoundsInt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a 32-bit integer")
    }
}
impl std::error::Error for OutOfBoundsInt {}

/// Parses an identifier. On success, a [&str](str) is produced. Else,
/// an [ErrorTree](ErrorTree) is produced with the context of the error to be
/// converted into a syntax error later.
pub fn parse_ident(input: &str) -> IResult<&str, &str, ErrorTree<&str>> {
    ws(map_res(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ))
        .context("identifier"),
        |s| match KEYWORD_HASHSET.get(s) {
            None => Ok(s),
            _ => Err(KeywordIdentError),
        },
    ))(input)
}

/// Parses a 32-bit integer. On success, an [i32](i32) is produced. Else,
/// an [ErrorTree](ErrorTree) is produced with the context of the error to be
/// converted into a syntax error later.
pub fn parse_int(input: &str) -> IResult<&str, i32, ErrorTree<&str>> {
    ws(recognize(pair(opt(alt((char('-'), char('+')))), digit1))
        .map_res_cut(|s: &str| match s.parse() {
            Err(_) => Err(OutOfBoundsInt),
            Ok(i) => Ok(i),
        })
        .context("Integer"))(input)
}

/// There are two cases in WACC where we want to produce escaped errors:
/// ```text
/// "hello\nworld"
/// ```
/// ```text
/// '\n'
/// ```
/// To prevent duplication of the parser, this combinator takes in the
/// delimiters, and produces a parser that can parse escaped string and char
/// literals. Fails if escaped character is illegal.
pub fn str_delimited<'a>(
    del: &'static str,
) -> impl FnMut(&'a str) -> IResult<&'a str, String, ErrorTree<&str>> {
    ws(map(
        delimited(
            tag(del),
            many0(alt((
                none_of("\\\'\""),
                map(preceded(char('\\'), one_of("'0nt\"b\\rf\'")), |c| match c {
                    'b' => '\u{0008}',
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    'f' => '\u{0012}',
                    '0' => '\0',
                    c => c,
                }),
            )))
            .cut(),
            tag(del).cut(),
        ),
        |s| s.into_iter().collect::<String>(),
    ))
}
