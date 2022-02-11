//! Parser for WACC language grammar.
//!
//! [parse](parse) is the entry point. On success, will return [Program](Program)
//! and will return a [Summary](Summary) on failure.

mod lexer;
use std::collections::HashMap;

use lexer::{parse_ident, ws, Lexer};

use crate::frontend::ast::*;
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
    combinator::{cut, eof, map, map_res, not, opt, recognize, success, value},
    error::{context, ParseError},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};
use nom_supreme::{
    error::{BaseErrorKind, ErrorTree, Expectation, StackContext},
    final_parser::final_parser,
    multi::collect_separated_terminated,
    parser_ext::Context,
    tag::complete::tag,
    ParserExt,
};

use self::lexer::{parse_int, str_delimited};

#[cfg(test)]
mod tests;

/// Wraps the result of the parser in a [WrapSpan]
///
/// Mimics the builder pattern's span in order to move building the span
/// outside of parsing the AST node itself.
///
/// * `inner` - Parser whose Result is wrapped in a [WrapSpan].
fn span<'a, F: 'a, O, E: ParseError<&'a str>>(
    mut inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, WrapSpan<'a, O>, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    move |input| {
        let (rest, ret) = inner(input)?;
        Ok((
            rest,
            WrapSpan(input[..input.len() - rest.len()].trim_end(), ret),
        ))
    }
}

/// Parses the input string into a [Program]. If the input string contains
/// syntax errors, a [Vec<Summary>](Vec<Summary>) is produced instead to be used by the
/// error handler.
pub fn parse(input: &str) -> Result<Program<&str>, Vec<Summary>> {
    let semantic_info = final_parser(parse_program)(input);
    match semantic_info {
        Ok(ast) => Ok(ast),
        Err(err) => Err(vec![convert_error_tree(input, err)]),
    }
}

/// Parses an input string into a [Program].
///
/// Some examples of valid programs:
/// ```text
/// begin
///     skip
/// end
/// ```
///
/// ```text
/// begin
///     int add(int a, int b) is
///         return a + b
///     end
///     int ret = call add(5, 10);
///     println ret
/// end
/// ```
///
/// Example of invalid program:
/// ```text
/// begin
/// end
/// ```
fn parse_program(input: &str) -> IResult<&str, Program<&str>, ErrorTree<&str>> {
    map(
        delimited(
            tuple((
                ws(success(())),
                Lexer::Begin.parser().context("Start of Program"),
            )),
            tuple((
                many0(span(parse_func)),
                parse_stats(Lexer::End).context("End of Program"),
            )),
            eof.context("End of File"),
        ),
        |(funcs, stats)| Program(funcs, stats),
    )(input)
}

/// Parses an input string into a [Function].
///
/// Cuts after parsing "(", so that error handling is well-formatted.
fn parse_func(input: &str) -> IResult<&str, Function<&str>, ErrorTree<&str>> {
    map(
        tuple((
            parse_type,
            parse_ident,
            preceded(
                Lexer::OpenParen.parser(),
                alt((
                    collect_separated_terminated(
                        span(parse_param),
                        Lexer::Comma.parser(),
                        Lexer::CloseParen.parser(),
                    ),
                    map(Lexer::CloseParen.parser(), |_| Vec::new()),
                ))
                .cut()
                .context("Argument List"),
            ),
            preceded(Lexer::Is.parser(), parse_stats(Lexer::End)),
        )),
        |(t, id, params, block)| Function(t, id, params, block),
    )(input)
}

/// Combinator that takes in a terminating [Lexer] and returns a parser for
/// parsing statement blocks.
///
/// We make a new combinator as parsing statements with different terminating
/// tokens comes up a lot. This allows the process to be streamlined.
///
/// Cuts immediately, and after parsing the first [Stat], so that error
/// handling is well-formatted.
fn parse_stats<'a>(
    term: Lexer,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<WrapSpan<Stat<&'a str>>>, ErrorTree<&str>> {
    cut(context(
        "Statement/Block Terminator",
        collect_separated_terminated(
            span(parse_stat).cut(),
            Lexer::SemiColon.parser(),
            term.parser(),
        ),
    ))
}

/// Parser for building a [Stat].
///
/// Two sub-parsers are built up first for brevity: parse_while and parse_if.
/// These are large parsers so it benefits us to break up the code, but would
/// not be worth moving them into new parsers as they are only used here and
/// can be surely inlined by the compiler.
///
/// Cuts in appropriate places, so that error
/// handling is well-formatted.
fn parse_stat(input: &str) -> IResult<&str, Stat<&str>, ErrorTree<&str>> {
    let parse_while = preceded(
        Lexer::While.parser(),
        separated_pair(parse_expr, Lexer::Do.parser(), parse_stats(Lexer::Done)).cut(),
    );
    let parse_if = preceded(
        Lexer::If.parser(),
        tuple((
            separated_pair(parse_expr, Lexer::Then.parser(), parse_stats(Lexer::Else)),
            parse_stats(Lexer::Fi),
        ))
        .cut()
        .context("If Statement"),
    );

    context(
        "Statement",
        alt((
            value(Stat::Skip, Lexer::Skip.parser()),
            map(
                separated_pair(
                    tuple((parse_type, parse_ident.cut())),
                    Lexer::Assign.parser().cut(),
                    parse_rhs.cut(),
                ),
                |((t, id), rhs)| Stat::Def(t, id, rhs),
            ),
            preceded(Lexer::Read.parser(), map(parse_lhs.cut(), Stat::Read)),
            preceded(Lexer::Free.parser(), map(parse_expr.cut(), Stat::Free)),
            preceded(Lexer::Return.parser(), map(parse_expr.cut(), Stat::Return)),
            preceded(Lexer::Exit.parser(), map(parse_expr.cut(), Stat::Exit)),
            preceded(
                Lexer::Println.parser(),
                map(parse_expr.cut(), Stat::PrintLn),
            ),
            preceded(Lexer::Print.parser(), map(parse_expr.cut(), Stat::Print)),
            preceded(Lexer::Return.parser(), map(parse_expr.cut(), Stat::Return)),
            map(parse_if, |((e, st), sf)| Stat::If(e, st, sf)),
            map(parse_while, |(e, s)| Stat::While(e, s)),
            preceded(
                Lexer::Begin.parser(),
                map(parse_stats(Lexer::End).cut(), Stat::Block),
            ),
            context(
                "Assign Statement",
                map(
                    separated_pair(parse_lhs, Lexer::Assign.parser().cut(), parse_rhs.cut()),
                    |(lhs, rhs)| Stat::Assign(lhs, rhs),
                ),
            ),
        )),
    )(input)
}

/// Parses a parameter for function definitions and returns a [Stat]. Cuts
/// after the type is parsed so that error messaging is well-formed.
fn parse_param(input: &str) -> IResult<&str, Param<&str>, ErrorTree<&str>> {
    map(tuple((parse_type, parse_ident.cut())), |(t, ident)| {
        Param(t, ident)
    })(input)
}

/// First of two array descriptor parsers used in parsing types.
///
/// This allows for 0 or more sets of
/// "[" "]" to be parsed. Returns [usize] of the number of pairs of well-formed array
/// descriptors, as only this is needed when parsing types.
fn parse_array_desc0(input: &str) -> IResult<&str, usize, ErrorTree<&str>> {
    map(
        many0(pair(
            Lexer::OpenBracket.parser(),
            Lexer::CloseBracket.parser(),
        )),
        |vec| vec.len(),
    )(input)
}

/// Second of two array descriptor parsers used in parsing types.
///  
/// This allows for 1 or more sets of
/// "[" "]" to be parsed. Returns [usize] of the number of pairs of well-formed array
/// descriptors, as only this is needed when parsing types.
/// This parser was specifically designed for parsing multi-level pair-types.
fn parse_array_desc1(input: &str) -> IResult<&str, usize, ErrorTree<&str>> {
    map(
        many1(pair(
            Lexer::OpenBracket.parser(),
            Lexer::CloseBracket.parser(),
        )),
        |vec| vec.len(),
    )(input)
}

/// Parser for the [Type] AST Node. Can parse well-formed base types, array
/// types and pair types.
fn parse_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    map(
        pair(alt((parse_base_type, parse_pair_type)), parse_array_desc0),
        |(t, arr_depth)| match arr_depth {
            0 => t,
            d => Type::Array(box t, d),
        },
    )(input)
}

/// Parser for the [Type] AST Node. Handles the case when a pair type needs
/// to be parsed.
fn parse_pair_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    map(
        preceded(
            Lexer::Pair.parser(),
            delimited(
                Lexer::OpenParen.parser(),
                separated_pair(
                    parse_pair_elem_type,
                    Lexer::Comma.parser(),
                    parse_pair_elem_type,
                ),
                Lexer::CloseParen.parser(),
            ),
        ),
        |(f, s)| Type::Pair(box f, box s),
    )(input)
}

/// Parser for the [Type] AST Node. Handles the case where you want to parse
/// the sub-type of a [Type::Pair]. This was complicated as the spec allows
/// for pair types to be pair-elem types, but only in the case that it's also
/// an array.
fn parse_pair_elem_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    alt((
        value(
            Type::Pair(box Type::Any, box Type::Any),
            Lexer::Pair.parser(),
        ),
        map(
            tuple((parse_pair_type, parse_array_desc1.cut())),
            |(t, n)| Type::Array(box t, n),
        ),
        map(
            tuple((parse_base_type, parse_array_desc0.cut())),
            |(t, n)| match n {
                0 => t,
                n => Type::Array(box t, n),
            },
        ),
    ))(input)
}

/// Parser for the [Type] AST node.
fn parse_base_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    ws(alt((
        value(Type::Int, Lexer::Int.parser()),
        value(Type::Bool, Lexer::Bool.parser()),
        value(Type::Char, Lexer::Char.parser()),
        value(Type::String, Lexer::String.parser()),
    )))(input)
}

/// Parser for the left-hand side of an assign expression.
fn parse_lhs(input: &str) -> IResult<&str, AssignLhs<&str>, ErrorTree<&str>> {
    alt((
        map(preceded(Lexer::Fst.parser(), parse_expr.cut()), |pair| {
            AssignLhs::PairFst(pair)
        }),
        map(preceded(Lexer::Snd.parser(), parse_expr.cut()), |pair| {
            AssignLhs::PairSnd(pair)
        }),
        map(
            pair(
                parse_ident,
                alt((map(parse_array_elem, Some), success(None))),
            ),
            |(id, arr)| match arr {
                Some(arr) => AssignLhs::ArrayElem(id, arr),
                None => AssignLhs::Var(id),
            },
        ),
    ))(input)
}

/// Parser for an array element.
fn parse_array_elem(input: &str) -> IResult<&str, Vec<ExprSpan<&str>>, ErrorTree<&str>> {
    many1(delimited(
        Lexer::OpenBracket.parser(),
        parse_expr,
        Lexer::CloseBracket.parser(),
    ))(input)
}

/// Parser for the right-hand side of an assign expression.
fn parse_rhs(input: &str) -> IResult<&str, AssignRhs<&str>, ErrorTree<&str>> {
    let call = context(
        "Argument List",
        delimited(
            Lexer::Call.parser(),
            separated_pair(
                parse_ident,
                Lexer::OpenParen.parser(),
                separated_list0(Lexer::Comma.parser(), parse_expr).cut(),
            )
            .cut(),
            Lexer::CloseParen.parser(),
        ),
    );
    let array_liter = context(
        "Array Literal",
        delimited(
            Lexer::OpenBracket.parser(),
            separated_list0(Lexer::Comma.parser(), parse_expr)
                .cut()
                .context("Array Literal"),
            Lexer::CloseBracket.parser(),
        ),
    );
    context(
        "Assign RHS",
        alt((
            map(call, |(id, es)| AssignRhs::Call(id, es)),
            map(
                span(preceded(Lexer::Fst.parser(), parse_expr.cut())),
                |WrapSpan(s, e)| AssignRhs::Expr(WrapSpan(s, Expr::UnOp(UnOp::Fst, box e))),
            ),
            map(
                span(preceded(Lexer::Snd.parser(), parse_expr.cut())),
                |WrapSpan(s, e)| AssignRhs::Expr(WrapSpan(s, Expr::UnOp(UnOp::Snd, box e))),
            ),
            map(parse_expr, AssignRhs::Expr),
            map(span(array_liter), AssignRhs::Array),
        )),
    )(input)
}

/// Entry-point parser for building a [Expr]. Precedence and associativity of
/// operators is handled in this parser.
fn parse_expr(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    context("Expression", parse_expr_or)(input)
}

/// Performs a right-fold on an expression to build up an expression tree from
/// a vector of nodes. Right-folding allows for left-associative operators.
fn fold_expr<'a>(
    expr: WrapSpan<'a, Expr<'a, &'a str>>,
    rem: Vec<(&'a str, WrapSpan<'a, Expr<'a, &'a str>>)>,
    input: &'a str,
) -> WrapSpan<'a, Expr<'a, &'a str>> {
    rem.into_iter()
        .rfold(expr, |acc, val| parse_bin_op(val, acc, input))
}

/// Parser for a `expr1 || expr2` expression.
fn parse_expr_or(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_and(input)?;
    let (rest, exprs) = many0(tuple((
        Lexer::Or.parser(),
        parse_expr_and.cut().context("Expression"),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 && expr2` expression.
fn parse_expr_and(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_eq(input)?;
    let (rest, exprs) = many0(tuple((
        Lexer::And.parser(),
        parse_expr_eq.cut().context("Expression"),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 == expr2` expression.
fn parse_expr_eq(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_cmp(input)?;
    let (rest, exprs) = many0(tuple((
        alt((Lexer::Eq.parser(), Lexer::Ne.parser())),
        parse_expr_cmp.cut().context("Expression"),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 (>= | > | <= | <) expr2` expression.
fn parse_expr_cmp(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_plus(input)?;
    let (rest, exprs) = many0(tuple((
        alt((
            Lexer::Gte.parser(),
            Lexer::Lte.parser(),
            Lexer::Gt.parser(),
            Lexer::Lt.parser(),
        )),
        parse_expr_plus.cut().context("Summand Expression"),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 (+ | -) expr2` expression.
fn parse_expr_plus(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_mult(input)?;
    let (rest, exprs) = many0(tuple((
        alt((Lexer::Plus.parser(), Lexer::Minus.parser())),
        parse_expr_mult.cut().context("Term Expression"),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 (* | / | %) expr2` expression.
fn parse_expr_mult(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = span(parse_expr_atom)(input)?;
    let (rest, exprs) = many0(tuple((
        alt((
            Lexer::Mult.parser(),
            Lexer::Div.parser(),
            Lexer::Mod.parser(),
        )),
        span(parse_expr_atom).cut().context("Atom Expression"),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for unary expressions and bool/pair literals.
fn parse_literal_keywords(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    alt((
        map(Lexer::True.parser(), |t| {
            WrapSpan(t, Expr::Bool(t.parse::<bool>().unwrap()))
        }),
        map(Lexer::False.parser(), |t| {
            WrapSpan(t, Expr::Bool(t.parse::<bool>().unwrap()))
        }),
        map(Lexer::Null.parser(), |t| WrapSpan(t, Expr::Null)),
        map(
            span(preceded(Lexer::Bang.parser(), parse_expr.cut())),
            |WrapSpan(s1, e)| WrapSpan(s1, Expr::UnOp(UnOp::Neg, box e)),
        ),
        map(
            span(preceded(Lexer::Minus.parser(), parse_expr.cut())),
            |WrapSpan(s1, e)| WrapSpan(s1, Expr::UnOp(UnOp::Minus, box e)),
        ),
        map(
            span(preceded(Lexer::Len.parser(), parse_expr.cut())),
            |WrapSpan(s1, e)| WrapSpan(s1, Expr::UnOp(UnOp::Len, box e)),
        ),
        map(
            span(preceded(Lexer::Ord.parser(), parse_expr.cut())),
            |WrapSpan(s1, e)| WrapSpan(s1, Expr::UnOp(UnOp::Ord, box e)),
        ),
        map(
            span(preceded(Lexer::Chr.parser(), parse_expr.cut())),
            |WrapSpan(s1, e)| WrapSpan(s1, Expr::UnOp(UnOp::Chr, box e)),
        ),
    ))(input)
}

/// Parser for atomic expressions such as newpair, integers, string/character
/// literals, unary expressions and '('expr')'.
fn parse_expr_atom(input: &str) -> IResult<&str, Expr<&str>, ErrorTree<&str>> {
    let new_pair = delimited(
        pair(Lexer::Newpair.parser(), Lexer::OpenParen.parser().cut()),
        separated_pair(parse_expr, Lexer::Comma.parser(), parse_expr),
        Lexer::CloseParen.parser(),
    );
    alt((
        map(new_pair, |(left, right)| {
            Expr::BinOp(box left, BinOp::Newpair, box right)
        }),
        map(str_delimited("\'"), |s| {
            Expr::Char(s.chars().next().unwrap_or_default())
        })
        .context("Char Literal"),
        map(parse_int, Expr::Int),
        map(str_delimited("\""), |s| Expr::String(s.to_string())).context("String Literal"),
        map(
            pair(parse_ident, opt(parse_array_elem)),
            |(id, arr)| match arr {
                Some(arr) => Expr::ArrayElem(id, arr),
                None => Expr::Var(id),
            },
        ),
        map(parse_literal_keywords, |WrapSpan(_, e)| e),
        map(
            delimited(
                Lexer::OpenParen.parser(),
                parse_expr.cut(),
                Lexer::CloseParen.parser().cut(),
            ),
            |e| e.1,
        ),
    ))(input)
}

/// Matches a [str] unary operator and returns [UnOp]
fn parse_unary(op: &str) -> UnOp {
    match op {
        "!" => UnOp::Neg,
        "-" => UnOp::Minus,
        "len" => UnOp::Len,
        "ord" => UnOp::Ord,
        "chr" => UnOp::Chr,
        _ => unreachable!(),
    }
}

/// Matches the binary operator string and combines the two input [Expr]s
/// into a [Expr::BinOp]. Also calculates the associatity-agnsostic span for the
/// resulting [Expr], allowing for nice error translation
fn parse_bin_op<'a>(
    tup: (&'a str, WrapSpan<'a, Expr<'a, &'a str>>),
    expr1: WrapSpan<'a, Expr<'a, &'a str>>,
    input: &'a str,
) -> WrapSpan<'a, Expr<'a, &'a str>> {
    let (op, expr2) = tup;

    let min = expr1.0.as_ptr().min(expr2.0.as_ptr()) as usize - input.as_ptr() as usize;
    let max = (expr2.0.as_ptr() as usize + expr2.0.len())
        .min(expr1.0.as_ptr() as usize + expr1.0.len())
        - input.as_ptr() as usize;
    let s = &input[min..max];

    WrapSpan(
        s,
        Expr::BinOp(
            box expr1,
            match op {
                "+" => BinOp::Add,
                "-" => BinOp::Sub,
                "*" => BinOp::Mul,
                "/" => BinOp::Div,
                "%" => BinOp::Mod,
                ">" => BinOp::Gt,
                ">=" => BinOp::Gte,
                "<" => BinOp::Lt,
                "<=" => BinOp::Lte,
                "==" => BinOp::Eq,
                "!=" => BinOp::Ne,
                "&&" => BinOp::And,
                "||" => BinOp::Or,
                _ => unreachable!(),
            },
            box expr2,
        ),
    )
}

use crate::frontend::error::*;

/// Takes the error tree and groups them by the start of their spans.
pub fn collect_errors<'a>(
    err: ErrorTree<&'a str>,
    locations: &mut HashMap<&'a str, (Vec<StackContext>, Vec<BaseErrorKind>)>,
    mut past_contexts: Vec<StackContext>,
) {
    match err {
        ErrorTree::Base { location, kind } => {
            let loc = locations.entry(location).or_insert((vec![], vec![]));
            loc.1.push(kind);
            loc.0 = past_contexts;
        }
        ErrorTree::Stack { contexts, box base } => {
            for (location, context) in contexts {
                past_contexts.push(context);
            }

            collect_errors(base, locations, past_contexts.clone());
        }
        ErrorTree::Alt(siblings) => {
            for err_tree in siblings {
                collect_errors(err_tree, locations, past_contexts.clone());
            }
        }
    };
}

/// Converts the [ErrorTree] into a [Summary] which allows for nice user-facing
/// error printing.
pub fn convert_error_tree<'a>(input: &'a str, err: ErrorTree<&'a str>) -> Summary<'a> {
    let mut h = HashMap::new();
    collect_errors(err, &mut h, vec![]);

    let mut summary = Summary::new(input, SummaryStage::Parser);
    for (k, v) in h {
        let mut summary_cell = SummaryCell::new(input);
        let contexts: String =
            v.0.into_iter()
                .map(|v| match v {
                    StackContext::Context(s) => s.to_string(),
                    StackContext::Kind(nek) => format!("{:?}", nek),
                })
                .next()
                .unwrap_or_else(|| String::from("missing context"));
        let expected: Vec<String> =
            v.1.into_iter()
                .map(|v| match v {
                    BaseErrorKind::Expected(e) => format!("{}", e),
                    BaseErrorKind::Kind(e) => format!("{:?}", e),
                    BaseErrorKind::External(e) => format!("{}", e),
                })
                .collect();

        summary_cell.add_component(
            SummaryComponent::new(
                SummaryType::Error,
                100,
                &k[..1],
                format!("Expected {}", contexts),
            )
            .set_note(format!("Try: {}", expected.join(", "))),
        );
        summary.add_cell(summary_cell);
    }
    summary
}
