//! Parser for WACC language grammar.
//!
//! [parse](parse) is the entry point. On success, will return [Program](Program)
//! and will return a [Summary](Summary) on failure.
#[cfg(test)]
mod tests;

mod lexer;

use super::ast::{
    ASTWrapper, AssignLhs, AssignRhs, BinOp, Expr, ExprWrap, Function, Param, Program, Stat,
    StatWrap, Type, UnOp,
};
use lexer::{parse_ident, ws, Lexer};
use nom::{
    branch::alt,
    character::complete::{char, none_of, one_of},
    combinator::{cut, eof, map, opt, success, value},
    error::{context, ParseError},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    IResult,
};
use nom_supreme::{
    error::{BaseErrorKind, ErrorTree, StackContext},
    final_parser::final_parser,
    multi::collect_separated_terminated,
    ParserExt,
};
use std::{
    collections::{HashMap, LinkedList},
    iter::zip,
    path::{Path, PathBuf},
};

use lexer::{parse_int, str_delimited};

/// Wraps the result of the parser in a [WrapSpan]
///
/// Mimics the builder pattern's span in order to move building the span
/// outside of parsing the AST node itself.
///
/// * `inner` - Parser whose Result is wrapped in a [WrapSpan].
fn span<'a, F: 'a, O, E: ParseError<&'a str>>(
    mut inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, ASTWrapper<&'a str, O>, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    move |input| {
        let (rest, ret) = inner(input)?;
        Ok((
            rest,
            ASTWrapper(input[..input.len() - rest.len()].trim_end(), ret),
        ))
    }
}

/// Parses the input string into a [Program]. If the input string contains
/// syntax errors, a [Vec<Summary>](Vec<Summary>) is produced instead to be used by the
/// error handler.
pub fn parse<'a>(
    main_input: &'a str,
    module_inputs: Vec<&'a str>,
) -> Result<Program<&'a str, &'a str>, Summary<'a>> {
    let main_semantic_info = final_parser(parse_program)(main_input);
    let module_semantic_infos = module_inputs
        .iter()
        .cloned()
        .map(final_parser(parse_module))
        .collect::<LinkedList<_>>();
    let mut error_trees = Vec::new();
    let ast = match main_semantic_info {
        Ok(ast) => Some(ast),
        Err(error_tree) => {
            error_trees.push((main_input, error_tree));
            None
        }
    };
    let module_functions = zip(&module_inputs, module_semantic_infos)
        .map(
            |(module_input, module_semantic_info)| match module_semantic_info {
                Ok(functions) => Some(functions),
                Err(error_tree) => {
                    error_trees.push((module_input, error_tree));
                    None
                }
            },
        )
        .collect::<Vec<_>>();
    if error_trees.is_empty() {
        let Program(functions, main_code) = ast.unwrap();
        let functions = vec![functions]
            .into_iter()
            .chain(
                module_functions
                    .into_iter()
                    .map(|module_semantic_info| module_semantic_info.unwrap()),
            )
            .flatten()
            .collect();
        Ok(Program(functions, main_code))
    } else {
        Err(error_trees.into_iter().fold(
            Summary::new(SummaryStage::Parser),
            |mut summary, (input, error_tree)| {
                convert_error_tree(&mut summary, input, error_tree);
                summary
            },
        ))
    }
}

/// Parses the mod declarations in the file.
pub fn parse_imports(input: &str) -> Result<(&str, Vec<PathBuf>), &str> {
    many0(delimited(
        tuple((ws(success(())), Lexer::Module.parser())),
        ws(parse_path),
        Lexer::SemiColon.parser(),
    ))(input)
    .map_err(|err| {
        if let nom::Err::Error(ErrorTree::Base { location, kind: _ }) = err {
            location
        } else {
            panic!("Expected a base error only")
        }
    })
}

/// Parses a unix-style path.
pub fn parse_path(input: &str) -> IResult<&str, PathBuf, ErrorTree<&str>> {
    let path_component = many0(alt((
        none_of("\0\\;/"),
        preceded(char('\\'), one_of("\\;/")),
    )));
    let first_path_component = many0(alt((
        none_of("\0\\;/"),
        preceded(char('\\'), one_of("\\;/")),
    )));
    map(
        tuple((
            first_path_component,
            many0(preceded(char('/'), path_component)),
        )),
        |(first_component, path)| {
            Path::new(
                &vec![first_component]
                    .into_iter()
                    .chain(path)
                    .into_iter()
                    .map(|component| component.into_iter().collect::<String>())
                    .intersperse("/".to_string())
                    .collect::<String>(),
            )
            .iter()
            .collect::<PathBuf>()
        },
    )(input)
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
fn parse_program(input: &str) -> IResult<&str, Program<&str, &str>, ErrorTree<&str>> {
    map(
        delimited(
            Lexer::Begin.parser().context("Start of Program"),
            tuple((
                many0(span(parse_func)),
                parse_stats(Lexer::End).context("End of Program"),
            )),
            eof.context("End of File"),
        ),
        |(funcs, stats)| Program(funcs, stats),
    )(input)
}

/// Parses an input string into a vector of [functions](Function).
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
fn parse_module(
    input: &str,
) -> IResult<&str, Vec<ASTWrapper<&str, Function<&str, &str>>>, ErrorTree<&str>> {
    delimited(
        Lexer::Begin.parser().context("Start of Program"),
        many0(span(parse_func)),
        eof.context("End of File"),
    )(input)
}

/// Parses an input string into a [Function].
///
/// Cuts after parsing "(", so that error handling is well-formatted.
fn parse_func(input: &str) -> IResult<&str, Function<&str, &str>, ErrorTree<&str>> {
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
                    map(Lexer::CloseParen.parser(), |_| vec![]),
                ))
                .cut()
                .context("Argument List"),
            ),
            preceded(Lexer::Is.parser(), parse_stats(Lexer::End)),
        )),
        |(t, id, params, block)| Function(t, ASTWrapper(id, id.into()), params, block),
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
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<StatWrap<&'a str, &'a str>>, ErrorTree<&str>> {
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
fn parse_stat(input: &str) -> IResult<&str, Stat<&str, &str>, ErrorTree<&str>> {
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
fn parse_lhs(input: &str) -> IResult<&str, AssignLhs<&str, &str>, ErrorTree<&str>> {
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
fn parse_array_elem(input: &str) -> IResult<&str, Vec<ExprWrap<&str, &str>>, ErrorTree<&str>> {
    many1(delimited(
        Lexer::OpenBracket.parser(),
        parse_expr,
        Lexer::CloseBracket.parser(),
    ))(input)
}

/// Parser for the right-hand side of an assign expression.
fn parse_rhs(input: &str) -> IResult<&str, AssignRhs<&str, &str>, ErrorTree<&str>> {
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
            map(call, |(id, es)| {
                AssignRhs::Call(ASTWrapper(id, id.into()), es)
            }),
            map(
                span(preceded(Lexer::Fst.parser(), parse_expr.cut())),
                |ASTWrapper(s, e)| AssignRhs::Expr(ASTWrapper(s, Expr::UnOp(UnOp::Fst, box e))),
            ),
            map(
                span(preceded(Lexer::Snd.parser(), parse_expr.cut())),
                |ASTWrapper(s, e)| AssignRhs::Expr(ASTWrapper(s, Expr::UnOp(UnOp::Snd, box e))),
            ),
            map(parse_expr, AssignRhs::Expr),
            map(span(array_liter), AssignRhs::Array),
        )),
    )(input)
}

/// Entry-point parser for building a [Expr]. Precedence and associativity of
/// operators is handled in this parser.
fn parse_expr(input: &str) -> IResult<&str, ExprWrap<&str, &str>, ErrorTree<&str>> {
    context("Expression", parse_expr_or)(input)
}

/// Performs a right-fold on an expression to build up an expression tree from
/// a vector of nodes. Right-folding allows for left-associative operators.
fn fold_expr<'a>(
    expr: ExprWrap<&'a str, &'a str>,
    rem: Vec<(&'a str, ExprWrap<&'a str, &'a str>)>,
    input: &'a str,
) -> ExprWrap<&'a str, &'a str> {
    rem.into_iter()
        .fold(expr, |acc, val| parse_bin_op(val, acc, input))
}

/// Parser for a `expr1 || expr2` expression.
fn parse_expr_or(input: &str) -> IResult<&str, ExprWrap<&str, &str>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_and(input)?;
    let (rest, exprs) = many0(tuple((
        Lexer::Or.parser(),
        parse_expr_and.cut().context("Expression"),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 && expr2` expression.
fn parse_expr_and(input: &str) -> IResult<&str, ExprWrap<&str, &str>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_eq(input)?;
    let (rest, exprs) = many0(tuple((
        Lexer::And.parser(),
        parse_expr_eq.cut().context("Expression"),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 == expr2` expression.
fn parse_expr_eq(input: &str) -> IResult<&str, ExprWrap<&str, &str>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_cmp(input)?;
    let (rest, exprs) = many0(tuple((
        alt((Lexer::Eq.parser(), Lexer::Ne.parser())),
        parse_expr_cmp.cut().context("Expression"),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 (>= | > | <= | <) expr2` expression.
fn parse_expr_cmp(input: &str) -> IResult<&str, ExprWrap<&str, &str>, ErrorTree<&str>> {
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
fn parse_expr_plus(input: &str) -> IResult<&str, ExprWrap<&str, &str>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_mult(input)?;
    let (rest, exprs) = many0(tuple((
        alt((Lexer::Plus.parser(), Lexer::Minus.parser())),
        parse_expr_mult.cut().context("Term Expression"),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 (* | / | %) expr2` expression.
fn parse_expr_mult(input: &str) -> IResult<&str, ExprWrap<&str, &str>, ErrorTree<&str>> {
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
fn parse_literal_keywords(input: &str) -> IResult<&str, ExprWrap<&str, &str>, ErrorTree<&str>> {
    alt((
        map(Lexer::True.parser(), |t| {
            ASTWrapper(t, Expr::Bool(t.parse::<bool>().unwrap()))
        }),
        map(Lexer::False.parser(), |t| {
            ASTWrapper(t, Expr::Bool(t.parse::<bool>().unwrap()))
        }),
        map(Lexer::Null.parser(), |t| ASTWrapper(t, Expr::Null)),
        map(
            span(preceded(Lexer::Bang.parser(), parse_expr.cut())),
            |ASTWrapper(s1, e)| ASTWrapper(s1, Expr::UnOp(UnOp::Neg, box e)),
        ),
        map(
            span(preceded(Lexer::Minus.parser(), parse_expr.cut())),
            |ASTWrapper(s1, e)| ASTWrapper(s1, Expr::UnOp(UnOp::Minus, box e)),
        ),
        map(
            span(preceded(Lexer::Len.parser(), parse_expr.cut())),
            |ASTWrapper(s1, e)| ASTWrapper(s1, Expr::UnOp(UnOp::Len, box e)),
        ),
        map(
            span(preceded(Lexer::Ord.parser(), parse_expr.cut())),
            |ASTWrapper(s1, e)| ASTWrapper(s1, Expr::UnOp(UnOp::Ord, box e)),
        ),
        map(
            span(preceded(Lexer::Chr.parser(), parse_expr.cut())),
            |ASTWrapper(s1, e)| ASTWrapper(s1, Expr::UnOp(UnOp::Chr, box e)),
        ),
    ))(input)
}

/// Parser for atomic expressions such as newpair, integers, string/character
/// literals, unary expressions and '('expr')'.
fn parse_expr_atom(input: &str) -> IResult<&str, Expr<&str, &str>, ErrorTree<&str>> {
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
        map(parse_literal_keywords, |ASTWrapper(_, e)| e),
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
    tup: (&'a str, ExprWrap<&'a str, &'a str>),
    expr1: ExprWrap<&'a str, &'a str>,
    input: &'a str,
) -> ExprWrap<&'a str, &'a str> {
    let (op, expr2) = tup;

    let min = expr1.0.as_ptr().min(expr2.0.as_ptr()) as usize - input.as_ptr() as usize;
    let max = (expr2.0.as_ptr() as usize + expr2.0.len())
        .max(expr1.0.as_ptr() as usize + expr1.0.len())
        - input.as_ptr() as usize;
    let s = &input[min..max];

    ASTWrapper(
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
            for (_, context) in contexts {
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
pub fn convert_error_tree<'a>(summary: &mut Summary<'a>, input: &'a str, err: ErrorTree<&'a str>) {
    let mut h = HashMap::new();
    collect_errors(err, &mut h, vec![]);

    for (k, v) in h {
        let k = match k {
            "" => &input[input.len() - 2..],
            _ => &k[..1],
        };

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
            SummaryComponent::new(SummaryType::Error, 100, k, format!("Expected {}", contexts))
                .set_note(format!("Try: {}", expected.join(", "))),
        );
        summary.add_cell(summary_cell);
    }
}

#[cfg(test)]
mod unit_tests {
    use indoc::indoc;

    use super::*;

    #[test]
    fn expr_folds_correctly() {
        let expr = fold_expr(
            ASTWrapper("", Expr::Int(0)),
            vec![
                ("-", ASTWrapper("", Expr::Int(1))),
                ("+", ASTWrapper("", Expr::Int(2))),
                ("-", ASTWrapper("", Expr::Int(3))),
            ],
            "",
        );

        assert_eq!(
            expr,
            ASTWrapper(
                "",
                Expr::BinOp(
                    box ASTWrapper(
                        "",
                        Expr::BinOp(
                            box ASTWrapper(
                                "",
                                Expr::BinOp(
                                    box ASTWrapper("", Expr::Int(0)),
                                    BinOp::Sub,
                                    box ASTWrapper("", Expr::Int(1)),
                                )
                            ),
                            BinOp::Add,
                            box ASTWrapper("", Expr::Int(2)),
                        )
                    ),
                    BinOp::Sub,
                    box ASTWrapper("", Expr::Int(3)),
                )
            )
        );
    }

    #[test]
    fn mod_delcarations_parsed_correctly() {
        let contents = indoc! {"
                            mod a.wacc;
            mod b/c.txt;
            mod d////e/f/g.txt;
            mod h/i\\;\\\\ .txt;
        "};
        if let Ok((_, mods)) = parse_imports(contents) {
            assert_eq!(
                mods.iter()
                    .map(|path| path.as_os_str().to_str())
                    .collect::<Vec<_>>(),
                vec![
                    Some("a.wacc"),
                    Some("b/c.txt"),
                    Some("d/e/f/g.txt"),
                    Some("h/i;\\ .txt")
                ]
            )
        } else {
            panic!("Parsing should succeed")
        }
    }
}
