#![allow(dead_code)]
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
    combinator::{cut, map, map_res, not, opt, recognize, success, value},
    error::{context, ParseError},
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};
use nom_supreme::{
    error::{BaseErrorKind, ErrorTree, Expectation, StackContext},
    final_parser::final_parser,
    multi::collect_separated_terminated,
    tag::complete::tag,
    ParserExt, parser_ext::Context,
};

use self::lexer::{parse_int, str_delimited};

#[cfg(test)]
mod tests;

type TreeResult<'a, T> = IResult<&'a str, T, ErrorTree<&'a str>>;

fn span<'a, F: 'a, O, E: ParseError<&'a str>>(
    mut inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, WrapSpan<'a, O>, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    move |input| {
        let (rest, ret) = inner(input)?;
        Ok((rest, WrapSpan(&input[..input.len() - rest.len()], ret)))
    }
}

pub fn parse(input: &str) -> Result<Program<&str>, ErrorTree<&str>> {
    final_parser(parse_program)(input)
}

fn parse_program(input: &str) -> IResult<&str, Program<&str>, ErrorTree<&str>> {
    map(
        preceded(
            tuple((ws(success(())), Lexer::Begin.parser())),
            tuple((many0(span(parse_func)), parse_stats(Lexer::End))),
        ),
        |(funcs, stats)| Program(funcs, stats),
    )(input)
}

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
                .cut(),
            ),
            preceded(Lexer::Is.parser(), parse_stats(Lexer::End)),
        )),
        |(t, id, params, block)| Function(t, id, params, block),
    )(input)
}

fn parse_stats<'a>(
    term: Lexer,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<WrapSpan<Stat<&'a str>>>, ErrorTree<&str>> {
    cut(collect_separated_terminated(
        span(parse_stat).cut(),
        Lexer::SemiColon.parser(),
        term.parser(),
    ))
}

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
        .cut(),
    );

    context(
        "statement",
        alt((
            value(Stat::Skip, Lexer::Skip.parser()),
            map(
                separated_pair(
                    tuple((parse_type, parse_ident.cut())),
                    Lexer::Assign.parser(),
                    parse_rhs,
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
            map(
                separated_pair(parse_lhs, Lexer::Assign.parser().cut(), parse_rhs.cut()),
                |(lhs, rhs)| Stat::Assign(lhs, rhs),
            ),
            map(
                separated_pair(parse_lhs, Lexer::Assign.parser().cut(), parse_rhs.cut()),
                |(lhs, rhs)| Stat::Assign(lhs, rhs),
            ),
            // context(
            //     "assign",
            //     map(
            //         separated_pair(parse_lhs, Lexer::Assign.parser(), parse_rhs.cut()),
            //         |(lhs, rhs)| Stat::Assign(lhs, rhs),
            //     ),
            // ),
        )),
    )(input)
}

fn parse_param(input: &str) -> IResult<&str, Param<&str>, ErrorTree<&str>> {
    map(tuple((parse_type, parse_ident.cut())), |(t, ident)| {
        Param(t, ident)
    })(input)
}

fn parse_array_desc0(input: &str) -> IResult<&str, usize, ErrorTree<&str>> {
    map(
        many0(pair(
            Lexer::OpenBracket.parser(),
            Lexer::CloseBracket.parser(),
        )),
        |vec| vec.len(),
    )(input)
}

fn parse_array_desc1(input: &str) -> IResult<&str, usize, ErrorTree<&str>> {
    map(
        many1(pair(
            Lexer::OpenBracket.parser(),
            Lexer::CloseBracket.parser(),
        )),
        |vec| vec.len(),
    )(input)
}

fn parse_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    map(
        pair(alt((parse_base_type, parse_pair_type)), parse_array_desc0),
        |(t, arr_depth)| match arr_depth {
            0 => t,
            d => Type::Array(box t, d),
        },
    )(input)
}

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

fn parse_base_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    ws(alt((
        value(Type::Int, Lexer::Int.parser()),
        value(Type::Bool, Lexer::Bool.parser()),
        value(Type::Char, Lexer::Char.parser()),
        value(Type::String, Lexer::String.parser()),
    )))(input)
}

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

fn parse_array_elem(input: &str) -> IResult<&str, Vec<ExprSpan<&str>>, ErrorTree<&str>> {
    many1(delimited(
        Lexer::OpenBracket.parser(),
        parse_expr,
        Lexer::CloseBracket.parser(),
    ))(input)
}

fn parse_rhs(input: &str) -> IResult<&str, AssignRhs<&str>, ErrorTree<&str>> {
    let call = delimited(
        Lexer::Call.parser(),
        separated_pair(
            parse_ident,
            Lexer::OpenParen.parser(),
            separated_list0(Lexer::Comma.parser(), parse_expr),
        )
        .cut(),
        Lexer::CloseParen.parser(),
    );
    let array_liter = delimited(
        Lexer::OpenBracket.parser(),
        separated_list0(Lexer::Comma.parser(), parse_expr).cut(),
        Lexer::CloseBracket.parser(),
    );
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
    ))(input)
}

fn parse_expr(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    parse_expr_or(input)
}

fn fold_expr<'a>(
    expr: WrapSpan<'a, Expr<'a, &'a str>>,
    rem: Vec<(&'a str, WrapSpan<'a, Expr<'a, &'a str>>)>,
    input: &'a str,
) -> WrapSpan<'a, Expr<'a, &'a str>> {
    rem.into_iter()
        .rfold(expr, |acc, val| parse_bin_op(val, acc, input))
}

fn parse_expr_or(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_and(input)?;
    let (rest, exprs) = many0(tuple((Lexer::Or.parser(), parse_expr_and)))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

fn parse_expr_and(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_eq(input)?;
    let (rest, exprs) = many0(tuple((Lexer::And.parser(), parse_expr_eq)))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

fn parse_expr_eq(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_cmp(input)?;
    let (rest, exprs) = many0(tuple((
        alt((Lexer::Eq.parser(), Lexer::Ne.parser())),
        parse_expr_cmp,
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

fn parse_expr_cmp(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_plus(input)?;
    let (rest, exprs) = many0(tuple((
        alt((
            Lexer::Gte.parser(),
            Lexer::Lte.parser(),
            Lexer::Gt.parser(),
            Lexer::Lt.parser(),
        )),
        parse_expr_plus,
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

fn parse_expr_plus(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = parse_expr_mult(input)?;
    let (rest, exprs) = many0(tuple((
        alt((Lexer::Plus.parser(), Lexer::Minus.parser())),
        parse_expr_mult,
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

fn parse_expr_mult(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (rest, sub_exp) = span(parse_expr_atom)(input)?;
    let (rest, exprs) = many0(tuple((
        alt((
            Lexer::Mult.parser(),
            Lexer::Div.parser(),
            Lexer::Mod.parser(),
        )),
        span(parse_expr_atom),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

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
        }),
        map(parse_int, Expr::Int),
        map(str_delimited("\""), |s| Expr::String(s.to_string())),
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
                parse_expr,
                Lexer::CloseParen.parser(),
            ),
            |e| e.1,
        ),
    ))(input)
}

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

pub fn collect_errors<'a>(err: ErrorTree<&'a str>, locations: &mut HashMap<&'a str, (Vec<StackContext>, Vec<BaseErrorKind>)>) {
    match err {
        ErrorTree::Base { location, kind } => { 
            locations.entry(location).or_insert((vec![], vec![])).1.push(kind);
        },
        ErrorTree::Stack { contexts, box base } => {
            for (location, context) in contexts {
                locations.entry(location).or_insert((vec![], vec![])).0.push(context);
            }
            collect_errors(base, locations);
        }
        ErrorTree::Alt(siblings) => {
            for err_tree in siblings {
                collect_errors(err_tree, locations);
            }
        }
    };
}

pub fn convert_error_tree<'a>(path: &'a str, input: &'a str, err: ErrorTree<&'a str>) -> Summary<'a> {
    // let summary_cell = Vec::new();

    let mut h = HashMap::new();
    let err = parse(include_str!("parser/invalid/syntaxErr/function/thisIsNotC.wacc")).unwrap_err();
    collect_errors(err, &mut h);

    let summary_cell =  ;
    
    let mut s = Summary::new(path, input, SummaryStage::Parser);
    s.add_cell(summary_cell);
    s
}

fn parse_str(input: &str) -> IResult<&str, &str, ErrorTree<&str>> {
    let esc = escaped(none_of("\\\"\'"), '\\', one_of("'0nt\"b\\rf"));
    let esc_or_empty = alt((esc, tag("")));
    delimited(tag("\""), esc_or_empty, tag("\""))(input)
}

fn parse_str(input: &str) -> IResult<&str, &str, ErrorTree<&str>> {
    let esc = escaped(none_of("\\\"\'"), '\\', one_of("'0nt\"b\\rf"));
    let esc_or_empty = alt((esc, tag("")));
    delimited(tag("\""), esc_or_empty, tag("\""))(input)
}
