#[allow(dead_code)]
use crate::frontend::ast::*;
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take_while},
    character::{
        complete::{alpha1, alphanumeric1, anychar, char, digit1, multispace0, one_of, space0},
        is_alphabetic, is_alphanumeric,
    },
    combinator::{map, map_res, not, opt, recognize, value},
    error::ParseError,
    multi::{many0, many1, separated_list0, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use nom_supreme::error::ErrorTree;

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    terminated(inner, multispace0)
}

fn span<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, WrapSpan<'a, O>, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    move |input| {
        let (rest, ret) = inner(input)?;
        Ok((rest, WrapSpan(&input[..input.len() - rest.len()], ret)))
    }
}

pub fn parse(input: &str) -> IResult<&str, Program<&str>> {
    parse_program(input)
}

fn parse_program(input: &str) -> IResult<&str, Program<&str>> {
    map(
        tuple((many0(span(parse_func)), parse_stats)),
        |(funcs, stats)| Program(funcs, stats),
    )(input)
}

fn parse_func(input: &str) -> IResult<&str, Function<&str>> {
    map(
        tuple((
            parse_type,
            parse_ident,
            delimited(
                char('('),
                separated_list0(char(','), span(parse_param)),
                char(')'),
            ),
            delimited(tag("is"), parse_stats, tag("end")),
        )),
        |(t, id, params, block)| Function(t, id.to_string(), params, block),
    )(input)
}

fn parse_stats(input: &str) -> IResult<&str, Vec<WrapSpan<Stat<&str>>>> {
    separated_list1(char(';'), span(parse_stat))(input)
}

fn parse_stat(input: &str) -> IResult<&str, Stat<&str>> {
    let parse_while = delimited(
        tag("while"),
        separated_pair(parse_expr, tag("do"), parse_stats),
        tag("done"),
    );
    let parse_if = delimited(
        tag("if"),
        tuple((
            separated_pair(parse_expr, tag("then"), parse_stats),
            preceded(tag("else"), parse_stats),
        )),
        tag("fi"),
    );

    alt((
        map(tag("skip"), |_| Stat::Skip),
        map(
            separated_pair(tuple((parse_type, parse_ident)), tag("="), parse_rhs),
            |((t, id), rhs)| Stat::Def(t, id, rhs),
        ),
        map(
            separated_pair(parse_lhs, tag("="), parse_rhs),
            |(lhs, rhs)| Stat::Assign(lhs, rhs),
        ),
        preceded(tag("read"), map(parse_lhs, |lhs| Stat::Read(lhs))),
        preceded(tag("free"), map(parse_expr, |e| Stat::Free(e))),
        preceded(tag("return"), map(parse_expr, |e| Stat::Return(e))),
        preceded(tag("exit"), map(parse_expr, |e| Stat::Exit(e))),
        preceded(tag("print"), map(parse_expr, |e| Stat::Print(e))),
        preceded(tag("println"), map(parse_expr, |e| Stat::PrintLn(e))),
        map(parse_if, |((e, st), sf)| Stat::If(e, st, sf)),
        map(parse_while, |(e, s)| Stat::While(e, s)),
        delimited(
            tag("begin"),
            map(parse_stats, |s| Stat::Block(s)),
            tag("end"),
        ),
    ))(input)
}

fn parse_ident(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn parse_param(input: &str) -> IResult<&str, Param<&str>> {
    map(tuple((parse_type, parse_ident)), |(t, ident)| {
        Param(t, ident)
    })(input)
}

fn parse_array_desc0(input: &str) -> IResult<&str, usize> {
    map(many0(pair(char('['), char(']'))), |vec| vec.len())(input)
}

fn parse_array_desc1(input: &str) -> IResult<&str, usize> {
    map(many1(pair(char('['), char(']'))), |vec| vec.len())(input)
}

fn parse_type(input: &str) -> IResult<&str, Type> {
    map(
        pair(alt((parse_base_type, parse_pair_type)), parse_array_desc0),
        |(t, arr_depth)| match arr_depth {
            0 => t,
            d => Type::Array(box t, d),
        },
    )(input)
}

fn parse_pair_type(input: &str) -> IResult<&str, Type> {
    map(
        preceded(
            tag("pair"),
            delimited(
                char('('),
                separated_pair(parse_pair_elem_type, char(','), parse_pair_elem_type),
                char(')'),
            ),
        ),
        |(f, s)| Type::Pair(box f, box s),
    )(input)
}

fn parse_pair_elem_type(input: &str) -> IResult<&str, Type> {
    alt((
        value(Type::GenericPair, tag("pair")),
        map(tuple((parse_pair_type, parse_array_desc1)), |(t, n)| {
            Type::Array(box t, n)
        }),
        map(
            tuple((parse_base_type, parse_array_desc0)),
            |(t, n)| match n {
                0 => t,
                n => Type::Array(box t, n),
            },
        ),
    ))(input)
}

fn parse_base_type(input: &str) -> IResult<&str, Type> {
    alt((
        value(Type::Int, tag("int")),
        value(Type::Bool, tag("bool")),
        value(Type::Char, tag("char")),
        value(Type::String, tag("string")),
    ))(input)
}

fn parse_lhs(input: &str) -> IResult<&str, AssignLhs<&str>> {
    alt((
        map(parse_ident, |id| AssignLhs::Var(id)),
        map(parse_array_elem, |(id, exprs)| {
            AssignLhs::ArrayElem(id, exprs)
        }),
        map(preceded(tag("fst"), parse_expr), |pair| AssignLhs::PairFst(pair)),
        map(preceded(tag("snd"), parse_expr), |pair| AssignLhs::PairSnd(pair)),
    ))(input)
}

fn parse_array_elem(input: &str) -> IResult<&str, (&str, Vec<WrapSpan<Expr<&str>>>)> {
    tuple((
        parse_ident,
        many1(delimited(char('['), parse_expr, char(']'))),
    ))(input)
}


fn parse_rhs(input: &str) -> IResult<&str, AssignRhs<&str>> {
    let new_pair = delimited(
        tuple((tag("newpair"), char('('))),
        separated_pair(parse_expr, char(','), parse_expr),
        char(')'),
    );
    let call = delimited(
        tag("call"),
        separated_pair(
            parse_ident,
            char('('),
            separated_list0(char(','), parse_expr),
        ),
        char(')'),
    );
    let array_liter = delimited(char('['), separated_list0(char(','), parse_expr), char(']'));
    alt((
        map(parse_expr, |e| AssignRhs::Expr(e)),
        map(array_liter, |arr| AssignRhs::Array(arr)),
        map(new_pair, |(e1, e2)| AssignRhs::NewPair(e1, e2)),
        map(preceded(tag("fst"), parse_expr), |pair| AssignRhs::PairFst(pair)),
        map(preceded(tag("snd"), parse_expr), |pair| AssignRhs::PairSnd(pair)),
        map(call, |(id, es)| AssignRhs::Call(id.to_string(), es)),
    ))(input)
}

fn parse_expr(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>> {
    parse_expr_and(input)
}

fn fold_expr<'a>(
    expr: WrapSpan<'a, Expr<'a, &'a str>>,
    rem: Vec<(&'a str, WrapSpan<'a, Expr<'a, &'a str>>)>,
) -> WrapSpan<'a, Expr<'a, &'a str>> {
    rem.into_iter()
        .fold(expr, |acc, val| parse_bin_op(val, acc))
}

fn parse_expr_or(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>> {
    let (input, sub_exp) = parse_expr_and(input)?;
    let (input, exprs) = many0(tuple((tag("||"), parse_expr_and)))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_expr_and(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>> {
    let (input, sub_exp) = parse_expr_eq(input)?;
    let (input, exprs) = many0(tuple((tag("&&"), parse_expr_eq)))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_expr_eq(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>> {
    let (input, sub_exp) = parse_expr_cmp(input)?;
    let (input, exprs) = many0(tuple((alt((tag("=="), tag("!="))), parse_expr_cmp)))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_expr_cmp(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>> {
    let (input, sub_exp) = parse_expr_plus(input)?;
    let (input, exprs) = many0(tuple((
        alt((tag(">="), tag("<="), tag(">"), tag("<"))),
        parse_expr_plus,
    )))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_expr_plus(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>> {
    let (input, sub_exp) = parse_expr_mult(input)?;
    let (input, exprs) = many0(tuple((alt((tag("+"), tag("-"))), parse_expr_mult)))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_expr_mult(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>> {
    let (input, sub_exp) = span(parse_expr_atom)(input)?;
    let (input, exprs) = many0(tuple((alt((tag("*"), tag("/"), tag("%"))), span(parse_expr_atom))))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_literal_keywords(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>> {
    let (input, WrapSpan(span, s)) = span(parse_ident)(input)?;
    match s {
        "true" | "false" => Ok((input, WrapSpan(span, Expr::Bool(s.parse::<bool>().unwrap())))),
        "null" => Ok((input, WrapSpan(span, Expr::Null))),
        "!" | "-" | "len" | "ord" | "chr" => {
            let (input, e) = parse_expr(input)?;
            Ok((input, WrapSpan(span, Expr::UnOp(parse_unary(&s), Box::new(e)))))
        }
        _ => Ok((input, WrapSpan(span, Expr::Var(s)))),
    }
}

fn parse_expr_atom(input: &str) -> IResult<&str, Expr<&str>> {
    alt((
        map(
            recognize(pair(
                opt(alt((char('-'), char('+')))),
                many1(one_of("0123456789")),
            )),
            |n: &str| Expr::Int(n.parse::<i32>().unwrap()),
        ),
        map(delimited(char('\''), anychar, char('\'')), |c| {
            Expr::Char(c)
        }),
        map(
            delimited(char('\"'), is_not("\""), char('\"')),
            |s: &str| Expr::String(s.to_string()),
        ),
        map(parse_array_elem, |(id, e)| Expr::ArrayElem(id, e)),
        map(parse_literal_keywords, |e| e.1),
        map(delimited(char('('), parse_expr, char(')')), |e| e.1),
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
) -> WrapSpan<'a, Expr<'a, &'a str>> {
    let (op, expr2) = tup;
    // input[..input.len() - rest.len()];
    WrapSpan(
        &expr1.0[..expr1.0.len() - expr2.0.len()],
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
