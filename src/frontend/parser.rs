#![allow(dead_code)]
use crate::frontend::ast::*;
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, take_while, take_until, escaped_transform, escaped},
    character::{
        complete::{
            alpha1, alphanumeric1, anychar, char, digit1, multispace0, multispace1,
            not_line_ending, one_of, space0, none_of,
        },
        is_alphabetic, is_alphanumeric,
    },
    combinator::{map, map_res, not, opt, recognize, value},
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

#[cfg(test)]
mod tests;

fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, ErrorTree<&str>>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, ErrorTree<&str>>,
{
    terminated(inner, many0(alt((comments, multispace1))))
}

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

fn comments(input: &str) -> IResult<&str, &str, ErrorTree<&str>> {
    recognize(pair(char('#'), alt((is_not("\n\r"), tag("")))))(input)
}

fn parse_program(input: &str) -> IResult<&str, Program<&str>, ErrorTree<&str>> {
    map(
        delimited(
            tuple((ws(tag("")), ws(tag("begin")))),
            tuple((many0(span(parse_func)), parse_stats)),
            ws(tag("end")),
        ),
        |(funcs, stats)| Program(funcs, stats),
    )(input)
}

fn parse_func(input: &str) -> IResult<&str, Function<&str>, ErrorTree<&str>> {
    map(
        tuple((
            parse_type,
            parse_ident,
            delimited(
                ws(char('(')),
                separated_list0(ws(char(',')), span(parse_param)),
                ws(char(')')),
            ),
            delimited(ws(tag("is")), parse_stats, ws(tag("end"))),
        )),
        |(t, id, params, block)| Function(t, id, params, block),
    )(input)
}

fn parse_stats(input: &str) -> IResult<&str, Vec<WrapSpan<Stat<&str>>>, ErrorTree<&str>> {
    separated_list1(ws(char(';')), span(parse_stat))(input)
}

fn parse_stat(input: &str) -> IResult<&str, Stat<&str>, ErrorTree<&str>> {
    let parse_while = delimited(
        ws(tag("while")),
        separated_pair(parse_expr, ws(tag("do")), parse_stats),
        ws(tag("done")),
    );
    let parse_if = delimited(
        ws(tag("if")),
        tuple((
            separated_pair(parse_expr, ws(tag("then")), parse_stats),
            preceded(ws(tag("else")), parse_stats),
        )),
        ws(tag("fi")),
    );

    context(
        "statement",
        alt((
            map(ws(tag("skip")), |_| Stat::Skip),
            map(
                separated_pair(tuple((parse_type, parse_ident)), ws(tag("=")), parse_rhs),
                |((t, id), rhs)| Stat::Def(t, id, rhs),
            ),
            context(
                "assign",
                map(
                    separated_pair(parse_lhs, ws(tag("=")), parse_rhs),
                    |(lhs, rhs)| Stat::Assign(lhs, rhs),
                ),
            ),
            preceded(ws(tag("read")), map(parse_lhs, Stat::Read)),
            preceded(ws(tag("free")), map(parse_expr, Stat::Free)),
            preceded(ws(tag("return")), map(parse_expr, Stat::Return)),
            preceded(ws(tag("exit")), map(parse_expr, Stat::Exit)),
            preceded(ws(tag("println")), map(parse_expr, Stat::PrintLn)),
            preceded(ws(tag("print")), map(parse_expr, Stat::Print)),
            preceded(ws(tag("return")), map(parse_expr, Stat::Return)),
            map(parse_if, |((e, st), sf)| Stat::If(e, st, sf)),
            map(parse_while, |(e, s)| Stat::While(e, s)),
            delimited(
                ws(tag("begin")),
                map(parse_stats, Stat::Block),
                ws(tag("end")),
            ),
        )),
    )(input)
}

fn parse_ident(input: &str) -> IResult<&str, &str, ErrorTree<&str>> {
    ws(recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    )))(input)
}

fn parse_param(input: &str) -> IResult<&str, Param<&str>, ErrorTree<&str>> {
    map(tuple((parse_type, parse_ident)), |(t, ident)| {
        Param(t, ident)
    })(input)
}

fn parse_array_desc0(input: &str) -> IResult<&str, usize, ErrorTree<&str>> {
    map(many0(pair(ws(char('[')), ws(char(']')))), |vec| vec.len())(input)
}

fn parse_array_desc1(input: &str) -> IResult<&str, usize, ErrorTree<&str>> {
    map(many1(pair(ws(char('[')), ws(char(']')))), |vec| vec.len())(input)
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
            ws(tag("pair")),
            delimited(
                ws(char('(')),
                separated_pair(parse_pair_elem_type, ws(char(',')), parse_pair_elem_type),
                ws(char(')')),
            ),
        ),
        |(f, s)| Type::Pair(box f, box s),
    )(input)
}

fn parse_pair_elem_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    alt((
        value(Type::Pair(box Type::Any, box Type::Any), ws(tag("pair"))),
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

fn parse_base_type(input: &str) -> IResult<&str, Type, ErrorTree<&str>> {
    ws(alt((
        value(Type::Int, ws(tag("int"))),
        value(Type::Bool, ws(tag("bool"))),
        value(Type::Char, ws(tag("char"))),
        value(Type::String, ws(tag("string"))),
    )))(input)
}

fn parse_lhs(input: &str) -> IResult<&str, AssignLhs<&str>, ErrorTree<&str>> {
    alt((
        map(parse_array_elem, |(id, exprs)| {
            AssignLhs::ArrayElem(id, exprs)
        }),
        map(preceded(ws(tag("fst")), parse_expr), |pair| {
            AssignLhs::PairFst(pair)
        }),
        map(preceded(ws(tag("snd")), parse_expr), |pair| {
            AssignLhs::PairSnd(pair)
        }),
        map(parse_ident, AssignLhs::Var),
    ))(input)
}

fn parse_array_elem(
    input: &str,
) -> IResult<&str, (&str, Vec<WrapSpan<Expr<&str>>>), ErrorTree<&str>> {
    tuple((
        parse_ident,
        many1(delimited(ws(char('[')), parse_expr, ws(char(']')))),
    ))(input)
}

fn parse_rhs(input: &str) -> IResult<&str, AssignRhs<&str>, ErrorTree<&str>> {
    let call = delimited(
        ws(tag("call")),
        separated_pair(
            parse_ident,
            ws(char('(')),
            separated_list0(ws(char(',')), parse_expr),
        ),
        ws(char(')')),
    );
    let array_liter = delimited(
        ws(char('[')),
        separated_list0(ws(char(',')), parse_expr),
        ws(char(']')),
    );
    alt((
        map(call, |(id, es)| AssignRhs::Call(id, es)),
        map(parse_expr, AssignRhs::Expr),
        map(array_liter, AssignRhs::Array),
        
    ))(input)
}

fn parse_expr(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    parse_expr_or(input)
}

fn fold_expr<'a>(
    expr: WrapSpan<'a, Expr<'a, &'a str>>,
    rem: Vec<(&'a str, WrapSpan<'a, Expr<'a, &'a str>>)>,
) -> WrapSpan<'a, Expr<'a, &'a str>> {
    rem.into_iter()
        .rfold(expr, |acc, val| parse_bin_op(val, acc))
}

fn parse_expr_or(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (input, sub_exp) = parse_expr_and(input)?;
    let (input, exprs) = many0(tuple((ws(tag("||")), parse_expr_and)))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_expr_and(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (input, sub_exp) = parse_expr_eq(input)?;
    let (input, exprs) = many0(tuple((ws(tag("&&")), parse_expr_eq)))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_expr_eq(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (input, sub_exp) = parse_expr_cmp(input)?;
    let (input, exprs) =
        many0(tuple((alt((ws(tag("==")), ws(tag("!=")))), parse_expr_cmp)))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_expr_cmp(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (input, sub_exp) = parse_expr_plus(input)?;
    let (input, exprs) = many0(tuple((
        alt((ws(tag(">=")), ws(tag("<=")), ws(tag(">")), ws(tag("<")))),
        parse_expr_plus,
    )))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_expr_plus(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (input, sub_exp) = parse_expr_mult(input)?;
    let (input, exprs) = many0(tuple((alt((ws(tag("+")), ws(tag("-")))), parse_expr_mult)))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_expr_mult(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (input, sub_exp) = span(parse_expr_atom)(input)?;
    let (input, exprs) = many0(tuple((
        alt((ws(tag("*")), ws(tag("/")), ws(tag("%")))),
        span(parse_expr_atom),
    )))(input)?;
    Ok((input, fold_expr(sub_exp, exprs)))
}

fn parse_literal_keywords(input: &str) -> IResult<&str, WrapSpan<Expr<&str>>, ErrorTree<&str>> {
    let (input, WrapSpan(span, s)) = ws(span(alt((tag("!"), tag("-"), parse_ident))))(input)?;
    match s {
        "true" | "false" => Ok((
            input,
            WrapSpan(span, Expr::Bool(s.parse::<bool>().unwrap())),
        )),
        "null" => Ok((input, WrapSpan(span, Expr::Null))),
        "!" | "-" | "len" | "ord" | "chr" | "fst" | "snd" => {
            let (input, e) = parse_expr(input)?;
            Ok((
                input,
                WrapSpan(span, Expr::UnOp(parse_unary(s), Box::new(e))),
            ))
        }
        _ => Ok((input, WrapSpan(span, Expr::Var(s)))),
    }
}


fn _escaped<'a, F: 'a>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, String, ErrorTree<&'a str>>
where
    F: FnMut(&'a str) -> IResult<&'a str, &'a str, ErrorTree<&str>>,
{
    escaped_transform(
      inner,
      '\\',
      alt((
        value('\0', tag("0")),
        value('\x08', tag("b")),
        value('\t', tag("t")),
        value('\n', tag("n")),
        value('\x0c', tag("f")),
        value('\r', tag("r")),
        value('\"', tag("\"")),
        // value('\'', tag("'")),
        value('\\', tag("\\")),
      ))
    )
  }

fn parse_expr_atom(input: &str) -> IResult<&str, Expr<&str>, ErrorTree<&str>> {
    let new_pair = delimited(
        pair(ws(tag("newpair")), ws(char('('))),
        separated_pair(parse_expr, ws(char(',')), parse_expr),
        ws(char(')')),
    );
    alt((
        map(new_pair, |(left, right)| {
            Expr::BinOp(box left, BinOp::Newpair, box right)
        }),
        ws(map(
            recognize(pair(
                opt(alt((char('-'), char('+')))),
                many1(one_of("0123456789")),
            )),
            |n: &str| Expr::Int(n.parse::<i32>().unwrap()),
        )),
        map(
            ws(parse_quoted),
            |s| Expr::Char(s.chars().next().unwrap_or_default()),
        ),
        map(
            ws(parse_str),
            |s| Expr::String(s.to_string()),
        ),
        map(parse_array_elem, |(id, e)| Expr::ArrayElem(id, e)),
        map(parse_literal_keywords, |e| e.1),
        map(delimited(ws(char('(')), parse_expr, ws(char(')'))), |e| e.1),
    ))(input)
}

fn parse_unary(op: &str) -> UnOp {
    match op {
        "!" => UnOp::Neg,
        "-" => UnOp::Minus,
        "len" => UnOp::Len,
        "ord" => UnOp::Ord,
        "chr" => UnOp::Chr,
        "fst" => UnOp::Fst,
        "snd" => UnOp::Snd,
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
        &"",
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

fn parse_quoted(input: &str) -> IResult<&str, &str, ErrorTree<&str>>  {
    let esc = escaped(none_of("\\\'\""), '\\', one_of("'0nt\"b\\rf"));
    let esc_or_empty = alt((esc, tag("")));
    delimited(tag("'"), esc_or_empty, tag("'"))(input)
}

fn parse_str(input: &str) -> IResult<&str, &str, ErrorTree<&str>>  {
    let esc = escaped(none_of("\\\"\'"), '\\', one_of("'0nt\"b\\rf"));
    let esc_or_empty = alt((esc, tag("")));
    delimited(tag("\""), esc_or_empty, tag("\""))(input)
}