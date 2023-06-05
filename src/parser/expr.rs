// based on from https://github.com/rust-bakery/nom/blob/main/tests/arithmetic_ast.rs, with thanks and quite a few changes 😊
//
// the original is part of nom, which is MIT licensed, and the fragment used here is not a "substantial portions" of nom

use std::fmt::Debug;

use super::{RawDecimalExpr, Span};
use nom::{
    branch::alt,
    character::complete::{digit1, multispace0, satisfy},
    combinator::{map, map_res, opt, recognize},
    multi::many0,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use nom_supreme::tag::complete::tag as sym;
use nom_tracable::tracable_parser;
use std::str::FromStr;

#[cfg(test)]
use {rust_decimal::Decimal, rust_decimal_macros::dec};

#[cfg(test)]
use test_case::test_case;

#[derive(Debug)]
pub enum Oper {
    Add,
    Sub,
    Mul,
    Div,
}

fn parens(i: Span) -> IResult<Span, RawDecimalExpr> {
    delimited(
        multispace0,
        delimited(
            sym("("),
            map(expr, |e| RawDecimalExpr::Paren(Box::new(e))),
            sym(")"),
        ),
        multispace0,
    )(i)
}

fn factor(i: Span) -> IResult<Span, RawDecimalExpr> {
    alt((value, parens))(i)
}

fn fold_exprs(initial: RawDecimalExpr, remainder: Vec<(Oper, RawDecimalExpr)>) -> RawDecimalExpr {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (oper, expr) = pair;
        match oper {
            Oper::Add => RawDecimalExpr::Add(Box::new(acc), Box::new(expr)),
            Oper::Sub => RawDecimalExpr::Sub(Box::new(acc), Box::new(expr)),
            Oper::Mul => RawDecimalExpr::Mul(Box::new(acc), Box::new(expr)),
            Oper::Div => RawDecimalExpr::Div(Box::new(acc), Box::new(expr)),
        }
    })
}

fn term(i: Span) -> IResult<Span, RawDecimalExpr> {
    let (i, initial) = factor(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, mul) = preceded(sym("*"), factor)(i)?;
            Ok((i, (Oper::Mul, mul)))
        },
        |i| {
            let (i, div) = preceded(sym("/"), factor)(i)?;
            Ok((i, (Oper::Div, div)))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

pub fn expr(i: Span) -> IResult<Span, RawDecimalExpr> {
    let (i, initial) = term(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, add) = preceded(sym("+"), term)(i)?;
            Ok((i, (Oper::Add, add)))
        },
        |i| {
            let (i, sub) = preceded(sym("-"), term)(i)?;
            Ok((i, (Oper::Sub, sub)))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

/// Match a single digit
fn digit(i: Span) -> IResult<Span, char> {
    satisfy(|c| c.is_ascii_digit())(i)
}

/// Match a number with optional thousands separators and optional decimal point and fractional part
#[tracable_parser]
pub fn value(i: Span) -> IResult<Span, RawDecimalExpr> {
    map(
        map_res(
            delimited(
                multispace0,
                recognize(tuple((
                    opt(sym("-")),
                    digit1,
                    many0(tuple((sym(","), digit, digit, digit))),
                    opt(tuple((sym("."), digit1))),
                ))),
                multispace0,
            ),
            |s: Span| {
                let mut without_commas = s.to_string();
                without_commas.retain(|c| c != ',');
                FromStr::from_str(&without_commas)
            },
        ),
        RawDecimalExpr::Value,
    )(i)
}

#[cfg(test)]
#[test_case("123,456,789", dec!(123456789))]
#[test_case("-123,456,789.12", dec!(-123456789.12))]
fn value_test(s: &str, expected: Decimal) {
    use RawDecimalExpr::Value;

    match value(s.into()) {
        Ok((_, Value(d))) => assert_eq!(d, expected),
        _ => panic!("oops"),
    }
}

#[cfg(test)]
fn format_span_expr((i, x): (Span, RawDecimalExpr)) -> (&str, String) {
    (*i.fragment(), format!("{:?}", x))
}

#[test]
fn factor_test() {
    assert_eq!(
        factor("  3  ".into()).map(format_span_expr),
        Ok(("", String::from("3")))
    );
}

#[test]
fn term_test() {
    assert_eq!(
        term(" 3 *  5   ".into()).map(format_span_expr),
        Ok(("", String::from("(3 * 5)")))
    );
}

#[test]
fn expr_test() {
    assert_eq!(
        expr(" 1 + 2 *  3 ".into()).map(format_span_expr),
        Ok(("", String::from("(1 + (2 * 3))")))
    );
    assert_eq!(
        expr(" 1 + 2 *  3 / 4 - 5 ".into()).map(format_span_expr),
        Ok(("", String::from("((1 + ((2 * 3) / 4)) - 5)")))
    );
    assert_eq!(
        expr(" 72 / 2 / 3 ".into()).map(format_span_expr),
        Ok(("", String::from("((72 / 2) / 3)")))
    );
    assert_eq!(
        expr(" 10 - 1 ".into()).map(format_span_expr),
        Ok(("", String::from("(10 - 1)")))
    );
    assert_eq!(
        expr(" 10 - -2 ".into()).map(format_span_expr),
        Ok(("", String::from("(10 - -2)")))
    );
}

#[test]
fn parens_test() {
    assert_eq!(
        expr(" ( 1 + 2 ) *  3 ".into()).map(format_span_expr),
        Ok(("", String::from("([(1 + 2)] * 3)")))
    );
}
