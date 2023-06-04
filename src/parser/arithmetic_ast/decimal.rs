use std::str::FromStr;

use nom::{
    character::complete::{digit1, multispace0 as multispace, satisfy},
    combinator::{map, map_res, opt, recognize},
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};

use nom_supreme::tag::complete::tag as sym;

use nom_tracable::tracable_parser;

#[cfg(test)]
use rust_decimal_macros::dec;

use super::super::Span;
use super::Expr;

/// Match a single digit
fn digit(i: Span) -> IResult<Span, char> {
    satisfy(|c| c.is_ascii_digit())(i)
}

/// Match a number with optional thousands separators and optional decimal point and fractional part
#[tracable_parser]
pub fn value(i: Span) -> IResult<Span, Expr> {
    map(
        map_res(
            delimited(
                multispace,
                recognize(tuple((
                    digit1,
                    many0(tuple((sym(","), digit, digit, digit))),
                    opt(tuple((sym("."), digit1))),
                ))),
                multispace,
            ),
            |s: Span| {
                let mut without_commas = s.to_string();
                without_commas.retain(|c| c != ',');
                FromStr::from_str(&without_commas)
            },
        ),
        Expr::Value,
    )(i)
}

#[test]
fn value_test() {
    use Expr::Value;
    match value("123,456,789".into()) {
        Ok((_, Value(d))) => assert_eq!(d, dec!(123456789)),
        _ => panic!("oops"),
    }
}
