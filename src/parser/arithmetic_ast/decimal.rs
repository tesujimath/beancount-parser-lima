use std::str::FromStr;

use nom::{
    bytes::complete::tag,
    character::complete::{digit1 as digit, multispace0 as multispace},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many1},
    sequence::{delimited, tuple},
    IResult,
};

#[cfg(test)]
use rust_decimal_macros::dec;

use super::Expr;

/// Match a number with optional thousands separators and optional decimal point and fractional part
pub fn value(i: &str) -> IResult<&str, Expr> {
    map(
        map_res(
            delimited(
                multispace,
                recognize(tuple((
                    many1(digit),
                    many0(tuple((tag(","), digit, digit, digit))),
                    opt(tuple((tag("."), many1(digit)))),
                ))),
                multispace,
            ),
            |s: &str| {
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
    match value("123,456,789") {
        Ok((_, Value(d))) => assert_eq!(d, dec!(123456789)),
        _ => panic!("oops"),
    }
}
