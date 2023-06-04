// based on from https://github.com/rust-bakery/nom/blob/main/tests/arithmetic_ast.rs, with thanks and quite a few changes 😊

use std::fmt;
use std::fmt::{Debug, Display, Formatter};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0 as multispace,
    combinator::map,
    multi::many0,
    sequence::{delimited, preceded},
    IResult,
};

use rust_decimal::Decimal;

pub enum Expr {
    Value(Decimal),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
}

#[derive(Debug)]
pub enum Oper {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for Expr {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Value(val) => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "{} + {}", left, right),
            Sub(ref left, ref right) => write!(format, "{} - {}", left, right),
            Mul(ref left, ref right) => write!(format, "{} * {}", left, right),
            Div(ref left, ref right) => write!(format, "{} / {}", left, right),
            Paren(ref expr) => write!(format, "({})", expr),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Value(val) => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "({:?} + {:?})", left, right),
            Sub(ref left, ref right) => write!(format, "({:?} - {:?})", left, right),
            Mul(ref left, ref right) => write!(format, "({:?} * {:?})", left, right),
            Div(ref left, ref right) => write!(format, "({:?} / {:?})", left, right),
            Paren(ref expr) => write!(format, "[{:?}]", expr),
        }
    }
}

fn parens(i: Span) -> IResult<Span, Expr> {
    delimited(
        multispace,
        delimited(tag("("), map(expr, |e| Expr::Paren(Box::new(e))), tag(")")),
        multispace,
    )(i)
}

fn factor(i: Span) -> IResult<Span, Expr> {
    alt((value, parens))(i)
}

fn fold_exprs(initial: Expr, remainder: Vec<(Oper, Expr)>) -> Expr {
    remainder.into_iter().fold(initial, |acc, pair| {
        let (oper, expr) = pair;
        match oper {
            Oper::Add => Expr::Add(Box::new(acc), Box::new(expr)),
            Oper::Sub => Expr::Sub(Box::new(acc), Box::new(expr)),
            Oper::Mul => Expr::Mul(Box::new(acc), Box::new(expr)),
            Oper::Div => Expr::Div(Box::new(acc), Box::new(expr)),
        }
    })
}

fn term(i: Span) -> IResult<Span, Expr> {
    let (i, initial) = factor(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, mul) = preceded(tag("*"), factor)(i)?;
            Ok((i, (Oper::Mul, mul)))
        },
        |i| {
            let (i, div) = preceded(tag("/"), factor)(i)?;
            Ok((i, (Oper::Div, div)))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

pub fn expr(i: Span) -> IResult<Span, Expr> {
    let (i, initial) = term(i)?;
    let (i, remainder) = many0(alt((
        |i| {
            let (i, add) = preceded(tag("+"), term)(i)?;
            Ok((i, (Oper::Add, add)))
        },
        |i| {
            let (i, sub) = preceded(tag("-"), term)(i)?;
            Ok((i, (Oper::Sub, sub)))
        },
    )))(i)?;

    Ok((i, fold_exprs(initial, remainder)))
}

#[cfg(test)]
fn format_span_expr((i, x): (Span, Expr)) -> (&str, String) {
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
}

#[test]
fn parens_test() {
    assert_eq!(
        expr(" ( 1 + 2 ) *  3 ".into()).map(format_span_expr),
        Ok(("", String::from("([(1 + 2)] * 3)")))
    );
}

// enhancements for decimal value
mod decimal;
use decimal::value;

use super::Span;
