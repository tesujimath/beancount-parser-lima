#![cfg(test)]
use super::*;
use rust_decimal_macros::dec;
use test_case::test_case;

#[test_case("123.45", CompoundExpr::PerUnit(Expr::Value(dec!(123.45))))]
#[test_case("789.45 #", CompoundExpr::PerUnit(Expr::Value(dec!(789.45))))]
#[test_case("# 123.45", CompoundExpr::Total(Expr::Value(dec!(123.45))))]
fn test_compound_expr(s: &str, expected: CompoundExpr) {
    let tokens = tokenize(s);
    let spanned = tokens.spanned(end_of_input(s));

    let result = compound_expr().parse(spanned).into_result();

    assert_eq!(result, Ok(expected));
}
