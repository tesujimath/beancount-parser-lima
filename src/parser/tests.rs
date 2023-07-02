#![cfg(test)]
use super::*;
use rust_decimal_macros::dec;
use test_case::test_case;

#[test_case("GBP", CompoundAmount::BareCurrency(&Currency("GBP")))]
#[test_case("456.78", CompoundAmount::BareAmount(CompoundExpr::PerUnit(Expr::Value(dec!(456.78)))))]
#[test_case("# 1456.98", CompoundAmount::BareAmount(CompoundExpr::Total(Expr::Value(dec!(1456.98)))))]
#[test_case("456.78 NZD", CompoundAmount::CurrencyAmount(CompoundExpr::PerUnit(Expr::Value(dec!(456.78))), &Currency("NZD")))]
#[test_case("# 1456.98 USD", CompoundAmount::CurrencyAmount(CompoundExpr::Total(Expr::Value(dec!(1456.98))), &Currency("USD")))]
fn test_compound_amount(s: &str, expected: CompoundAmount) {
    let tokens = tokenize(s);
    let spanned = tokens.spanned(end_of_input(s));

    let result = compound_amount().parse(spanned).into_result();

    assert_eq!(result, Ok(expected));
}

#[test_case("123.45", CompoundExpr::PerUnit(Expr::Value(dec!(123.45))))]
#[test_case("789.45 #", CompoundExpr::PerUnit(Expr::Value(dec!(789.45))))]
#[test_case("# 123.45", CompoundExpr::Total(Expr::Value(dec!(123.45))))]
fn test_compound_expr(s: &str, expected: CompoundExpr) {
    let tokens = tokenize(s);
    let spanned = tokens.spanned(end_of_input(s));

    let result = compound_expr().parse(spanned).into_result();

    assert_eq!(result, Ok(expected));
}
