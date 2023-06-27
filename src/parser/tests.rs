#![cfg(test)]
use super::*;
use rust_decimal_macros::dec;
use test_case::test_case;

#[test_case("GBP", Some("GBP"))]
#[test_case("AAPL", Some("AAPL"))] // stock
#[test_case("V", Some("V"))] // single-character stock
#[test_case("NT.TO", Some("NT.TO"))] // stock on another market
#[test_case("TLT_040921C144", Some("TLT_040921C144"))] // equity option
#[test_case("/6J", Some("/6J"))] // currency futures
#[test_case("/6'J", Some("/6'J"))] // currency futures
#[test_case("/NQH21", Some("/NQH21"))] // commodity futures
#[test_case("/NQH21-EXT", Some("/NQH21-EXT"))] // commodity futures
#[test_case("/NQH21_QNEG21C13100", Some("/NQH21_QNEG21C13100"))] // futures option
#[test_case("/6.3", None)]
#[test_case("CAC_", None)]
#[test_case("abc", None)]
#[test_case("", None)]
fn test_currency(s: &str, expected: Option<&str>) {
    let expected = expected.map(|s| s.parse::<Currency>().unwrap());

    assert_eq!(currency().parse(s).into_output(), expected);
}

#[test_case("123.45", Some(CompoundExpr::PerUnit(Expr::Value(dec!(123.45)))))]
#[test_case("789.45 #", Some(CompoundExpr::PerUnit(Expr::Value(dec!(789.45)))))]
#[test_case("# 123.45", Some(CompoundExpr::Total(Expr::Value(dec!(123.45)))))]
fn test_compound_expr(s: &str, expected: Option<CompoundExpr>) {
    match compound_expr().parse(s).into_result() {
        Ok(result) => assert_eq!(result, expected.unwrap()),
        Err(e) => {
            println!("{:?}", e);
            assert!(expected.is_none());
        }
    }
}
