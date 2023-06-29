#![cfg(test)]
use super::*;
use rust_decimal_macros::dec;
use test_case::test_case;

// #[test_case("123.45", Some(CompoundExpr::PerUnit(Expr::Value(dec!(123.45)))))]
// #[test_case("789.45 #", Some(CompoundExpr::PerUnit(Expr::Value(dec!(789.45)))))]
// #[test_case("# 123.45", Some(CompoundExpr::Total(Expr::Value(dec!(123.45)))))]
// fn test_compound_expr(s: &str, expected: Option<CompoundExpr>) {
//     match compound_expr().parse(s).into_result() {
//         Ok(result) => assert_eq!(result, expected.unwrap()),
//         Err(e) => {
//             println!("{:?}", e);
//             assert!(expected.is_none());
//         }
//     }
// }
