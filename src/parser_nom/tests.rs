#![cfg(test)]
use super::*;
use either::Either::{self, Left, Right};
use rust_decimal_macros::dec;
use test_case::test_case;

#[test_case("Assets:Car", Some((AccountType::Assets, vec!["Car"])))]
#[test_case("Assets:oops", None)]
#[test_case("Assets:Bike:oops", Some((AccountType::Assets, vec!["Bike"])))]
fn test_account(s: &str, expected_raw: Option<(AccountType, Vec<&str>)>) {
    let expected = expected_raw.map(|(account_type, names)| Account {
        account_type,
        names: NonEmpty::collect(names.iter().map(|name| AccountName(name.to_string()))).unwrap(),
    });

    match account(s.into()) {
        Ok((_, result)) => assert_eq!(result, expected.unwrap()),
        Err(e) => {
            println!("{:?}", e);
            assert!(expected.is_none());
        }
    }
}

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
#[test_case("A?=.-BJ", Some("A"))] // parser takes what it can
#[test_case("", None)]
fn test_currency(s: &str, expected: Option<&str>) {
    match (currency(s.into()), expected) {
        (Ok((_, actual)), Some(expected)) => assert_eq!(actual, Currency(expected.to_owned())),
        (Err(_), None) => (),
        (Err(actual), Some(_)) => panic!("unexpected failure: {}", actual),
        (Ok((_, actual)), None) => panic!("unexpected success: {}", actual),
    }
}

#[test_case("123.45", Some(CompoundExpr::PerUnit(Expr::Value(dec!(123.45)))))]
#[test_case("# 123.45", Some(CompoundExpr::Total(Expr::Value(dec!(123.45)))))]
fn test_compound_expr(s: &str, expected: Option<CompoundExpr>) {
    match (compound_expr(s.into()), expected) {
        (Ok((_, actual)), Some(expected)) => assert_eq!(actual, expected),
        (Err(_), None) => (),
        (Err(actual), Some(_)) => panic!("unexpected failure: {}", actual),
        (Ok((_, actual)), None) => panic!("unexpected success: {}", actual),
    }
}

#[test_case("GBP", Some(CompoundAmount::BareCurrency(Currency("GBP".to_owned()))))]
#[test_case("456.78", Some(CompoundAmount::BareAmount(CompoundExpr::PerUnit(Expr::Value(dec!(456.78))))))]
#[test_case("# 1456.98", Some(CompoundAmount::BareAmount(CompoundExpr::Total(Expr::Value(dec!(1456.98))))))]
#[test_case("456.78 NZD", Some(CompoundAmount::CurrencyAmount(CompoundExpr::PerUnit(Expr::Value(dec!(456.78))), Currency("NZD".to_owned()))))]
#[test_case("# 1456.98 USD", Some(CompoundAmount::CurrencyAmount(CompoundExpr::Total(Expr::Value(dec!(1456.98))), Currency("USD".to_owned()))))]
fn test_compound_amount(s: &str, expected: Option<CompoundAmount>) {
    match (compound_amount(s.into()), expected) {
        (Ok((_, actual)), Some(expected)) => assert_eq!(actual, expected),
        (Err(_), None) => (),
        (Err(actual), Some(_)) => panic!("unexpected failure: {}", actual),
        (Ok((_, actual)), None) => panic!("unexpected success: {}", actual),
    }
}

// TODO test txn and flag

#[test_case(r#"X"#, Vec::new(), "X")]
#[test_case(r#""a" "b" "c"X"#, vec!["a", "b", "c"], "X")]
#[test_case(r#""d"   "e""f"X"#, vec!["d", "e", "f"], "X")]
fn test_txn_strings(s: &str, expected: Vec<&str>, expected_i: &str) {
    match txn_strings(s.into()) {
        Ok((i, result)) => {
            assert_eq!(result, expected);
            assert_eq!(*i.fragment(), expected_i);
        }
        e => panic!("failed with {:?}", e),
    }
}

#[test_case(r#"#a ^b #c-is-my-tag ^d.is_my/link="#, vec![Left("a".parse::<Tag>().unwrap()), Right("b".parse::<Link>().unwrap()), Left("c-is-my-tag".parse::<Tag>().unwrap()), Right("d.is_my/link".parse::<Link>().unwrap())], "=")]
fn test_tags_links(s: &str, expected: Vec<Either<Tag, Link>>, expected_i: &str) {
    match tags_links(s.into()) {
        Ok((i, result)) => {
            assert_eq!(result, expected);
            assert_eq!(*i.fragment(), expected_i);
        }
        e => panic!("failed with {:?}", e),
    }
}

#[test_case("#c-is-my-tag ", Left("c-is-my-tag".parse::<Tag>().unwrap()), " ")]
#[test_case("^d.is_my/link ", Right("d.is_my/link".parse::<Link>().unwrap()), " ")]
fn test_tag_or_links(s: &str, expected: Either<Tag, Link>, expected_i: &str) {
    match tag_or_link(s.into()) {
        Ok((i, result)) => {
            assert_eq!(result, expected);
            assert_eq!(*i.fragment(), expected_i);
        }
        e => panic!("failed with {:?}", e),
    }
}

#[test_case("2023-03-23", Some(("", 2023, 3, 23)))]
#[test_case("2023-3-2", Some(("", 2023, 3, 2)))]
#[test_case("2023/03/24", Some(("", 2023, 3, 24)))]
#[test_case("002023-0004-0000011x", Some(("x", 2023, 4, 11)))]
#[test_case("2023/03-25", None)]
#[test_case("23-03-01", None)]
#[test_case("123-03-01", None)]
#[test_case("2023-03-99", None)]
#[test_case("2023-13-01", None)]
#[test_case("2023-12-101", None)]
#[test_case("2023_04-01", None)]
#[test_case("freddyfish", None)]
fn test_date(s: &str, expected: Option<(&str, i32, u32, u32)>) {
    match (date(s.into()), expected) {
        (Ok((i, result)), Some((expected_i, y, m, d))) => {
            assert_eq!(result, NaiveDate::from_ymd_opt(y, m, d).unwrap());
            assert_eq!(*i.fragment(), expected_i);
        }
        (Err(_), None) => (),
        (Ok(_), None) => panic!("unexpectedly succeeded"),
        (Err(e), Some(_)) => panic!("unexpectedly failed with {:?}", e),
    }
}

#[test_case("\"hello world\"\"", "hello world", "\"")]
#[test_case("\"hello \\t world\" ", "hello \t world", " ")]
#[test_case("\"hello newline\\nworld\" ", "hello newline\nworld", " ")]
#[test_case("\"hello quoted \\\"world\\\"\" ", "hello quoted \"world\"", " ")]
#[test_case(
    r#""hello multiline quoted \"world\"
ok" extras"#,
    r#"hello multiline quoted "world"
ok"#,
    " extras"
)]
fn test_string(s: &str, expected: &str, expected_i: &str) {
    match string(s.into()) {
        Ok((i, result)) => {
            assert_eq!(result, expected);
            assert_eq!(*i.fragment(), expected_i);
        }
        e => panic!("failed with {:?}", e),
    }
}
