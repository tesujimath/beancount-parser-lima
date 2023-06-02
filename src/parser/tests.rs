#![cfg(test)]
use super::*;
use either::Either::{self, Left, Right};
use test_case::test_case;

#[test_case("Assets:Car", Some((AccountType::Assets, vec!["Car"])))]
#[test_case("Assets:oops", None)]
#[test_case("Assets:Bike:oops", Some((AccountType::Assets, vec!["Bike"])))]
fn test_account(s: &str, expected_raw: Option<(AccountType, Vec<&str>)>) {
    let expected = expected_raw.map(|(account_type, subs)| Account {
        account_type,
        sub_accounts: subs.iter().map(|sub| SubAccount(sub.to_string())).collect(),
    });

    match account(s) {
        Ok((_, result)) => assert_eq!(result, expected.unwrap()),
        Err(e) => {
            println!("{:?}", e);
            assert!(expected.is_none());
        }
    }
}

// TODO test txn and flag

#[test_case(r#"X"#, Vec::new(), "X")]
#[test_case(r#""a" "b" "c"X"#, vec!["a", "b", "c"], "X")]
#[test_case(r#""d"   "e""f"X"#, vec!["d", "e", "f"], "X")]
fn test_txn_strings(s: &str, expected: Vec<&str>, expected_loc: &str) {
    match txn_strings(s) {
        Ok((loc, actual)) => {
            assert_eq!(actual, expected);
            assert_eq!(loc, expected_loc);
        }
        e => panic!("failed with {:?}", e),
    }
}

#[test_case(r#"#a ^b #c-is-my-tag ^d.is_my/link="#, vec![Left("a".parse::<Tag>().unwrap()), Right("b".parse::<Link>().unwrap()), Left("c-is-my-tag".parse::<Tag>().unwrap()), Right("d.is_my/link".parse::<Link>().unwrap())], "=")]
fn test_tags_links(s: &str, expected: Vec<Either<Tag, Link>>, expected_loc: &str) {
    match tags_links(s) {
        Ok((loc, actual)) => {
            assert_eq!(actual, expected);
            assert_eq!(loc, expected_loc);
        }
        e => panic!("failed with {:?}", e),
    }
}

#[test_case("#c-is-my-tag ", Left("c-is-my-tag".parse::<Tag>().unwrap()), " ")]
#[test_case("^d.is_my/link ", Right("d.is_my/link".parse::<Link>().unwrap()), " ")]
fn test_tag_or_links(s: &str, expected: Either<Tag, Link>, expected_loc: &str) {
    match tag_or_link(s) {
        Ok((loc, actual)) => {
            assert_eq!(actual, expected);
            assert_eq!(loc, expected_loc);
        }
        e => panic!("failed with {:?}", e),
    }
}

#[test_case("2023-03-23", Some(("", 2023, 3, 23)))]
#[test_case("2023-3-2", Some(("", 2023, 3, 2)))]
#[test_case("2023/03/24", Some(("", 2023, 3, 24)))]
#[test_case("2023/03-25", Some(("", 2023, 3, 25)))]
#[test_case("002023-0004-0000011x", Some(("x", 2023, 4, 11)))]
#[test_case("23-03-01", None)]
#[test_case("123-03-01", None)]
#[test_case("2023-03-99", None)]
#[test_case("2023-13-01", None)]
#[test_case("2023-12-101", None)]
#[test_case("2023_04-01", None)]
#[test_case("freddyfish", None)]
fn test_date(s: &str, expected: Option<(&str, i32, u32, u32)>) {
    match (date(s), expected) {
        (Ok((actual_loc, actual_result)), Some((expected_loc, y, m, d))) => {
            assert_eq!(actual_result, NaiveDate::from_ymd_opt(y, m, d).unwrap());
            assert_eq!(actual_loc, expected_loc);
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
fn test_string(s: &str, expected: &str, expected_loc: &str) {
    match string(s) {
        Ok((loc, actual)) => {
            assert_eq!(actual, expected);
            assert_eq!(loc, expected_loc);
        }
        e => panic!("failed with {:?}", e),
    }
}
