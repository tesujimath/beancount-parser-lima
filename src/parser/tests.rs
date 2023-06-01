#![cfg(test)]
use super::*;
use test_case::test_case;

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

// TODO test txn and flag

#[test_case("\"hello world\"\"", "hello world", "\"")]
#[test_case("\"hello \t world\" ", "hello \t world", " ")]
#[test_case("\"hello quoted \\\"world\\\"\" ", "hello quoted \"world\"", " ")]
fn test_string(s: &str, expected: &str, expected_loc: &str) {
    match string(s) {
        Ok((loc, actual)) => {
            assert_eq!(actual, expected);
            assert_eq!(loc, expected_loc);
        }
        e => panic!("failed with {:?}", e),
    }
}
