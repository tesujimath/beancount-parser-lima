#![cfg(test)]
use super::*;
use nom::Finish;
use test_case::test_case;

#[test_case("2023-03-23", Some(("", NaiveDate::from_ymd_opt(2023, 3, 23).unwrap())))]
#[test_case("2023-04-011", Some(("1", NaiveDate::from_ymd_opt(2023, 4, 1).unwrap())))]
#[test_case("2023-03-XX", None)]
fn test_date(s: &str, expected: Option<(&str, NaiveDate)>) {
    assert_eq!(date(s).finish().ok(), expected);
}
