#![cfg(test)]
use super::*;
use nom::{
    error::{Error, ErrorKind},
    Err, IResult,
};

use test_case::test_case;

#[test_case("2023-03-23", Ok(("", NaiveDate::from_ymd_opt(2023, 3, 23).unwrap())))]
#[test_case("2023-04-011", Ok(("1", NaiveDate::from_ymd_opt(2023, 4, 1).unwrap())))]
#[test_case(
    "2023-03-99",
    Err(Err::Error(Error::new("2023-03-99", ErrorKind::Fail)))
)]
#[test_case(
    "2023-03-XX",
    Err(Err::Error(Error::new("XX", ErrorKind::TakeWhileMN)))
)]
#[test_case(
    "freddyfish",
    Err(Err::Error(Error::new("freddyfish", ErrorKind::TakeWhileMN)))
)]
fn test_date(s: &str, expected: IResult<&str, NaiveDate>) {
    assert_eq!(date(s), expected);
}
