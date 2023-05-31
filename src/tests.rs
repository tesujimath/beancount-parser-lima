#![cfg(test)]
use super::*;
use nom::IResult;
use nom_supreme::error::{BaseErrorKind, ErrorTree, Expectation};
use test_case::test_case;

#[test_case("2023-03-23", Ok(("", NaiveDate::from_ymd_opt(2023, 3, 23).unwrap())))]
#[test_case("2023-04-011", Ok(("1", NaiveDate::from_ymd_opt(2023, 4, 1).unwrap())))]
#[test_case(
    "2023-03-99",
    Err(nom::Err::Error(ErrorTree::Base {
            location: "2023-03-99",
            kind: BaseErrorKind::External(Box::new(ParseError {
                reason: ParseErrorReason::DateOutOfRange,
            }))
    })))]
#[test_case("2023_04-011", Err(nom::Err::Error(ErrorTree::Base { location: "_04-011", kind: BaseErrorKind::Expected(Expectation::Tag("-")) })))]
//#[test_case("2023_03-23", Err(Err::Error(Error::new("_03-23", ErrorKind::Tag))))]
//#[test_case(
//    "2023-03-XX",
//    Err(Err::Error(Error::new("XX", ErrorKind::TakeWhileMN)))
//)]
//#[test_case(
//    "freddyfish",
//    Err(Err::Error(Error::new("freddyfish", ErrorKind::TakeWhileMN)))
//)]
fn test_date(s: &str, expected: IResult<&str, NaiveDate, ErrorTree<&str>>) {
    match (date(s), expected) {
        (Ok(actual), Ok(expected)) => assert_eq!(actual, expected),
        (
            Err(nom::Err::Error(ErrorTree::Base {
                location: actual_location,
                ..
            })),
            Err(nom::Err::Error(ErrorTree::Base {
                location: expected_location,
                ..
            })),
        ) => assert_eq!(actual_location, expected_location),
        (actual, expected) => panic!("got {:?} expected {:?}", actual, expected),
    }
}

/*
#[test_case("2022-04-01 2023-03-31", Ok(Doc{ d1: NaiveDate::from_ymd_opt(2022, 4, 1).unwrap(), d2: NaiveDate::from_ymd_opt(2023, 3, 31).unwrap()}))]
#[test_case("2022_04-01 2023-03-", Ok(Doc{ d1: NaiveDate::from_ymd_opt(2022, 4, 1).unwrap(), d2: NaiveDate::from_ymd_opt(2023, 3, 31).unwrap()}))]
fn test_final_doc(s: &str, expected: Result<Doc, ()>) {
    assert_eq!(final_doc(s), expected);
}
*/
