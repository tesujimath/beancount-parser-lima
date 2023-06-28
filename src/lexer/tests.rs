#![cfg(test)]
use super::*;
use test_case::test_case;

#[test_case("2023-03-23", Some((2023, 3, 23, "")))]
#[test_case("2023-3-2", None)]
#[test_case("2023/03/24", Some((2023, 3, 24, "")))]
#[test_case("2023-04-11x", Some((2023, 4, 11, "x")))]
#[test_case("2023/03-25", Some((2023, 3, 25, "")))]
#[test_case("23-03-01", None)]
#[test_case("123-03-01", None)]
#[test_case("2023-03-99", None)]
#[test_case("2023-13-01", None)]
#[test_case("2023-12-101", Some((2023, 12, 10, "1")))]
#[test_case("2023_04-01", None)]
#[test_case("freddyfish", None)]
fn test_date(s: &str, expected: Option<(i32, u32, u32, &str)>) {
    let result = date()
        .then(any().repeated().collect::<String>())
        .parse(s)
        .into_result();

    match expected {
        Some((year, month, day, unparsed)) => assert_eq!(
            result,
            Ok((
                NaiveDate::from_ymd_opt(year, month, day).unwrap(),
                unparsed.to_owned()
            )),
        ),
        None => assert!(result.is_err()),
    }
}

#[test_case("3:03:23", Some((3, 3, 23, "")))]
#[test_case("2023:3:2", None)]
#[test_case("23:03", Some((23, 3, 0, "")))]
#[test_case("23:04:11x", Some((23, 4, 11, "x")))]
#[test_case("23:03:25", Some((23, 3, 25, "")))]
#[test_case("24:03:01", None)]
#[test_case("123:03:01", None)]
#[test_case("2:03:99", None)]
#[test_case("2:60:01", None)]
#[test_case("02:12:101", Some((2, 12, 10, "1")))]
#[test_case("3_04:01", None)]
#[test_case("freddyfish", None)]
fn test_time(s: &str, expected: Option<(u32, u32, u32, &str)>) {
    let result = time()
        .then(any().repeated().collect::<String>())
        .parse(s)
        .into_result();

    match expected {
        Some((hour, min, sec, unparsed)) => assert_eq!(
            result,
            Ok((
                NaiveTime::from_hms_opt(hour, min, sec).unwrap(),
                unparsed.to_owned()
            )),
        ),
        None => assert!(result.is_err()),
    }
}

#[test_case("Assets:Car", Some((AccountType::Assets, vec!["Car"])))]
#[test_case("Assets:Car:Fuel", Some((AccountType::Assets, vec!["Car", "Fuel"])))]
#[test_case("Assets:oops", None)]
fn test_account(s: &str, expected_raw: Option<(AccountType, Vec<&str>)>) {
    let expected = expected_raw.map(|(account_type, names)| Account {
        account_type,
        names: NonEmpty::collect(names.into_iter().map(|name| AccountName(name.to_string())))
            .unwrap(),
    });

    match account().parse(s).into_result() {
        Ok(result) => assert_eq!(result, expected.unwrap()),
        Err(e) => {
            println!("{:?}", e);
            assert!(expected.is_none());
        }
    }
}

#[test_case(r#""hello world"""#, "hello world", r#"""#)]
#[test_case(r#""hello \t world" "#, "hello \t world", " ")]
#[test_case(r#""hello newline\nworld" "#, "hello newline\nworld", " ")]
#[test_case(r#""hello quoted \"world\"" "#, "hello quoted \"world\"", " ")]
#[test_case(
    r#""hello multiline quoted \"world\"
ok" extras"#,
    r#"hello multiline quoted "world"
ok"#,
    " extras"
)]
fn test_string_literal(s: &str, expected: &str, unparsed: &str) {
    let result = string_literal()
        .then(any().repeated().collect::<String>())
        .parse(s)
        .into_result();

    assert_eq!(result, Ok((expected.to_owned(), unparsed.to_owned())));
}
