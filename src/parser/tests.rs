#![cfg(test)]
use super::*;
use test_case::test_case;

#[test_case("Assets:Car", Some((AccountType::Assets, vec!["Car"])))]
#[test_case("Assets:Car:Fuel", Some((AccountType::Assets, vec!["Car", "Fuel"])))]
#[test_case("Assets:oops", None)]
fn test_account(s: &str, expected_raw: Option<(AccountType, Vec<&str>)>) {
    let expected = expected_raw.map(|(account_type, subs)| Account {
        account_type,
        sub_accounts: NonEmpty::collect(subs.into_iter().map(|sub| SubAccount(sub.to_string())))
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
