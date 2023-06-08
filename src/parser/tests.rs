#![cfg(test)]
use super::*;
use test_case::test_case;

#[test_case("Assets:Car", Some((AccountType::Assets, vec!["Car"])))]
#[test_case("Assets:oops", None)]
#[test_case("Assets:Bike:oops", Some((AccountType::Assets, vec!["Bike"])))]
fn test_account(s: &str, expected_raw: Option<(AccountType, Vec<&str>)>) {
    let expected = expected_raw.map(|(account_type, subs)| Account {
        account_type,
        sub_accounts: subs.iter().map(|sub| SubAccount(sub.to_string())).collect(),
    });

    match account().parse(s) {
        Ok(result) => assert_eq!(result, expected.unwrap()),
        Err(e) => {
            println!("{:?}", e);
            assert!(expected.is_none());
        }
    }
}
