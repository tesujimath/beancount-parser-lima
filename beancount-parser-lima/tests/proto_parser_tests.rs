use test_case::test_case;

#[test_case("parser.basic-testing")]
#[test_case("parser-entry-types.balance")]
#[test_case("parser-entry-types.transaction-one-string")]
#[test_case("parser-entry-types.transaction-two-strings")]
fn test_parser(test_name: &str) {
    check_parse(test_name);
}

use proto::check_parse;

mod proto;
