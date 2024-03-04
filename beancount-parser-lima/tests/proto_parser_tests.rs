use test_case::test_case;

#[test_case("parser.basic-testing")]
#[test_case("parser-entry-types.balance")]
fn test_parser(test_name: &str) {
    check_parse(test_name);
}

use proto::check_parse;

mod proto;
