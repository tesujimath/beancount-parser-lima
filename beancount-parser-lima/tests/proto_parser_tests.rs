#[test]
fn proto_parser_entry_types_balance() {
    check_proto_parse(
        r#"
2013-05-18 balance Assets:US:BestBank:Checking  200 USD
2013-05-18 balance Assets:US:BestBank:Checking  200 ~ 0.002 USD
"#,
        r#"
    directives {
      date { year: 2013 month: 5 day: 18 }
      balance {
        account: "Assets:US:BestBank:Checking"
        amount { number { exact: "200" } currency: "USD" }
      }
    }
    directives {
      date { year: 2013 month: 5 day: 18 }
      balance {
        account: "Assets:US:BestBank:Checking"
        amount { number { exact: "200" } currency: "USD" }
        tolerance { exact: "0.002" }
      }
    }
"#,
    )
}

use proto::check_proto_parse;

mod proto;
