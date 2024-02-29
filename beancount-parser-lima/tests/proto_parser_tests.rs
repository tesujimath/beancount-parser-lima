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

#[test]
fn all_proto_parser_tests() {
    let cargo_manifest_dir: PathBuf = env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let tests_path = cargo_manifest_dir.join("tests/tests.txtpb");
    let tests_txtpb = read_to_string(&tests_path).unwrap_or_else(|_| {
        panic!(
            "failed to read tests from text format file {:?}",
            &tests_path
        )
    });

    check_tests(tests_txtpb.as_str());
}

use std::{env, fs::read_to_string, path::PathBuf};

use proto::{check_proto_parse, check_tests};

mod proto;
