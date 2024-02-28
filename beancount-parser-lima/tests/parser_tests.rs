#[test]
fn parser_basic_testing() {
    check_parse!(
        r#"

2014-01-27 * "UNION MARKET"
  Liabilities:US:Amex:BlueCash    -22.02 USD
  Expenses:Food:Grocery            22.02 USD

"#,
        vec![transaction(
            Flag::Asterisk,
            vec![
                posting("Liabilities:US:Amex:BlueCash")
                    .amount(dec!(-22.02))
                    .currency("USD"),
                posting("Expenses:Food:Grocery")
                    .amount(dec!(22.02))
                    .currency("USD"),
            ],
        )
        .narration("UNION MARKET")
        .date("2014-01-27"),]
    )
}

#[test]
fn parser_entry_types_transaction_one_string() {
    check_parse!(
        r#"
2013-05-18 * "Nice dinner at Mermaid Inn"
    Expenses:Restaurant         100 USD
    Assets:US:Cash             -100 USD
"#,
        vec![transaction(
            Flag::Asterisk,
            vec![
                posting("Expenses:Restaurant")
                    .amount(dec!(100))
                    .currency("USD"),
                posting("Assets:US:Cash").amount(dec!(-100)).currency("USD"),
            ],
        )
        .narration("Nice dinner at Mermaid Inn")
        .date("2013-05-18"),]
    )
}

#[test]
fn parser_entry_types_transaction_two_strings() {
    check_parse!(
        r#"
2013-05-18 * "Mermaid Inn" "Nice dinner"
    Expenses:Restaurant         100 USD
    Assets:US:Cash             -100 USD
"#,
        vec![transaction(
            Flag::Asterisk,
            vec![
                posting("Expenses:Restaurant")
                    .amount(dec!(100))
                    .currency("USD"),
                posting("Assets:US:Cash").amount(dec!(-100)).currency("USD"),
            ],
        )
        .payee("Mermaid Inn")
        .narration("Nice dinner")
        .date("2013-05-18"),]
    )
}

#[test]
fn parser_entry_types_transaction_three_strings() {
    check_parse_errors!(
        r#"
2013-05-18 * "Mermaid Inn" "Nice dinner" "With Caroline"
    Expenses:Restaurant         100 USD
    Assets:US:Cash             -100 USD
"#,
        vec![r#"found '"With Caroline"' expected something else"#]
    )
}

use crate::test_support::{check, check_errors, posting, transaction};
use ::beancount_parser_lima as lima;
use lima::{BeancountParser, BeancountSources, Flag};
use rust_decimal_macros::dec;

mod test_support;
