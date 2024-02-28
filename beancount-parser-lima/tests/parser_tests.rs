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

use crate::test_support::{posting, transaction, Directive, ExpectEq};
use ::beancount_parser_lima as lima;
use lima::{BeancountParser, BeancountSources, Flag, ParseError, ParseSuccess};
use rust_decimal_macros::dec;
use std::io::{stderr, Write};

mod test_support;
