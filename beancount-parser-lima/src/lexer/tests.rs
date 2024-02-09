#![cfg(test)]
use std::borrow::Cow;

use crate::SourceId;

use super::{lex, LexerError, Token, Token::*};
use rust_decimal_macros::dec;
use time::format_description::well_known::Iso8601;

macro_rules! number {
    ($n:expr) => {
        Number(dec!($n))
    };
}

fn date(s: &str) -> Token {
    Date(time::Date::parse(s, &Iso8601::DEFAULT).unwrap())
}

fn time(s: &str) -> Token {
    Time(time::Time::parse(s, &Iso8601::DEFAULT).unwrap())
}

fn string_literal(s: &str) -> Token {
    StringLiteral(Cow::Borrowed(s))
}

fn unrecognized() -> Token<'static> {
    Error(LexerError::new("unrecognized token"))
}

fn error(s: &'static str) -> Token<'static> {
    Error(LexerError::new(s))
}

fn lex_and_check(s: &str, expected: Vec<Token>) {
    let actual = lex(SourceId::default(), s)
        .into_iter()
        .map(|(tok, _span)| tok)
        .collect::<Vec<_>>();

    assert_eq!(actual, expected);
}

#[test]
fn basic_tokens() {
    lex_and_check(
        r#"
2013-05-18 2014-01-02 2014/01/02
    13:18 2013-05-18 12:34:56 2013-05-18 12:34
    Assets:US:Bank:Checking
    Liabilities:US:Bank:Credit
    Other:Bank
    USD HOOL TEST_D TEST_3 TEST-D TEST-3 NT V V12
    /NQH21 /6A /6J8 ABC.TO /3.2
    "Nice dinner at Mermaid Inn"
    ""
    123 123.45 123.456789 -123 -123.456789
    #sometag123
    ^sometag123
    somekey:
"#,
        vec![
            date("2013-05-18"),
            date("2014-01-02"),
            date("2014-01-02"),
            Eol,
            Indent,
            // ANOMALY: The original Beancount parser would only match a time after a date,
            // but that was for performance reasons, not reasons of syntax, so
            // this parser differs here.
            time("13:18"),
            date("2013-05-18"),
            time("12:34:56.0"),
            date("2013-05-18"),
            time("12:34"),
            Eol,
            Indent,
            Account("Assets:US:Bank:Checking"),
            Eol,
            Indent,
            Account("Liabilities:US:Bank:Credit"),
            Eol,
            Indent,
            Account("Other:Bank"),
            Eol,
            Indent,
            Currency("USD"),
            Currency("HOOL"),
            Currency("TEST_D"),
            Currency("TEST_3"),
            Currency("TEST-D"),
            Currency("TEST-3"),
            Currency("NT"),
            Currency("V"),
            Currency("V12"),
            Eol,
            Indent,
            Currency("/NQH21"),
            Currency("/6A"),
            Currency("/6J8"),
            Currency("ABC.TO"),
            Slash,
            number!(3.2),
            Eol,
            Indent,
            string_literal("Nice dinner at Mermaid Inn"),
            Eol,
            Indent,
            string_literal(""),
            Eol,
            Indent,
            number!(123),
            number!(123.45),
            number!(123.456789),
            Minus,
            number!(123),
            Minus,
            number!(123.456789),
            Eol,
            Indent,
            Tag("sometag123"),
            Eol,
            Indent,
            Link("sometag123"),
            Eol,
            Indent,
            Key("somekey"),
            Colon,
            Eol,
        ],
    );
}

#[test]
fn unicode_account_name() {
    lex_and_check(
        r#"
Other:Bank
Óthяr:Bあnk
abc1:abc1
ΑβγⅠ:ΑβγⅠ
ابجا:ابجا
"#,
        vec![
            Account("Other:Bank"),
            Eol,
            Account("Óthяr:Bあnk"),
            Eol,
            Key("abc1"),
            Colon,
            Key("abc1"),
            Eol,
            Account("ΑβγⅠ:ΑβγⅠ"),
            Eol,
            Account("ابجا:ابجا"),
            Eol,
        ],
    );
}

#[test]
fn indent() {
    lex_and_check(
        r#"
2014-07-05 *
    Equity:Something
"#,
        vec![
            date("2014-07-05"),
            Asterisk,
            Eol,
            Indent,
            Account("Equity:Something"),
            Eol,
        ],
    );
}

#[test]
fn comma_currencies() {
    lex_and_check(
        r#"
USD,CAD,AUD
"#,
        vec![
            Currency("USD"),
            Comma,
            Currency("CAD"),
            Comma,
            Currency("AUD"),
            Eol,
        ],
    );
}

#[test]
fn number_ok() {
    lex_and_check(
        r#"
1001 USD
1002.00 USD
-1001 USD
-1002.00 USD
+1001 USD
+1002.00 USD
1,001 USD
1,002.00 USD
-1,001 USD
-1,002.00 USD
+1,001 USD
+1,002.00 USD
"#,
        vec![
            number!(1001),
            Currency("USD"),
            Eol,
            number!(1002.00),
            Currency("USD"),
            Eol,
            Minus,
            number!(1001),
            Currency("USD"),
            Eol,
            Minus,
            number!(1002.00),
            Currency("USD"),
            Eol,
            Plus,
            number!(1001),
            Currency("USD"),
            Eol,
            Plus,
            number!(1002.00),
            Currency("USD"),
            Eol,
            number!(1001),
            Currency("USD"),
            Eol,
            number!(1002.00),
            Currency("USD"),
            Eol,
            Minus,
            number!(1001),
            Currency("USD"),
            Eol,
            Minus,
            number!(1002.00),
            Currency("USD"),
            Eol,
            Plus,
            number!(1001),
            Currency("USD"),
            Eol,
            Plus,
            number!(1002.00),
            Currency("USD"),
            Eol,
        ],
    );
}

#[test]
fn number_space() {
    lex_and_check(
        r#"
- 1002.00 USD
"#,
        vec![Minus, number!(1002), Currency("USD"), Eol],
    );
}

#[test]
fn number_dots() {
    lex_and_check(
        r#"
1.234.00 USD
"#,
        vec![
            number!(1.234),
            unrecognized(), // ANOMALY: just the dot, which is where we differ from the original Beancount scanner
            number!(0),
            Currency("USD"),
            Eol,
        ],
    );
}

#[test]
fn number_no_integer() {
    lex_and_check(
        r#"
.2347 USD
"#,
        vec![
            unrecognized(), // ANOMALY: just the dot, which is where we differ from the original Beancount scanner
            number!(2347),
            Currency("USD"),
            Eol,
        ],
    );
}

#[test]
fn currency_number() {
    lex_and_check(
        r#"
555.00 CAD.11
"#,
        vec![number!(555), Currency("CAD.11"), Eol],
    );
}

#[test]
fn currency_dash() {
    lex_and_check(
        r#"
TEST-DA
"#,
        vec![Currency("TEST-DA"), Eol],
    );
}

#[test]
fn bad_date_invalid_token() {
    lex_and_check(
        r#"
2013-12-98
"#,
        vec![error("date out of range"), Eol],
    );
}

#[test]
fn bad_date_valid_but_invalid() {
    lex_and_check(
        r#"
2013-15-01
"#,
        vec![error("month out of range"), Eol],
    );
}

#[test]
fn date_followed_by_number() {
    lex_and_check(
        r#"
2013-12-228
"#,
        // ANOMALY: Because it cares about word boundary on the date, original Beancount parses as an
        // arithmetic expression.  But Logos doesn't support boundaries.  So we do it differently.
        // vec![number!(2013), Minus, number!(12), Minus, number!(228), Eol],
        vec![date("2013-12-22"), number!(8), Eol],
    );
}

#[test]
fn bad_time() {
    lex_and_check(
        r#"
99:99
2000-09-10 99:99
"#,
        // ANOMALY: Again, we handle this differently.
        vec![
            number!(99),
            Colon,
            number!(99),
            Eol,
            date("2000-09-10"),
            number!(99),
            Colon,
            number!(99),
            Eol,
        ],
    );
}

#[test]
fn single_letter_account() {
    lex_and_check(
        r#"
Assets:A
"#,
        vec![Account("Assets:A"), Eol],
    );
}

#[test]
fn account_names_with_numbers() {
    lex_and_check(
        r#"
Assets:Vouchers:99Ranch
Assets:99Test
Assets:signals
"#,
        vec![
            Account("Assets:Vouchers:99Ranch"),
            Eol,
            Account("Assets:99Test"),
            Eol,
            Account("Assets:signals"),
            Eol,
        ],
    );
}

#[test]
fn account_names_with_dash() {
    lex_and_check(
        r#"
Equity:Beginning-Balances
"#,
        vec![Account("Equity:Beginning-Balances"), Eol],
    );
}

#[test]
fn stupidly_long_account_names() {
    lex_and_check(
        r#"
Expenses:Something:AX:BX:CX:DX:EX:FX:GX:HX
"#,
        vec![Account("Expenses:Something:AX:BX:CX:DX:EX:FX:GX:HX"), Eol],
    );
}

#[test]
fn keywords_as_key() {
    lex_and_check(
        r#"
2013-05-18 open Assets:US:Bank:Checking
    txn: "is a key not a keyword"
    balance: "is a key not a keyword"
    open: "is a key not a keyword"
    close: "is a key not a keyword"
    commodity: "is a key not a keyword"
    pad: "is a key not a keyword"
    event: "is a key not a keyword"
    query: "is a key not a keyword"
    custom: "is a key not a keyword"
    price: "is a key not a keyword"
    note: "is a key not a keyword"
    document: "is a key not a keyword"
    pushtag: "is a key not a keyword"
    poptag: "is a key not a keyword"
    pushmeta: "is a key not a keyword"
    popmeta: "is a key not a keyword"
    option: "is a key not a keyword"
    options: "is a key not a keyword"
    plugin: "is a key not a keyword"
    include: "is a key not a keyword"
"#,
        vec![
            date("2013-05-18"),
            Open,
            Account("Assets:US:Bank:Checking"),
            Eol,
            Indent,
            Key("txn"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("balance"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("open"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("close"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("commodity"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("pad"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("event"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("query"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("custom"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("price"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("note"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("document"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("pushtag"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("poptag"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("pushmeta"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("popmeta"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("option"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("options"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("plugin"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
            Indent,
            Key("include"),
            Colon,
            string_literal("is a key not a keyword"),
            Eol,
        ],
    );
}
