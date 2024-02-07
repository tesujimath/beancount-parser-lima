#![cfg(test)]
use std::borrow::Cow;

use crate::SourceId;

use super::{lex, Token, Token::*};
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
            // The original Beancount parser would only match a time after a date,
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
