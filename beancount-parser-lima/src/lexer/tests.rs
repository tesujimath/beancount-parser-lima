#![cfg(test)]
use crate::bare_lex;

use super::{lex, LexerError, Token, Token::*};
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::borrow::Cow;
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
    let actual = lex(s).map(|(tok, _span)| tok).collect::<Vec<_>>();

    assert_eq!(actual, expected);
}

fn bare_lex_and_check(s: &str, expected: Vec<Token>) {
    let actual = bare_lex(s).map(|(tok, _span)| tok).collect::<Vec<_>>();

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

// ANOMALY: The test from original Beancount has account name of `Assets:signals`
// which is invalid.  Fixed here.
#[test]
fn account_names_with_numbers() {
    lex_and_check(
        r#"
Assets:Vouchers:99Ranch
Assets:99Test
Assets:Signals
"#,
        vec![
            Account("Assets:Vouchers:99Ranch"),
            Eol,
            Account("Assets:99Test"),
            Eol,
            Account("Assets:Signals"),
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

// ANOMALY: because Logos doesn't support contextual lexing,
// we lex the bogus `check` directive as a Key.  Would be rejected by our parser.
#[test]
fn invalid_directive() {
    lex_and_check(
        r#"
2008-03-01 check Assets:BestBank:Savings 2340.19 USD
"#,
        vec![
            date("2008-03-01"),
            Key("check"),
            Account("Assets:BestBank:Savings"),
            number!(2340.19),
            Currency("USD"),
            Eol,
        ],
    );
}

// ANOMALY: our lexer strips the comment leaving nothing, whereas original
// Beancount replaces it with an Eol token.
#[test]
fn comment() {
    lex_and_check(
        r#"
;; This is a typical error that should get detected for long strings.
2008-03-01
"#,
        vec![date("2008-03-01"), Eol],
    );
}

#[test]
fn multiline_string() {
    lex_and_check(
        r#"
"
I'm a
multi-line
string
but not too long
"
"#,
        vec![
            string_literal(
                r#"
I'm a
multi-line
string
but not too long
"#,
            ),
            Eol,
        ],
    );
}

// ANOMALY: string_too_long is not checked by our lexer, it is a parser feature,
// because only the parser has access to the option which defines how long is too long

// ANOMALY: we represent numbers as rust_decimal::Decimal, which are 128 bit quantities,
// not arbitrary precision.
#[test]
fn very_long_number() {
    let long_number_str = String::from_iter(&['1'; 29]);
    let long_number = Decimal::try_from(long_number_str.as_ref()).unwrap();
    lex_and_check(long_number_str.as_ref(), vec![Number(long_number), Eol]);
}

// ANOMALY: very_long_string is a parser test for us

// ANOMALY: default lexer adds a final newline; if this is not desired, use `bare_lex`
#[test]
fn no_final_newline() {
    bare_lex_and_check(
        r#"2014-01-01 open Assets:Temporary    "#,
        vec![date("2014-01-01"), Open, Account("Assets:Temporary")],
    );
}

// ANOMALY: our string unescaper is slightly different from abseil CUnescape, which does C style escapes.
// See https://doc.rust-lang.org/std/ascii/fn.escape_default.html
#[test]
fn string_escaped() {
    lex_and_check(
        r#"
"The Great \"Juju\""
"The Great \t\n\r\f\b"
"#,
        vec![
            string_literal(r#"The Great "Juju""#),
            Eol,
            string_literal("The Great \t\n\r\x0C\x08"),
            Eol,
        ],
    );
}

#[test]
fn string_newline() {
    let s = format!(r#""The Great\nJuju"{}"#, "\n");
    lex_and_check(s.as_str(), vec![string_literal("The Great\nJuju"), Eol]);
}

// ANOMALY: multiline strings include and spaces at the beginning of each line.
// Original Beancount test suggests that is not the case.
#[test]
fn string_newline_long() {
    lex_and_check(
        r#"
    "Forty
    world
    leaders
    and
    hundreds"
"#,
        vec![
            string_literal("Forty\n    world\n    leaders\n    and\n    hundreds"),
            Eol,
        ],
    );
}

// ANOMALY: I am unsure what StringNewlineTooLong is trying to test, so I ignored it.

// ANOMALY: Omitted InvalidCharacter test, as Rust prevents generation of such a malformed string

#[test]
fn popmeta() {
    lex_and_check(
        r#"
    popmeta location:
"#,
        vec![Popmeta, Key("location"), Colon, Eol],
    );
}

#[test]
fn true_false_null() {
    lex_and_check(
        r#"
    TRUE FALSE NULL
"#,
        vec![True, False, Null, Eol],
    );
}

#[test]
fn valid_commas_in_number() {
    lex_and_check(
        r#"
    45,234.00
"#,
        vec![number!(45234), Eol],
    );
}

// ANOMALY: We're stricter about commas than original Beancount, so this lexes differently
// ANOMALY: There seems to be a Logos bug here, which is why the comma gets eaten by the 452.
// Logos issue: [Incorrectly matched token slice](https://github.com/maciejhirsz/logos/issues/256)
#[test]
fn invalid_commas_in_integer() {
    lex_and_check(
        r#"
    452,34.00
"#,
        vec![
            number!(452),
            // should get a comma token here:
            // Comma,
            number!(34.0),
            Eol,
        ],
    );
}

#[test]
fn invalid_commas_in_fractional() {
    lex_and_check(
        r#"
    45234.000,000
"#,
        vec![number!(45234.0), Comma, number!(0), Eol],
    );
}

#[test]
fn ignored_lines_comment() {
    lex_and_check(
        r#"
    ;; Long comment line about something something.
"#,
        Vec::default(),
    );
}

// ANOMALY: we handle indent before discarded line differently from original Beancount
#[test]
fn ignored_lines_indented_comment() {
    lex_and_check(
        r#"
option "title" "The Title"
      ;; Something something.

"#,
        vec![
            Option,
            string_literal("title"),
            string_literal("The Title"),
            Eol,
        ],
    );
}

// ANOMALY: unrecognized tokens are returned per character, not per-word
// and we lex lowercase words as keys even without trailing colons (as Logos
// can't do trailing context).
#[test]
fn ignored_lines_non_comment_ignored() {
    lex_and_check(
        r#"
    Regular prose appearing mid-file.
"#,
        vec![
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            Key("prose"),
            Key("appearing"),
            Key("mid-file"),
            unrecognized(),
            Eol,
        ],
    );
}

// ANOMALY: as above with keys
#[test]
fn ignored_lines_non_comment_non_flag() {
    lex_and_check(
        r#"
    Xxx this sentence starts with a non-flag character.
"#,
        vec![
            unrecognized(),
            unrecognized(),
            unrecognized(),
            Key("this"),
            Key("sentence"),
            Key("starts"),
            Key("with"),
            unrecognized(),
            Key("non-flag"),
            Key("character"),
            unrecognized(),
            Eol,
        ],
    );
}

// ANOMALY: To ignore a line starting with an flag there must be no leading space.
// I am unsure whether this is OK or not, but it's tricky to replicate the existing
// functionality because of the ambiguous token matching for indent #tag,
// which must lex as a tag and not an ignored line.  Also postings with flags.
#[test]
fn ignored_lines_non_comment_org_mode_title() {
    lex_and_check(
        r#"
* This sentence is an org-mode title.
"#,
        Vec::default(),
    );
}

// whitespace prefix *is* allowed here
#[test]
fn ignored_lines_non_comment_org_mode_drawer() {
    lex_and_check(
        r#"
    :PROPERTIES:
    :this: is an org-mode property drawer
    :END:
"#,
        Vec::default(),
    );
}

// ANOMALY: gets lexed differently, and with unrecognized per character not per word
#[test]
fn lexer_error_invalid_text() {
    lex_and_check(
        r#"
    Not a Beancount file.
"#,
        vec![
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            unrecognized(),
            Key("file"),
            unrecognized(),
            Eol,
        ],
    );
}

#[test]
fn lexer_error_invalid_token() {
    lex_and_check(
        r#"
    2000-01-01 open ` USD
"#,
        vec![
            date("2000-01-01"),
            Open,
            unrecognized(),
            Currency("USD"),
            Eol,
        ],
    );
}

#[test]
fn lexer_error_recovery() {
    lex_and_check(
        r#"
    2000-13-32 open Assets:Something
    2000-01-02 open Assets:Working
"#,
        vec![
            error("month out of range"),
            Open,
            Account("Assets:Something"),
            Eol,
            Indent,
            date("2000-01-02"),
            Open,
            Account("Assets:Working"),
            Eol,
        ],
    );
}

// ANOMALY: why are the single quotes escaped in original lexer?
#[test]
fn lexer_error_substring_with_quotes() {
    lex_and_check(
        r#"
    2016-07-15 query "hotels" "SELECT * WHERE account ~ 'Expenses:Accommodation'"
"#,
        vec![
            date("2016-07-15"),
            Query,
            string_literal("hotels"),
            string_literal("SELECT * WHERE account ~ 'Expenses:Accommodation'"),
            Eol,
        ],
    );
}

#[test]
fn unicode_utf8() {
    lex_and_check(
        r#"
    2015-05-23 note Assets:Something "a¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ z"
"#,
        vec![
            date("2015-05-23"),
            Note,
            Account("Assets:Something"),
            string_literal("a¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼ z"),
            Eol,
        ],
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
