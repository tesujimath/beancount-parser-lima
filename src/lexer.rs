use super::*;
use chrono::{NaiveDateTime, NaiveTime};
use chumsky::{
    prelude::*,
    span::SimpleSpan,
    text::{inline_whitespace, keyword},
};

#[derive(Clone)]
pub enum Token {
    True,
    False,
    Null,
    Currency(super::Currency),
    Pipe,
    AtAt,
    At,
    LcurlCurl,
    RcurlCurl,
    Lcurl,
    Rcurl,
    Comma,
    Tilde,
    Plus,
    Minus,
    Slash,
    Lparen,
    Rparen,
    Hash,
    Asterisk,
    Colon,
    OtherFlag(char),
    Txn,
    Balance,
    Open,
    Close,
    Commodity,
    Pad,
    Event,
    Query,
    Custom,
    Price,
    Note,
    Document,
    Pushtag,
    Poptag,
    Pushmeta,
    Popmeta,
    Option,
    Options,
    Plugin,
    Include,
    DateTime(NaiveDateTime),
    Date(NaiveDate),
}

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char, Span>>> {
    use Token::*;

    // keywords which look like currencies, so must be first
    keyword("TRUE").to(True)
        .or(keyword("FALSE").to(False))
        .or(keyword("NULL").to(Null))
        //
        // currency
        .or(regex(r"[A-Z][A-Z0-9\'\.\_\-]*[A-Z0-9]?\b|\/[A-Z0-9\'\.\_\-]*[A-Z]([A-Z0-9\'\.\_\-]*[A-Z0-9])?")
            .try_map(|s: &str, span| {
                s.parse::<super::Currency>()
                    .map(Currency)
                    .map_err(|e| chumsky::error::Rich::custom(span, e))
            }))
        //
        // special characters
        .or(just("|").to(Pipe))
        .or(just("@@").to(AtAt))
        .or(just("@").to(At))
        .or(just("{{").to(LcurlCurl))
        .or(just("}}").to(RcurlCurl))
        .or(just("{").to(Lcurl))
        .or(just("}").to(Rcurl))
        .or(just(",").to(Comma))
        .or(just("~").to(Tilde))
        .or(just("+").to(Plus))
        .or(just("-").to(Minus))
        .or(just("/").to(Slash))
        .or(just("(").to(Lparen))
        .or(just(")").to(Rparen))
        .or(just("#").to(Hash))
        .or(just("*").to(Asterisk))
        .or(just(":").to(Colon))
        //
        // flag characters other than * #
        .or(one_of("!&?%").map(OtherFlag))
        .or(just("'")
            .then(any().filter(char::is_ascii_uppercase))
            .map(|(_, c)| OtherFlag(c)))
        //
        // other keywords
        .or(keyword("txn").to(Txn))
        .or(keyword("balance").to(Balance))
        .or(keyword("open").to(Open))
        .or(keyword("close").to(Close))
        .or(keyword("commodity").to(Commodity))
        .or(keyword("pad").to(Pad))
        .or(keyword("event").to(Event))
        .or(keyword("query").to(Query))
        .or(keyword("custom").to(Custom))
        .or(keyword("price").to(Price))
        .or(keyword("note").to(Note))
        .or(keyword("document").to(Document))
        .or(keyword("pushtag").to(Pushtag))
        .or(keyword("poptag").to(Poptag))
        .or(keyword("pushmeta").to(Pushmeta))
        .or(keyword("popmeta").to(Popmeta))
        .or(keyword("option").to(Option))
        .or(keyword("options").to(Options))
        .or(keyword("plugin").to(Plugin))
        .or(keyword("include").to(Include))
        //
        // date/time or bare date
        .or(date()
            .then_ignore(inline_whitespace())
            .then(time())
            .then_ignore(end_of_word())
            .map(|(d, t)| DateTime(NaiveDateTime::new(d, t))))
        .or(date().then_ignore(end_of_word()).map(Date))
}

fn date<'src>() -> impl Parser<'src, &'src str, NaiveDate, extra::Err<Rich<'src, char, Span>>> {
    const DATE_SEP: &str = "-/";

    digit()
        .repeated()
        .exactly(4)
        .map_slice(|s| s.parse::<i32>().unwrap())
        .then(one_of(DATE_SEP))
        .then(
            digit()
                .repeated()
                .exactly(2)
                .map_slice(|s| s.parse::<u32>().unwrap()),
        )
        .then(one_of(DATE_SEP))
        .then(
            digit()
                .repeated()
                .exactly(2)
                .map_slice(|s| s.parse::<u32>().unwrap()),
        )
        .try_map(|((((year, _), month), _), day), span| {
            NaiveDate::from_ymd_opt(year, month, day)
                .ok_or(chumsky::error::Rich::custom(span, "date out of range"))
        })
}

fn time<'src>() -> impl Parser<'src, &'src str, NaiveTime, extra::Err<Rich<'src, char, Span>>> {
    const TIME_SEP: char = ':';

    digit()
        .repeated()
        .at_least(1)
        .at_most(2)
        .map_slice(|s| s.parse::<u32>().unwrap())
        .then(one_of(TIME_SEP))
        .then(
            digit()
                .repeated()
                .exactly(2)
                .map_slice(|s| s.parse::<u32>().unwrap()),
        )
        .then(
            just(TIME_SEP)
                .then(
                    digit()
                        .repeated()
                        .exactly(2)
                        .map_slice(|s| s.parse::<u32>().unwrap()),
                )
                .map(|(_, sec)| sec)
                .or_not(),
        )
        .try_map(|(((hour, _), min), sec), span| {
            NaiveTime::from_hms_opt(hour, min, sec.unwrap_or(0))
                .ok_or(chumsky::error::Rich::custom(span, "time out of range"))
        })
}

fn digit<'src>() -> impl Parser<'src, &'src str, char, extra::Err<Rich<'src, char, Span>>> {
    any().filter(char::is_ascii_digit)
}

/// match only at the end of a word, without consuming what comes next
fn end_of_word<'src>() -> impl Parser<'src, &'src str, (), extra::Err<Rich<'src, char, Span>>> {
    any()
        .filter(|c: &char| !c.is_alphanumeric() && *c != '_')
        .ignored()
        .or(end())
        .rewind()
}

pub type Span = SimpleSpan<usize>;

mod tests;
