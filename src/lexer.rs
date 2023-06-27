use super::*;
use chrono::NaiveTime;
use chumsky::{
    prelude::*,
    span::SimpleSpan,
    text::{inline_whitespace, keyword},
};

#[derive(Clone)]
pub enum Token {
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
    Flag(char),
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
}

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char, Span>>> {
    use Token::*;

    regex(r"[A-Z][A-Z0-9\'\.\_\-]*[A-Z0-9]?\b|\/[A-Z0-9\'\.\_\-]*[A-Z]([A-Z0-9\'\.\_\-]*[A-Z0-9])?")
        .try_map(|s: &str, span| {
            s.parse::<super::Currency>()
                .map(Currency)
                .map_err(|e| chumsky::error::Rich::custom(span, e))
        })
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
        .or(one_of("!&?%").map(Flag))
        .or(just("'")
            .then(any().filter(char::is_ascii_uppercase))
            .map(|(_, c)| Flag(c)))
        .or(just("txn").to(Txn))
        .or(just("balance").to(Balance))
        .or(just("open").to(Open))
        .or(just("close").to(Close))
        .or(just("commodity").to(Commodity))
        .or(just("pad").to(Pad))
        .or(just("event").to(Event))
        .or(just("query").to(Query))
        .or(just("custom").to(Custom))
        .or(just("price").to(Price))
        .or(just("note").to(Note))
        .or(just("document").to(Document))
        .or(just("pushtag").to(Pushtag))
        .or(just("poptag").to(Poptag))
        .or(just("pushmeta").to(Pushmeta))
        .or(just("popmeta").to(Popmeta))
        .or(just("option").to(Option))
        .or(just("options").to(Options))
        .or(just("plugin").to(Plugin))
        .or(just("include").to(Include))
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

    // const date_re: &str = r"\d{4}[\-/]\d{2}[\-/]\d{2}"
    // const time_re: &str = r"\d{1,2}:\d{2}(:\d{2})?"
}

fn digit<'src>() -> impl Parser<'src, &'src str, char, extra::Err<Rich<'src, char, Span>>> {
    any().filter(char::is_ascii_digit)
}

pub type Span = SimpleSpan<usize>;

mod tests;
