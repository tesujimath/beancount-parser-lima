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
    Account(Account),
    StringLiteral(String),
    Number(Decimal),
    Tag(super::Tag),
    Link(super::Link),
}

pub fn lexer<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char, Span>>> {
    use Token::*;

    // keywords which look like currencies, so must be first
    keyword("TRUE")
        .to(True)
        .or(keyword("FALSE").to(False))
        .or(keyword("NULL").to(Null))
        //
        // currency
        .or(currency().map(Currency))
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
        //
        .or(account().map(Account))
        //
        .or(string_literal().map(StringLiteral))
        //
        .or(number().map(Number))
        //
        .or(tag().map(Tag))
        .or(link().map(Link))
}

fn currency<'src>() -> impl Parser<'src, &'src str, Currency, extra::Err<Rich<'src, char, Span>>> {
    regex(r"[A-Z][A-Z0-9'\._-]*[A-Z0-9]?\b|/[A-Z0-9'\._-]*[A-Z]([A-Z0-9'\._-]*[A-Z0-9])?").try_map(
        |s: &str, span| {
            s.parse::<super::Currency>()
                .map_err(|e| chumsky::error::Rich::custom(span, e))
        },
    )
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

/// Matches `Account`.
pub fn account<'src>() -> impl Parser<'src, &'src str, Account, extra::Err<Rich<'src, char, Span>>>
{
    account_type()
        .then(
            just(':')
                .ignore_then(account_name())
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .map(|(acc_type, names)| {
            Account::new(acc_type, NonEmpty::collect(names.into_iter()).unwrap())
        })
}

fn account_type<'src>(
) -> impl Parser<'src, &'src str, AccountType, extra::Err<Rich<'src, char, Span>>> {
    regex(r"[\p{Lu}\p{Lo}][\p{L}\p{N}\-]*").try_map(|s: &str, span| {
        s.parse::<AccountType>()
            .map_err(|e| chumsky::error::Rich::custom(span, e))
    })
}

fn account_name<'src>(
) -> impl Parser<'src, &'src str, AccountName, extra::Err<Rich<'src, char, Span>>> {
    regex(r"[\p{Lu}\p{Lo}\p{N}][\p{L}\p{N}\-]*").try_map(|s: &str, span| {
        s.parse::<AccountName>()
            .map_err(|e| chumsky::error::Rich::custom(span, e))
    })
}

/// Matches a quoted string supporting embedded newlines and character escapes for `\\`, `\"`, `\n`, `\t`.
fn string_literal<'src>() -> impl Parser<'src, &'src str, String, extra::Err<Rich<'src, char, Span>>>
{
    regex(r#""([^\\"]|\\.)*""#).try_map(|s: &str, span| {
        if s.len() >= 16384 {
            Err(chumsky::error::Rich::custom(span, "string too long"))
        } else {
            let content = &s[1..s.len() - 1];
            if content.contains('\\') {
                Ok(content
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\\"", "\"")
                    .replace("\\\\", "\\"))
            } else {
                Ok(content.to_owned())
            }
        }
    })
}

fn number<'src>() -> impl Parser<'src, &'src str, Decimal, extra::Err<Rich<'src, char, Span>>> {
    digit()
        .repeated()
        .at_least(1)
        .then((just(',').then(digit().repeated().exactly(3))).repeated())
        .then((just('.').then(digit().repeated().at_least(1))).or_not())
        .slice()
        .try_map(|s: &'src str, span| {
            let mut without_commas = s.to_string();
            without_commas.retain(|c| c != ',');
            FromStr::from_str(&without_commas).map_err(|e| chumsky::error::Rich::custom(span, e))
        })
}

fn tag<'src>() -> impl Parser<'src, &'src str, Tag, extra::Err<Rich<'src, char, Span>>> {
    regex(r"#[A-Za-z0-9\-_/.]+").try_map(|s: &str, span| {
        s[1..]
            .parse::<TagOrLinkIdentifier>()
            .map(super::Tag)
            .map_err(|e| chumsky::error::Rich::custom(span, e))
    })
}

fn link<'src>() -> impl Parser<'src, &'src str, Link, extra::Err<Rich<'src, char, Span>>> {
    regex(r"\^[A-Za-z0-9\-_/.]+").try_map(|s: &str, span| {
        s[1..]
            .parse::<TagOrLinkIdentifier>()
            .map(super::Link)
            .map_err(|e| chumsky::error::Rich::custom(span, e))
    })
}

fn key<'src>() -> impl Parser<'src, &'src str, Key, extra::Err<Rich<'src, char, Span>>> {
    regex(r"[a-z][a-zA-Z0-9\-_]+")
        .then_ignore(just(':').rewind())
        .try_map(|s: &str, span| {
            s[1..]
                .parse::<Key>()
                .map_err(|e| chumsky::error::Rich::custom(span, e))
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
