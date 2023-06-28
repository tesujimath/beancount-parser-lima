use std::iter::once;

use super::*;
use chrono::{NaiveDateTime, NaiveTime};
use chumsky::{
    prelude::*,
    span::SimpleSpan,
    text::{inline_whitespace, keyword},
};
use std::string::ToString;

#[derive(Clone, Debug)]
pub enum Token<'a> {
    // invisible tokens for line-oriented parsing
    Indent,
    Eol,

    // visible tokens
    True,
    False,
    Null,
    Currency(super::Currency<'a>),
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
    DedicatedFlag(Flag),
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
    Account(Account<'a>),
    StringLiteral(String),
    Number(Decimal),
    Tag(super::Tag),
    Link(super::Link),
    Key(super::Key),
}

// TODO remove this temporary diagnostic
pub fn dump_tokens(s: &str) {
    match lexer().parse(s).into_result() {
        Ok(tok_spans) => {
            for (tok, _span) in tok_spans {
                use Token::*;

                match tok {
                    Indent => print!("    "),
                    Eol => println!(),
                    _ => print!("{:?} ", tok),
                }
            }
        }
        Err(e) => println!("failed: {:?}", e),
    };
}

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    line().foldl(line().repeated(), |mut all, mut next_line| {
        all.append(&mut next_line);
        all
    })
}

/// match a whole lines worth of visible tokens, up until `Eol` or `end()`
fn line<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    use Token::*;

    // skip comments to end-of-line
    fn comment_to_end_of_line<'src>(
    ) -> impl Parser<'src, &'src str, (), extra::Err<Rich<'src, char, Span>>> {
        just(";")
            .then(any().filter(|c: &char| *c != '\n').repeated())
            .then(end_of_line())
            .ignored()
    }

    group((
        nonempty_inline_whitespace()
            .to(Indent)
            .map_with_span(|tok, span| (tok, span))
            .or_not(),
        visible_token()
            .map_with_span(|tok, span| (tok, span))
            .padded_by(inline_whitespace())
            .repeated()
            .collect::<Vec<_>>()
            .then_ignore(comment_to_end_of_line().or_not()),
        just('\n').to(Eol).map_with_span(|tok, span| (tok, span)),
    ))
    .map(|(indent, tokens, eol)| {
        indent
            .into_iter()
            .chain(tokens.into_iter().chain(once(eol)))
            .collect::<Vec<_>>()
    })
}

/// match a single visible token
fn visible_token<'src>(
) -> impl Parser<'src, &'src str, Token<'src>, extra::Err<Rich<'src, char, Span>>> {
    use Token::*;

    choice((
        // keywords which look like currencies, so must be first
        choice((
            keyword("TRUE").to(True),
            keyword("FALSE").to(False),
            keyword("NULL").to(Null),
        )),
        //
        // currency
        currency().map(Currency),
        //
        // special characters
        choice((
            just("|").to(Pipe),
            just("@@").to(AtAt),
            just("@").to(At),
            just("{{").to(LcurlCurl),
            just("}}").to(RcurlCurl),
            just("{").to(Lcurl),
            just("}").to(Rcurl),
            just(",").to(Comma),
            just("~").to(Tilde),
            just("+").to(Plus),
            just("-").to(Minus),
            just("/").to(Slash),
            just("(").to(Lparen),
            just(")").to(Rparen),
            just("#").to(Hash),
            just("*").to(Asterisk),
            just(":").to(Colon),
        )),
        //
        // flag characters other than * #
        choice((
            just("!").to(DedicatedFlag(Flag::Exclamation)),
            just("&").to(DedicatedFlag(Flag::Ampersand)),
            just("?").to(DedicatedFlag(Flag::Question)),
            just("%").to(DedicatedFlag(Flag::Percent)),
            just("'")
                .ignore_then(any().filter(char::is_ascii_uppercase))
                .try_map(|c, span| {
                    FlagLetter::try_from(c)
                        .map(Flag::Letter)
                        .map_err(|e| chumsky::error::Rich::custom(span, e))
                        .map(DedicatedFlag)
                }),
        )),
        //
        // other keywords
        choice((
            keyword("txn").to(Txn),
            keyword("balance").to(Balance),
            keyword("open").to(Open),
            keyword("close").to(Close),
            keyword("commodity").to(Commodity),
            keyword("pad").to(Pad),
            keyword("event").to(Event),
            keyword("query").to(Query),
            keyword("custom").to(Custom),
            keyword("price").to(Price),
            keyword("note").to(Note),
            keyword("document").to(Document),
            keyword("pushtag").to(Pushtag),
            keyword("poptag").to(Poptag),
            keyword("pushmeta").to(Pushmeta),
            keyword("popmeta").to(Popmeta),
            keyword("option").to(Option),
            keyword("options").to(Options),
            keyword("plugin").to(Plugin),
            keyword("include").to(Include),
        )),
        //
        // date/time or bare date
        choice((
            date()
                .then_ignore(inline_whitespace())
                .then(time().then_ignore(end_of_word()))
                .map(|(d, t)| DateTime(NaiveDateTime::new(d, t))),
            date().then_ignore(end_of_word()).map(Date),
        )),
        //
        account().map(Account),
        //
        string_literal().map(StringLiteral),
        //
        number().map(Number),
        //
        tag().map(Tag),
        link().map(Link),
        key().map(Key),
    ))
}

fn currency<'src>(
) -> impl Parser<'src, &'src str, Currency<'src>, extra::Err<Rich<'src, char, Span>>> {
    regex(r"[A-Z][A-Z0-9'\._-]*[A-Z0-9]?\b|/[A-Z0-9'\._-]*[A-Z]([A-Z0-9'\._-]*[A-Z0-9])?").try_map(
        |s: &str, span| {
            super::Currency::try_from(s).map_err(|e| chumsky::error::Rich::custom(span, e))
        },
    )
}

fn date<'src>() -> impl Parser<'src, &'src str, NaiveDate, extra::Err<Rich<'src, char, Span>>> {
    const DATE_SEP: &str = "-/";

    digit()
        .repeated()
        .exactly(4)
        .map_slice(|s| s.parse::<i32>().unwrap())
        .then_ignore(one_of(DATE_SEP))
        .then(
            digit()
                .repeated()
                .exactly(2)
                .map_slice(|s| s.parse::<u32>().unwrap()),
        )
        .then_ignore(one_of(DATE_SEP))
        .then(
            digit()
                .repeated()
                .exactly(2)
                .map_slice(|s| s.parse::<u32>().unwrap()),
        )
        .try_map(|((year, month), day), span| {
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
        .then_ignore(one_of(TIME_SEP))
        .then(
            digit()
                .repeated()
                .exactly(2)
                .map_slice(|s| s.parse::<u32>().unwrap()),
        )
        .then(
            just(TIME_SEP)
                .ignore_then(
                    digit()
                        .repeated()
                        .exactly(2)
                        .map_slice(|s| s.parse::<u32>().unwrap()),
                )
                .or_not(),
        )
        .try_map(|((hour, min), sec), span| {
            NaiveTime::from_hms_opt(hour, min, sec.unwrap_or(0))
                .ok_or(chumsky::error::Rich::custom(span, "time out of range"))
        })
}

/// Matches `Account`.
pub fn account<'src>(
) -> impl Parser<'src, &'src str, Account<'src>, extra::Err<Rich<'src, char, Span>>> {
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
) -> impl Parser<'src, &'src str, AccountName<'src>, extra::Err<Rich<'src, char, Span>>> {
    regex(r"[\p{Lu}\p{Lo}\p{N}][\p{L}\p{N}\-]*").try_map(|s: &str, span| {
        AccountName::try_from(s).map_err(|e| chumsky::error::Rich::custom(span, e))
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
            s.parse::<Key>()
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

/// match only at the end of a line, without consuming the newline itself
fn end_of_line<'src>() -> impl Parser<'src, &'src str, (), extra::Err<Rich<'src, char, Span>>> {
    just("\n").ignored().or(end()).rewind()
}

fn nonempty_inline_whitespace<'src>(
) -> impl Parser<'src, &'src str, (), extra::Err<Rich<'src, char, Span>>> {
    fn is_inline_whitespace(c: &char) -> bool {
        *c == ' ' || *c == '\t'
    }

    any()
        .filter(is_inline_whitespace)
        .ignored()
        .repeated()
        .at_least(1)
}

pub type Span = SimpleSpan<usize>;

mod tests;
