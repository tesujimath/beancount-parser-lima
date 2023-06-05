// TODO remove suppression for dead code warning
#![allow(dead_code)]

use std::iter::once;

use super::*;
use either::Either;
use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, take_while1},
    character::complete::{anychar, one_of, satisfy, space0},
    combinator::{map, map_res, opt, recognize, value},
    multi::{many0, many0_count},
    sequence::{delimited, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
use nom_supreme::{
    error::{BaseErrorKind, ErrorTree},
    tag::complete::tag as sym, // beancount grammar has its own tag
};
use nom_tracable::TracableInfo;

type Span<'a> = LocatedSpan<&'a str, TracableInfo>;

/// Matches `Account`.
pub fn account(i: Span) -> IResult<Span, Account, ErrorTree<Span>> {
    map_res(
        tuple((
            account_type,
            sym(":"),
            sub_account,
            many0(tuple((sym(":"), sub_account))),
        )),
        |(acc_type, _colon, sub, colon_sub_pairs)| {
            Account::new(
                acc_type,
                once(sub)
                    .chain(colon_sub_pairs.into_iter().map(|(_colon, sub)| sub))
                    .collect(),
            )
        },
    )(i)
}

/// Matches `AccountType`.
pub fn account_type(i: Span) -> IResult<Span, AccountType, ErrorTree<Span>> {
    alt((
        map(sym("Assets"), |_| AccountType::Assets),
        map(sym("Liabilities"), |_| AccountType::Liabilities),
        map(sym("Equity"), |_| AccountType::Equity),
        map(sym("Income"), |_| AccountType::Income),
        map(sym("Expenses"), |_| AccountType::Expenses),
    ))(i)
}

/// Matches `SubAccount`.
pub fn sub_account(i: Span) -> IResult<Span, SubAccount, ErrorTree<Span>> {
    map_res(
        recognize(tuple((
            satisfy(|c| SubAccount::is_valid_initial(&c)),
            many0_count(satisfy(|c| SubAccount::is_valid_subsequent(&c))),
        ))),
        |s: Span| s.parse::<SubAccount>(),
    )(i)
}

/// Matches the `txn` keyword or a flag.
pub fn txn(i: Span) -> IResult<Span, Flag, ErrorTree<Span>> {
    alt((map(sym("txn"), |_| Flag::Asterisk), flag))(i)
}

/// Matches any flag, including asterisk or hash.
pub fn flag(i: Span) -> IResult<Span, Flag, ErrorTree<Span>> {
    alt((
        map(sym("*"), |_| Flag::Asterisk),
        map(sym("#"), |_| Flag::Hash),
        map(sym("!"), |_| Flag::Exclamation),
        map(sym("&"), |_| Flag::Exclamation),
        map(sym("?"), |_| Flag::Question),
        map(sym("%"), |_| Flag::Percent),
        map_res(tuple((sym("'"), anychar)), |(_, c)| {
            FlagLetter::try_from(c).map(Flag::Letter)
        }),
    ))(i)
}

/// Matches zero or more quoted strings, optionally separated by whitespace.
pub fn txn_strings(i: Span) -> IResult<Span, Vec<String>, ErrorTree<Span>> {
    match opt(tuple((string, many0(tuple((space0, string))))))(i)? {
        (i, Some((s1, v))) => Ok((i, once(s1).chain(v.into_iter().map(|(_, s)| s)).collect())),
        (i, None) => Ok((i, Vec::new())),
    }
}

/// Matches zero or more tags or links, optionally separated by whitespace.
pub fn tags_links(i: Span) -> IResult<Span, Vec<Either<Tag, Link>>, ErrorTree<Span>> {
    match opt(tuple((tag_or_link, many0(tuple((space0, tag_or_link))))))(i)? {
        (i, Some((s1, v))) => Ok((i, once(s1).chain(v.into_iter().map(|(_, s)| s)).collect())),
        (i, None) => Ok((i, Vec::new())),
    }
}

/// Matches a tag or a link.
pub fn tag_or_link(i: Span) -> IResult<Span, Either<Tag, Link>, ErrorTree<Span>> {
    use Either::*;
    alt((map(tag, Left), map(link, Right)))(i)
}

/// Matches a tag.
pub fn tag(i: Span) -> IResult<Span, Tag, ErrorTree<Span>> {
    let (i, _) = sym("#")(i)?;
    map(tag_or_link_identifier, Tag::from)(i)
}

/// Matches a link.
pub fn link(i: Span) -> IResult<Span, Link, ErrorTree<Span>> {
    let (i, _) = sym("^")(i)?;
    map(tag_or_link_identifier, Link::from)(i)
}

pub fn tag_or_link_identifier(i: Span) -> IResult<Span, TagOrLinkIdentifier, ErrorTree<Span>> {
    map_res(
        take_while1(|c: char| TagOrLinkIdentifier::is_valid_char(&c)),
        |s: Span| TagOrLinkIdentifier::from_str(&s),
    )(i)
}

pub fn date(i0: Span) -> IResult<Span, NaiveDate, ErrorTree<Span>> {
    fn date_sep(i: Span) -> IResult<Span, char, ErrorTree<Span>> {
        one_of("-/")(i).map(|(i, c)| (i, c))
    }

    let (i, (year, year_len)) = map_res(take_while1(|c: char| c.is_ascii_digit()), |s: Span| {
        s.parse::<i32>().map(|y| (y, s.len()))
    })(i0)?;
    if year_len < 4 {
        return Err(ParseError::nom_error(
            i0,
            ParseErrorReason::DateMissingCentury,
        ));
    }
    let (i, first_date_sep) = date_sep(i)?;
    let (i, month) = map_res(take_while1(|c: char| c.is_ascii_digit()), |s: Span| {
        s.parse::<u32>()
    })(i)?;
    let (i, _) = satisfy(|c| c == first_date_sep)(i)?;
    let (i, day) = map_res(take_while1(|c: char| c.is_ascii_digit()), |s: Span| {
        s.parse::<u32>()
    })(i)?;

    match NaiveDate::from_ymd_opt(year, month, day) {
        Some(d) => Ok((i, d)),
        None => Err(ParseError::nom_error(i0, ParseErrorReason::DateOutOfRange)),
    }
}

/// Matches a quoted string supporting embedded newlines and character escapes for `\\`, `\"`, `\n`, `\t`.
pub fn string(i: Span) -> IResult<Span, String, ErrorTree<Span>> {
    fn string_content(i: Span) -> IResult<Span, String, ErrorTree<Span>> {
        escaped_transform(
            take_while1(|c| c != '\\' && c != '"'),
            '\\',
            alt((
                value("\\", sym("\\")),
                value("\"", sym("\"")),
                value("\n", sym("n")),
                value("\t", sym("t")),
            )),
        )(i)
    }

    delimited(sym("\""), string_content, sym("\""))(i)
}

#[derive(Debug)]
enum ParseErrorReason {
    DateMissingCentury,
    DateOutOfRange,
}

#[derive(Debug)]
pub struct ParseError {
    reason: ParseErrorReason,
}

impl ParseError {
    fn nom_error(location: Span, reason: ParseErrorReason) -> nom::Err<ErrorTree<Span>> {
        nom::Err::Error(ErrorTree::Base {
            location,
            kind: BaseErrorKind::External(Box::new(ParseError { reason })),
        })
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.reason {
            ParseErrorReason::DateMissingCentury => write!(f, "date requires century"),
            ParseErrorReason::DateOutOfRange => write!(f, "date out of range"),
        }
    }
}

impl Error for ParseError {}

mod expr;
mod tests;
