// TODO remove suppression for dead code warning
#![allow(dead_code)]

use std::iter::once;

use super::*;
use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, take_while1},
    character::complete::{anychar, one_of, space0},
    combinator::{map, map_res, opt, value},
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};
use nom_supreme::{
    error::{BaseErrorKind, ErrorTree},
    tag::complete::tag,
};

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
    fn nom_error(location: &str, reason: ParseErrorReason) -> nom::Err<ErrorTree<&str>> {
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

pub fn date(i0: &str) -> IResult<&str, NaiveDate, ErrorTree<&str>> {
    fn date_sep(i: &str) -> IResult<&str, (), ErrorTree<&str>> {
        one_of("-/")(i).map(|(s, _)| (s, ()))
    }

    let (i, (year, year_len)) = map_res(take_while1(|c: char| c.is_ascii_digit()), |s: &str| {
        s.parse::<i32>().map(|y| (y, s.len()))
    })(i0)?;
    if year_len < 4 {
        return Err(ParseError::nom_error(
            i0,
            ParseErrorReason::DateMissingCentury,
        ));
    }
    let (i, _) = date_sep(i)?;
    let (i, month) = map_res(take_while1(|c: char| c.is_ascii_digit()), |s: &str| {
        s.parse::<u32>()
    })(i)?;
    let (i, _) = date_sep(i)?;
    let (i, day) = map_res(take_while1(|c: char| c.is_ascii_digit()), |s: &str| {
        s.parse::<u32>()
    })(i)?;

    match NaiveDate::from_ymd_opt(year, month, day) {
        Some(d) => Ok((i, d)),
        None => Err(ParseError::nom_error(i0, ParseErrorReason::DateOutOfRange)),
    }
}

pub fn flag(i: &str) -> IResult<&str, Flag, ErrorTree<&str>> {
    alt((
        map(tag("!"), |_| Flag::Exclamation),
        map(tag("&"), |_| Flag::Exclamation),
        map(tag("#"), |_| Flag::Hash),
        map(tag("?"), |_| Flag::Question),
        map(tag("%"), |_| Flag::Percent),
        map_res(tuple((tag("'"), anychar)), |(_, c)| {
            UppercaseAsciiChar::try_from(c).map(Flag::Letter)
        }),
    ))(i)
}

/// Matches the `txn` keyword or a flag.
pub fn txn(i: &str) -> IResult<&str, Flag, ErrorTree<&str>> {
    alt((
        map(tag("txn"), |_| Flag::Asterisk),
        map(tag("*"), |_| Flag::Asterisk),
        flag,
    ))(i)
}

/// Matches zero or more quoted strings, optionally separated by whitespace.
pub fn txn_strings(i: &str) -> IResult<&str, Vec<String>, ErrorTree<&str>> {
    match opt(tuple((string, many0(tuple((space0, string))))))(i)? {
        (i, Some((s1, v))) => Ok((i, once(s1).chain(v.into_iter().map(|(_, s)| s)).collect())),
        (i, None) => Ok((i, Vec::new())),
    }
}

/// Matches a quoted string supporting embedded newlines and character escapes for `\\`, `\"`, `\n`, `\t`.
pub fn string(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
    fn string_content(i: &str) -> IResult<&str, String, ErrorTree<&str>> {
        escaped_transform(
            take_while1(|c| c != '\\' && c != '"'),
            '\\',
            alt((
                value("\\", tag("\\")),
                value("\"", tag("\"")),
                value("\n", tag("n")),
                value("\t", tag("t")),
            )),
        )(i)
    }

    delimited(tag("\""), string_content, tag("\""))(i)
}

mod tests;
