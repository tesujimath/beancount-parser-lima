// TODO remove suppression for dead code warning
#![allow(dead_code)]

use super::*;
use nom::{
    branch::alt,
    bytes::complete::take_while1,
    character::complete::{anychar, none_of, one_of},
    combinator::{map, map_res, recognize},
    multi::many0_count,
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

pub fn txn(i: &str) -> IResult<&str, Flag, ErrorTree<&str>> {
    alt((
        map(tag("txn"), |_| Flag::Asterisk),
        map(tag("*"), |_| Flag::Asterisk),
        flag,
    ))(i)
}

pub fn string(i: &str) -> IResult<&str, &str, ErrorTree<&str>> {
    delimited(tag("\""), recognize(many0_count(none_of("\""))), tag("\""))(i)
}

mod tests;
