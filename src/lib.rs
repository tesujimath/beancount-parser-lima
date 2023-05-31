use chrono::NaiveDate;
use nom::{
    branch::alt,
    bytes::complete::take_while_m_n,
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
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
enum ParseErrorReason {
    DateOutOfRange,
}

#[derive(Debug)]
pub struct ParseError {
    reason: ParseErrorReason,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.reason {
            ParseErrorReason::DateOutOfRange => write!(f, "date out of range"),
        }
    }
}

impl Error for ParseError {}

pub fn date(i0: &str) -> IResult<&str, NaiveDate, ErrorTree<&str>> {
    fn date_sep(i: &str) -> IResult<&str, (), ErrorTree<&str>> {
        one_of("-/")(i).map(|(s, _)| (s, ()))
    }

    let (i, year) = map_res(
        take_while_m_n(4, 4, |c: char| c.is_ascii_digit()),
        |s: &str| s.parse::<i32>(),
    )(i0)?;
    let (i, _) = date_sep(i)?;
    let (i, month) = map_res(
        take_while_m_n(2, 2, |c: char| c.is_ascii_digit()),
        |s: &str| s.parse::<u32>(),
    )(i)?;
    let (i, _) = date_sep(i)?;
    let (i, day) = map_res(
        take_while_m_n(2, 2, |c: char| c.is_ascii_digit()),
        |s: &str| s.parse::<u32>(),
    )(i)?;

    match NaiveDate::from_ymd_opt(year, month, day) {
        Some(d) => Ok((i, d)),
        // TODO build this via helper
        None => Err(nom::Err::Error(ErrorTree::Base {
            location: i0,
            kind: BaseErrorKind::External(Box::new(ParseError {
                reason: ParseErrorReason::DateOutOfRange,
            })),
        })), //None => Err(parse_error(i, ParseErrorReason::DateOutOfRange)),
    }
}

#[derive(Debug)]
pub struct UppercaseAsciiChar(char);

#[derive(Debug)]
pub struct UppercaseAsciiCharError;

impl Display for UppercaseAsciiCharError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "not uppercase ASCII")
    }
}

impl Error for UppercaseAsciiCharError {}

impl TryFrom<char> for UppercaseAsciiChar {
    type Error = UppercaseAsciiCharError;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        if c.is_ascii_uppercase() {
            Ok(UppercaseAsciiChar(c))
        } else {
            Err(UppercaseAsciiCharError)
        }
    }
}

pub enum Flag {
    Asterisk,
    Exclamation,
    Ampersand,
    Hash,
    Question,
    Percent,
    Letter(UppercaseAsciiChar),
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
