use chrono::NaiveDate;
use nom::{bytes::complete::take_while_m_n, combinator::map_res, IResult};
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
    let (i, year) = map_res(
        take_while_m_n(4, 4, |c: char| c.is_ascii_digit()),
        |s: &str| s.parse::<i32>(),
    )(i0)?;
    let (i, _) = tag("-")(i)?;
    let (i, month) = map_res(
        take_while_m_n(2, 2, |c: char| c.is_ascii_digit()),
        |s: &str| s.parse::<u32>(),
    )(i)?;
    let (i, _) = tag("-")(i)?;
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

mod tests;
