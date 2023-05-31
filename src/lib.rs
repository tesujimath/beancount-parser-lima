use chrono::NaiveDate;
use nom::{
    bytes::complete::{tag, take, take_while_m_n},
    combinator::map_res,
    error::{ErrorKind, FromExternalError},
    IResult,
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

pub fn date(i0: &str) -> IResult<&str, NaiveDate> {
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
        None => Err(nom::Err::Error(nom::error::Error::from_external_error(
            i0, // TODO this is wrong, needs to be original i not final
            ErrorKind::Fail,
            ParseError {
                reason: ParseErrorReason::DateOutOfRange,
            },
        ))), //None => Err(parse_error(i, ParseErrorReason::DateOutOfRange)),
    }
}

pub fn date2(i: &str) -> IResult<&str, NaiveDate> {
    // this isn't as good, as we lean too heavily on parse_from_str
    map_res(take(10usize), |s| NaiveDate::parse_from_str(s, "%Y-%m-%d"))(i)
}

mod tests;
