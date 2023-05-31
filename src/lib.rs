use chrono::NaiveDate;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
pub enum Flag {
    Asterisk,
    Exclamation,
    Ampersand,
    Hash,
    Question,
    Percent,
    Letter(UppercaseAsciiChar),
}

impl Default for Flag {
    fn default() -> Self {
        Flag::Asterisk
    }
}

impl Display for Flag {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Flag::*;
        let (prefix, c) = match self {
            Asterisk => (None, '*'),
            Exclamation => (None, '!'),
            Ampersand => (None, '&'),
            Hash => (None, '#'),
            Question => (None, '?'),
            Percent => (None, '%'),
            Letter(UppercaseAsciiChar(c)) => (Some('\''), *c),
        };

        match prefix {
            Some(prefix) => write!(f, "{}{}", prefix, c),
            None => write!(f, "{}", c),
        }
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

mod parser;
