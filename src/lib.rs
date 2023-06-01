use chrono::NaiveDate;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Default, Debug)]
pub enum Flag {
    #[default]
    Asterisk,
    Exclamation,
    Ampersand,
    Hash,
    Question,
    Percent,
    Letter(UppercaseAsciiChar),
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

#[derive(PartialEq, Eq, Debug)]
pub struct Tag(TagOrLinkIdentifier);

impl From<TagOrLinkIdentifier> for Tag {
    fn from(id: TagOrLinkIdentifier) -> Self {
        Self(id)
    }
}

impl TryFrom<&str> for Tag {
    type Error = TagOrLinkIdentifierError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        TagOrLinkIdentifier::try_from(s).map(Tag)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Link(TagOrLinkIdentifier);

impl From<TagOrLinkIdentifier> for Link {
    fn from(id: TagOrLinkIdentifier) -> Self {
        Self(id)
    }
}

impl TryFrom<&str> for Link {
    type Error = TagOrLinkIdentifierError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        TagOrLinkIdentifier::try_from(s).map(Link)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct TagOrLinkIdentifier(String);

/// The valid characters for tags and links besides alphanumeric.
const TAG_OR_LINK_EXTRA_CHARS: [char; 4] = ['-', '_', '/', '.'];

impl TagOrLinkIdentifier {
    pub fn is_valid_char(c: &char) -> bool {
        c.is_alphanumeric() || TAG_OR_LINK_EXTRA_CHARS.contains(c)
    }
}

#[derive(Debug)]
pub struct TagOrLinkIdentifierError(char);

impl Display for TagOrLinkIdentifierError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "invalid character for tag or link: '{}'", self.0)
    }
}

impl Error for TagOrLinkIdentifierError {}

impl TryFrom<&str> for TagOrLinkIdentifier {
    type Error = TagOrLinkIdentifierError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s.chars().find(|c| !(TagOrLinkIdentifier::is_valid_char(c))) {
            None => Ok(TagOrLinkIdentifier(s.to_owned())),
            Some(bad) => Err(TagOrLinkIdentifierError(bad)),
        }
    }
}

mod parser;
