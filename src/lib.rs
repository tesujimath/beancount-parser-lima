use chrono::NaiveDate;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};
use strum_macros::Display;

#[derive(PartialEq, Eq, Display, Debug)]
pub enum AccountType {
    Assets,
    Liabilities,
    Equity,
    Income,
    Expenses,
}

#[derive(PartialEq, Eq, Default, Debug)]
pub enum Flag {
    #[default]
    Asterisk,
    Exclamation,
    Ampersand,
    Hash,
    Question,
    Percent,
    Letter(FlagLetter),
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
            Letter(FlagLetter(c)) => (Some('\''), *c),
        };

        match prefix {
            Some(prefix) => write!(f, "{}{}", prefix, c),
            None => write!(f, "{}", c),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct FlagLetter(char);

impl FlagLetter {
    pub fn is_valid(c: &char) -> bool {
        c.is_ascii_uppercase()
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct FlagLetterError(char);

impl Display for FlagLetterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "invalid character '{}' for flag letter - must be uppercase ASCII",
            self.0
        )
    }
}

impl Error for FlagLetterError {}

impl TryFrom<char> for FlagLetter {
    type Error = FlagLetterError;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        if FlagLetter::is_valid(&c) {
            Ok(FlagLetter(c))
        } else {
            Err(FlagLetterError(c))
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

impl Display for Tag {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0 .0)
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

impl Display for Link {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "^{}", self.0 .0)
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

#[derive(PartialEq, Eq, Debug)]
pub struct TagOrLinkIdentifierError(Vec<char>);

impl Display for TagOrLinkIdentifierError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "invalid characters {} for tag or link identifier - must be alphanumeric or one of {}",
            itertools::Itertools::intersperse(
                self.0.iter().map(|c| format!("'{}'", c)),
                ", ".to_string()
            )
            .collect::<String>(),
            itertools::Itertools::intersperse(
                TAG_OR_LINK_EXTRA_CHARS.iter().map(|c| format!("'{}'", c)),
                ", ".to_string()
            )
            .collect::<String>()
        )
    }
}

impl Error for TagOrLinkIdentifierError {}

impl TryFrom<&str> for TagOrLinkIdentifier {
    type Error = TagOrLinkIdentifierError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let bad_chars = s
            .chars()
            .filter_map(|c| (!TagOrLinkIdentifier::is_valid_char(&c)).then_some(c))
            .collect::<Vec<char>>();
        if bad_chars.is_empty() {
            Ok(TagOrLinkIdentifier(s.to_owned()))
        } else {
            Err(TagOrLinkIdentifierError(bad_chars))
        }
    }
}

mod parser;
mod tests;
