use chumsky::{
    extra::ParserExtra,
    input::{Input, MapExtra},
};
use std::{
    fmt::{self, Display, Formatter},
    hash::{Hash, Hasher},
};
use strum_macros::{Display, EnumString};

use crate::parser::SourceId;

#[derive(PartialEq, Eq, Hash, Clone, Copy, EnumString, Display, Debug)]
pub enum AccountType {
    Assets,
    Liabilities,
    Equity,
    Income,
    Expenses,
}
#[derive(PartialEq, Eq, Default, Clone, Copy, Debug)]
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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
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

impl std::error::Error for FlagLetterError {}

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

/// The booking method for an account
#[derive(EnumString, PartialEq, Eq, Default, Clone, Copy, Display, Debug)]
#[strum(serialize_all = "SCREAMING_SNAKE_CASE")]
pub enum Booking {
    #[default]
    Strict,
    StrictWithSize,
    None,
    Average,
    Fifo,
    Lifo,
    Hifo,
}

/// Our span type
pub type Span = chumsky::span::SimpleSpan<usize, SourceId>;

/// A Spanned item may be located within a source file if the file path is known.
/// The span is invisible with respect to equality and hashing.
#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub(crate) item: T,
    pub(crate) span: Span,
}

pub fn spanned<T>(item: T, span: Span) -> Spanned<T> {
    Spanned { item, span }
}

/// for use with `map_with`
pub fn spanned_extra<'a, 'b, T, I, E>(item: T, e: &mut MapExtra<'a, 'b, I, E>) -> Spanned<T>
where
    I: Input<'a, Span = Span>,
    E: ParserExtra<'a, I>,
{
    Spanned {
        item,
        span: e.span(),
    }
}

impl<T> Spanned<T> {
    pub fn item(&self) -> &T {
        &self.item
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            item: &self.item,
            span: self.span,
        }
    }

    pub fn map<U, F>(&self, f: F) -> Spanned<U>
    where
        F: FnOnce(&T) -> U,
    {
        Spanned {
            item: f(&self.item),
            span: self.span,
        }
    }
}

impl<T> PartialEq for Spanned<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.item.eq(&other.item)
    }
}

impl<T> Eq for Spanned<T> where T: Eq {}

impl<T> Hash for Spanned<T>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.item.hash(state)
    }
}

impl<T> Copy for Spanned<T> where T: Copy {}

impl<T> Display for Spanned<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.item,)
    }
}

mod tests;
