// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]

use chrono::NaiveDate;
use nonempty::NonEmpty;
use rust_decimal::Decimal;
use std::{
    cmp::max,
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    iter::empty,
    str::FromStr,
};
use strum_macros::{Display, EnumString};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Account<'a> {
    account_type: AccountType,
    names: NonEmpty<AccountName<'a>>,
}

impl<'a> Account<'a> {
    pub fn new(account_type: AccountType, names: NonEmpty<AccountName>) -> Account {
        Account {
            account_type,
            names,
        }
    }
}

impl<'a> Display for Account<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}",
            self.account_type,
            itertools::Itertools::intersperse(
                self.names.iter().map(|s| format!("{}", s)),
                ":".to_string()
            )
            .collect::<String>()
        )
    }
}

#[derive(PartialEq, Eq, Display, Clone, EnumString, Debug)]
pub enum AccountType {
    Assets,
    Liabilities,
    Equity,
    Income,
    Expenses,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct AccountName<'a>(&'a str);

impl<'a> AccountName<'a> {
    pub fn is_valid_initial(c: &char) -> bool {
        c.is_ascii_uppercase() || c.is_ascii_digit()
    }

    pub fn is_valid_subsequent(c: &char) -> bool {
        c.is_alphanumeric() || *c == '-'
    }
}

impl<'a> Display for AccountName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct AccountNameError(AccountNameErrorKind);

#[derive(PartialEq, Eq, Debug)]
enum AccountNameErrorKind {
    Empty,
    Initial(char),
    Subsequent(Vec<char>),
}

impl Display for AccountNameError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AccountNameErrorKind::*;
        match &self.0 {
            Empty => write!(f, "empty account name"),
            Initial(bad_char) => write!(
                f,
                "invalid character '{}' for account name initial - must be uppercase ASCII letter or digit",
                bad_char
            ),
            Subsequent(bad_chars) => write!(f,
                                            "invalid characters {} for account name - must be alphanumeric or '-'",
                                            itertools::Itertools::intersperse(
                                                bad_chars.iter().map(|c| format!("'{}'", c)),
                                                ", ".to_string())
                                            .collect::<String>()),
        }
    }
}

impl Error for AccountNameError {}

impl<'a> TryFrom<&'a str> for AccountName<'a> {
    type Error = AccountNameError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        use AccountNameErrorKind::*;
        if s.is_empty() {
            Err(AccountNameError(Empty))
        } else {
            let mut chars = s.chars();
            let initial = chars.next().unwrap();
            if !AccountName::is_valid_initial(&initial) {
                Err(AccountNameError(Initial(initial)))
            } else {
                let bad_chars = chars
                    .filter_map(|c| (!AccountName::is_valid_subsequent(&c)).then_some(c))
                    .collect::<Vec<char>>();
                if bad_chars.is_empty() {
                    Ok(AccountName(s))
                } else {
                    Err(AccountNameError(Subsequent(bad_chars)))
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Currency<'a>(&'a str);

/// The valid intermediate characters for currency, in addition to ASCII uppercase and digits
const CURRENCY_INTERMEDIATE_EXTRA_CHARS: [char; 4] = ['\'', '.', '_', '-'];

impl<'a> Currency<'a> {
    fn is_valid_initial(c: &char) -> bool {
        c.is_ascii_uppercase() || *c == '/'
    }

    fn is_valid_intermediate(c: &char) -> bool {
        c.is_ascii_uppercase()
            || c.is_ascii_digit()
            || CURRENCY_INTERMEDIATE_EXTRA_CHARS.contains(c)
    }

    fn is_valid_final(c: &char) -> bool {
        c.is_ascii_uppercase() || c.is_ascii_digit()
    }
}

impl<'a> Display for Currency<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct CurrencyError(CurrencyErrorKind);

#[derive(PartialEq, Eq, Debug)]
enum CurrencyErrorKind {
    Empty,
    Initial(char),
    Intermediate(Vec<char>),
    Final(char),
    MissingLetter,
}

impl Display for CurrencyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use CurrencyErrorKind::*;
        match &self.0 {
            Empty => write!(f, "empty currency"),
            Initial(bad_char) => write!(
                f,
                "invalid initial character '{}' for currency - must be uppercase ASCII letter or '/'",
                bad_char
            ),
            Intermediate(bad_chars) => write!(f,
                                              "invalid intermediate characters {} for currency - must be upppercase ASCII alphanumeric or one of {}",
                                              itertools::Itertools::intersperse(
                                                  bad_chars.iter().map(|c| format!("'{}'", c)),
                                                  ", ".to_string())
                                              .collect::<String>(),
                                              itertools::Itertools::intersperse(
                                                  CURRENCY_INTERMEDIATE_EXTRA_CHARS.iter().map(|c| format!("'{}'", c)),
                                                  ", ".to_string())
                                              .collect::<String>()),
            Final(bad_char) => write!(
                f,
                "invalid final character '{}' for currency - must be uppercase ASCII alphanumeric",
                bad_char
            ),
            MissingLetter => write!(f, "currency must contain at least one letter")
        }
    }
}

impl Error for CurrencyError {}

impl<'a> TryFrom<&'a str> for Currency<'a> {
    type Error = CurrencyError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        use CurrencyErrorKind::*;
        if s.is_empty() {
            Err(CurrencyError(Empty))
        } else {
            let mut chars = s.chars();
            let first = chars.next().unwrap();
            let intermediate: Vec<char> = if s.len() > 2 {
                chars.by_ref().take(s.len() - 2).collect()
            } else {
                empty::<char>().collect()
            };
            let last = if s.len() > 1 {
                chars.next().unwrap()
            } else {
                first
            };

            use CurrencyErrorKind::*;
            if !Currency::is_valid_initial(&first) {
                Err(CurrencyError(Initial(first)))
            } else if !Currency::is_valid_final(&last) {
                Err(CurrencyError(Final(last)))
            } else {
                let bad_intermediates = intermediate
                    .into_iter()
                    .filter_map(|c| (!Currency::is_valid_intermediate(&c)).then_some(c))
                    .collect::<Vec<char>>();
                if !bad_intermediates.is_empty() {
                    Err(CurrencyError(Intermediate(bad_intermediates)))
                } else if s.find(|c: char| c.is_ascii_uppercase()).is_none() {
                    Err(CurrencyError(MissingLetter))
                } else {
                    Ok(Currency(s))
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Default, Clone, Debug)]
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

#[derive(PartialEq, Eq, Clone, Debug)]
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Tag<'a>(TagOrLinkIdentifier<'a>);

impl<'a> From<TagOrLinkIdentifier<'a>> for Tag<'a> {
    fn from(id: TagOrLinkIdentifier<'a>) -> Self {
        Self(id)
    }
}

impl<'a> TryFrom<&'a str> for Tag<'a> {
    type Error = TagOrLinkIdentifierError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        TagOrLinkIdentifier::try_from(s).map(Tag)
    }
}

impl<'a> Display for Tag<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0 .0)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Link<'a>(TagOrLinkIdentifier<'a>);

impl<'a> From<TagOrLinkIdentifier<'a>> for Link<'a> {
    fn from(id: TagOrLinkIdentifier<'a>) -> Self {
        Self(id)
    }
}

impl<'a> TryFrom<&'a str> for Link<'a> {
    type Error = TagOrLinkIdentifierError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        TagOrLinkIdentifier::try_from(s).map(Link)
    }
}

impl<'a> Display for Link<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "^{}", self.0 .0)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TagOrLinkIdentifier<'a>(&'a str);

/// The valid characters for tags and links besides alphanumeric.
const TAG_OR_LINK_EXTRA_CHARS: [char; 4] = ['-', '_', '/', '.'];

impl<'a> TagOrLinkIdentifier<'a> {
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

impl<'a> TryFrom<&'a str> for TagOrLinkIdentifier<'a> {
    type Error = TagOrLinkIdentifierError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        let bad_chars = s
            .chars()
            .filter_map(|c| (!TagOrLinkIdentifier::is_valid_char(&c)).then_some(c))
            .collect::<Vec<char>>();
        if bad_chars.is_empty() {
            Ok(TagOrLinkIdentifier(s))
        } else {
            Err(TagOrLinkIdentifierError(bad_chars))
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Key<'a>(&'a str);

impl<'a> Key<'a> {
    pub fn is_valid_initial(c: &char) -> bool {
        c.is_ascii_lowercase()
    }

    pub fn is_valid_subsequent(c: &char) -> bool {
        c.is_alphanumeric() || *c == '-'
    }
}

impl<'a> Display for Key<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct KeyError(KeyErrorKind);

#[derive(PartialEq, Eq, Debug)]
enum KeyErrorKind {
    Empty,
    Initial(char),
    Subsequent(Vec<char>),
}

impl Display for KeyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use KeyErrorKind::*;
        match &self.0 {
            Empty => write!(f, "empty key"),
            Initial(bad_char) => write!(
                f,
                "invalid character '{}' for key initial - must be lowercase ASCII letter",
                bad_char
            ),
            Subsequent(bad_chars) => write!(
                f,
                "invalid characters {} for key - must be alphanumeric or '-'",
                itertools::Itertools::intersperse(
                    bad_chars.iter().map(|c| format!("'{}'", c)),
                    ", ".to_string()
                )
                .collect::<String>()
            ),
        }
    }
}

impl Error for KeyError {}

impl<'a> TryFrom<&'a str> for Key<'a> {
    type Error = KeyError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        use KeyErrorKind::*;
        if s.is_empty() {
            Err(KeyError(Empty))
        } else {
            let mut chars = s.chars();
            let initial = chars.next().unwrap();
            if !Key::is_valid_initial(&initial) {
                Err(KeyError(Initial(initial)))
            } else {
                let bad_chars = chars
                    .filter_map(|c| (!Key::is_valid_subsequent(&c)).then_some(c))
                    .collect::<Vec<char>>();
                if bad_chars.is_empty() {
                    Ok(Key(s))
                } else {
                    Err(KeyError(Subsequent(bad_chars)))
                }
            }
        }
    }
}

#[derive(Debug)]
/// An `Expr` which has been evaluated.
pub struct ExprValue {
    pub value: Decimal,
    expr: Expr,
}

impl From<Expr> for ExprValue {
    /// Evaluate the `Expr` rounding to the max scale it contains.
    fn from(expr: Expr) -> Self {
        let (mut value, scale) = expr.evaluate();
        value.rescale(scale);
        Self { value, expr }
    }
}

impl Display for ExprValue {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.expr, format)
    }
}

#[derive(PartialEq, Eq)]
pub enum Expr {
    Value(Decimal),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Paren(Box<Expr>),
}

impl Expr {
    fn evaluate(&self) -> (Decimal, u32) {
        fn evaluate_unary<F>(op: F, e1: &Expr) -> (Decimal, u32)
        where
            F: Fn(Decimal) -> Decimal,
        {
            let (d1, s1) = e1.evaluate();
            (op(d1), s1)
        }

        fn evaluate_binary<F>(op: F, e1: &Expr, e2: &Expr) -> (Decimal, u32)
        where
            F: Fn(Decimal, Decimal) -> Decimal,
        {
            let (d1, s1) = e1.evaluate();
            let (d2, s2) = e2.evaluate();
            (op(d1, d2), max(s1, s2))
        }

        use Expr::*;
        match self {
            Value(d) => (*d, d.scale()),
            Add(e1, e2) => evaluate_binary(std::ops::Add::add, e1, e2),
            Sub(e1, e2) => evaluate_binary(std::ops::Sub::sub, e1, e2),
            Mul(e1, e2) => evaluate_binary(std::ops::Mul::mul, e1, e2),
            Div(e1, e2) => evaluate_binary(std::ops::Div::div, e1, e2),
            Neg(e1) => evaluate_unary(std::ops::Neg::neg, e1),
            Paren(e) => e.evaluate(),
        }
    }
}

// impl PartialEq for Expr {

// }

impl Display for Expr {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Value(val) => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "{} + {}", left, right),
            Sub(ref left, ref right) => write!(format, "{} - {}", left, right),
            Mul(ref left, ref right) => write!(format, "{} * {}", left, right),
            Div(ref left, ref right) => write!(format, "{} / {}", left, right),
            Neg(ref expr) => write!(format, "-{}", expr),
            Paren(ref expr) => write!(format, "({})", expr),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::Expr::*;
        match *self {
            Value(val) => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "({:?} + {:?})", left, right),
            Sub(ref left, ref right) => write!(format, "({:?} - {:?})", left, right),
            Mul(ref left, ref right) => write!(format, "({:?} * {:?})", left, right),
            Div(ref left, ref right) => write!(format, "({:?} / {:?})", left, right),
            Neg(ref expr) => write!(format, "(-{:?})", expr),
            Paren(ref expr) => write!(format, "[{:?}]", expr),
        }
    }
}

/// TODO It's unclear to me whether a compound expression is simply either per-unit or total, or whether it can be both.
/// For now I assume one or the other.
#[derive(PartialEq, Eq, Debug)]
pub enum CompoundExpr {
    PerUnit(Expr),
    Total(Expr),
}

impl Display for CompoundExpr {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::CompoundExpr::*;
        match self {
            PerUnit(e) => write!(format, "{}", e),
            Total(e) => write!(format, "# {}", e),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum CompoundAmount<'a> {
    BareCurrency(&'a Currency<'a>),
    BareAmount(CompoundExpr),
    CurrencyAmount(CompoundExpr, &'a Currency<'a>),
}

impl<'a> Display for CompoundAmount<'a> {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::CompoundAmount::*;
        match self {
            BareCurrency(cur) => write!(format, "{}", cur),
            BareAmount(ce) => write!(format, "{}", ce),
            CurrencyAmount(ce, cur) => write!(format, "{} {}", ce, cur),
        }
    }
}

pub use lexer::dump as logos_dump;
pub use lexer_chumsky::dump as chumsky_dump;

mod lexer;
mod lexer_chumsky;
mod parser;
mod tests;
