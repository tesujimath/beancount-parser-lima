// TODO remove suppression for dead code warning
#![allow(dead_code)]

use chrono::NaiveDate;
use rust_decimal::Decimal;
use std::{
    cmp::max,
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    iter::empty,
    str::FromStr,
};
use strum_macros::Display;

#[derive(PartialEq, Eq, Debug)]
pub struct Account {
    account_type: AccountType,
    sub_accounts: Vec<SubAccount>,
}

impl Account {
    pub fn new(
        account_type: AccountType,
        sub_accounts: Vec<SubAccount>,
    ) -> Result<Account, AccountError> {
        use AccountErrorKind::*;
        if sub_accounts.is_empty() {
            Err(AccountError(MissingSubAccounts))
        } else {
            Ok(Account {
                account_type,
                sub_accounts,
            })
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct AccountError(AccountErrorKind);

#[derive(PartialEq, Eq, Debug)]
enum AccountErrorKind {
    MissingSubAccounts,
}

impl Display for AccountError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AccountErrorKind::*;
        match &self.0 {
            MissingSubAccounts => write!(f, "missing subaccounts"),
        }
    }
}

impl Error for AccountError {}

impl Display for Account {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}",
            self.account_type,
            itertools::Itertools::intersperse(
                self.sub_accounts.iter().map(|s| format!("{}", s)),
                ":".to_string()
            )
            .collect::<String>()
        )
    }
}

#[derive(PartialEq, Eq, Display, Debug)]
pub enum AccountType {
    Assets,
    Liabilities,
    Equity,
    Income,
    Expenses,
}

#[derive(PartialEq, Eq, Debug)]
pub struct SubAccount(String);

impl SubAccount {
    pub fn is_valid_initial(c: &char) -> bool {
        c.is_ascii_uppercase() || c.is_ascii_digit()
    }

    pub fn is_valid_subsequent(c: &char) -> bool {
        c.is_alphanumeric() || *c == '-'
    }
}

impl Display for SubAccount {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct SubAccountError(SubAccountErrorKind);

#[derive(PartialEq, Eq, Debug)]
enum SubAccountErrorKind {
    Empty,
    Initial(char),
    Subsequent(Vec<char>),
}

impl Display for SubAccountError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use SubAccountErrorKind::*;
        match &self.0 {
            Empty => write!(f, "empty subaccount"),
            Initial(bad_char) => write!(
                f,
                "invalid character '{}' for subaccount initial - must be uppercase ASCII letter or digit",
                bad_char
            ),
            Subsequent(bad_chars) => write!(f,
                                            "invalid characters {} for subaccount - must be alphanumeric or '-'",
                                            itertools::Itertools::intersperse(
                                                bad_chars.iter().map(|c| format!("'{}'", c)),
                                                ", ".to_string())
                                            .collect::<String>()),
        }
    }
}

impl Error for SubAccountError {}

impl FromStr for SubAccount {
    type Err = SubAccountError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use SubAccountErrorKind::*;
        if s.is_empty() {
            Err(SubAccountError(Empty))
        } else {
            let mut chars = s.chars();
            let initial = chars.next().unwrap();
            if !SubAccount::is_valid_initial(&initial) {
                Err(SubAccountError(Initial(initial)))
            } else {
                let bad_chars = chars
                    .filter_map(|c| (!SubAccount::is_valid_subsequent(&c)).then_some(c))
                    .collect::<Vec<char>>();
                if bad_chars.is_empty() {
                    Ok(SubAccount(s.to_owned()))
                } else {
                    Err(SubAccountError(Subsequent(bad_chars)))
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct Currency(String);

/// The valid intermediate characters for currency, in addition to ASCII uppercase and digits
const CURRENCY_INTERMEDIATE_EXTRA_CHARS: [char; 4] = ['\'', '.', '_', '-'];

impl Currency {
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

impl Display for Currency {
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

impl FromStr for Currency {
    type Err = CurrencyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
                    Ok(Currency(s.to_owned()))
                }
            }
        }
    }
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

impl FromStr for Tag {
    type Err = TagOrLinkIdentifierError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        TagOrLinkIdentifier::from_str(s).map(Tag)
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

impl FromStr for Link {
    type Err = TagOrLinkIdentifierError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        TagOrLinkIdentifier::from_str(s).map(Link)
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

impl FromStr for TagOrLinkIdentifier {
    type Err = TagOrLinkIdentifierError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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

#[derive(Debug)]
pub struct DecimalExpr {
    pub value: Decimal,
    raw: RawDecimalExpr,
}

impl DecimalExpr {
    /// Evaluate the `RawDecimalExpr` rounding to the max scale it contains.
    fn new(raw: RawDecimalExpr) -> Self {
        let (mut value, scale) = raw.evaluate();
        value.rescale(scale);
        Self { value, raw }
    }
}

impl Display for DecimalExpr {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.raw, format)
    }
}

pub enum RawDecimalExpr {
    Value(Decimal),
    Add(Box<RawDecimalExpr>, Box<RawDecimalExpr>),
    Sub(Box<RawDecimalExpr>, Box<RawDecimalExpr>),
    Mul(Box<RawDecimalExpr>, Box<RawDecimalExpr>),
    Div(Box<RawDecimalExpr>, Box<RawDecimalExpr>),
    Paren(Box<RawDecimalExpr>),
}

impl RawDecimalExpr {
    fn evaluate(&self) -> (Decimal, u32) {
        fn evaluate_binary<F>(op: F, e1: &RawDecimalExpr, e2: &RawDecimalExpr) -> (Decimal, u32)
        where
            F: Fn(Decimal, Decimal) -> Decimal,
        {
            let (d1, s1) = e1.evaluate();
            let (d2, s2) = e2.evaluate();
            (op(d1, d2), max(s1, s2))
        }

        use RawDecimalExpr::*;
        match self {
            Value(d) => (*d, d.scale()),
            Add(e1, e2) => evaluate_binary(std::ops::Add::add, e1, e2),
            Sub(e1, e2) => evaluate_binary(std::ops::Sub::sub, e1, e2),
            Mul(e1, e2) => evaluate_binary(std::ops::Mul::mul, e1, e2),
            Div(e1, e2) => evaluate_binary(std::ops::Div::div, e1, e2),
            Paren(e) => e.evaluate(),
        }
    }
}

impl Display for RawDecimalExpr {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::RawDecimalExpr::*;
        match *self {
            Value(val) => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "{} + {}", left, right),
            Sub(ref left, ref right) => write!(format, "{} - {}", left, right),
            Mul(ref left, ref right) => write!(format, "{} * {}", left, right),
            Div(ref left, ref right) => write!(format, "{} / {}", left, right),
            Paren(ref expr) => write!(format, "({})", expr),
        }
    }
}

impl Debug for RawDecimalExpr {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::RawDecimalExpr::*;
        match *self {
            Value(val) => write!(format, "{}", val),
            Add(ref left, ref right) => write!(format, "({:?} + {:?})", left, right),
            Sub(ref left, ref right) => write!(format, "({:?} - {:?})", left, right),
            Mul(ref left, ref right) => write!(format, "({:?} * {:?})", left, right),
            Div(ref left, ref right) => write!(format, "({:?} / {:?})", left, right),
            Paren(ref expr) => write!(format, "[{:?}]", expr),
        }
    }
}

mod parser;
mod tests;
