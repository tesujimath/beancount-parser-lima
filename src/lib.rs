use chrono::NaiveDate;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    str::FromStr,
};
use strum_macros::Display;

#[derive(PartialEq, Eq, Debug)]
pub struct Account {
    account_type: AccountType,
    sub_accounts: Vec<SubAccount>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct AccountError;

impl Account {
    pub fn new(
        account_type: AccountType,
        sub_accounts: Vec<SubAccount>,
    ) -> Result<Account, AccountError> {
        if sub_accounts.is_empty() {
            Err(AccountError)
        } else {
            Ok(Account {
                account_type,
                sub_accounts,
            })
        }
    }
}

impl Display for AccountError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // there's only one error possible
        write!(f, "missing subaccounts")
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

mod parser;
mod tests;
