use super::parser::ParserError;
use chrono::NaiveDate;
use chumsky::span::SimpleSpan;
use nonempty::NonEmpty;
use rust_decimal::Decimal;
use std::{
    borrow::Cow,
    cmp::max,
    fmt::{self, Display, Formatter},
    iter::empty,
    mem::swap,
    path::Path,
};
use strum_macros::{Display, EnumString};

pub type Span = SimpleSpan<usize>;

/// a Spanned value may be located within a source file if the file path is known.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

pub fn spanned<T>(value: T, span: Span) -> Spanned<T> {
    Spanned { value, span }
}

impl<T> Spanned<T> {
    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            value: &self.value,
            span: self.span,
        }
    }
}

impl<T> Display for Spanned<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value,)
    }
}

/// Spanned and additionally with source path
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Sourced<'a, T> {
    pub spanned: Spanned<T>,
    pub source_path: &'a Path,
}

/// Our public error type
#[derive(Debug)]
pub struct SourcedError<'a> {
    pub(crate) source_path: &'a Path,
    pub(crate) span: Span,
    pub(crate) message: String,
    pub(crate) reason: Option<String>,
    pub(crate) contexts: Vec<(String, Span)>,
}

impl<'a> SourcedError<'a> {
    pub(crate) fn from_parser_error(source_path: &'a Path, error: ParserError) -> Self {
        let error = error.map_token(|tok| tok.to_string());

        SourcedError {
            source_path,
            span: *error.span(),
            message: error.to_string(),
            reason: Some(error.reason().to_string()),
            contexts: error
                .contexts()
                .map(|(label, span)| (label.to_string(), *span))
                .collect(),
        }
    }
}

impl<'a> Display for SourcedError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ", self.message)?;
        if let Some(reason) = &self.reason {
            write!(f, "({}) ", reason)?;
        }
        write!(
            f,
            "at {} of {}",
            self.span,
            self.source_path.to_string_lossy()
        )?;
        for context in self.contexts.iter() {
            write!(f, " while parsing {} at {}", context.0, context.1)?;
        }
        Ok(())
    }
}

impl<'a> std::error::Error for SourcedError<'a> {}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Declaration<'a> {
    Directive(Directive<'a>),
    // TODO actually support Pragma
    Pragma(Pragma<'a>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Directive<'a> {
    Transaction(Transaction<'a>),
    Open(Open<'a>),
    Commodity(Commodity<'a>),
    // TODO other directives
}

impl<'a> Display for Directive<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Directive::*;

        match &self {
            Transaction(x) => x.fmt(f),
            Open(x) => x.fmt(f),
            Commodity(x) => x.fmt(f),
        }
    }
}

/// A trait for items which have a (naive) date
pub trait Date {
    fn date(&self) -> &NaiveDate;
}

impl<'a> Date for Directive<'a> {
    fn date(&self) -> &NaiveDate {
        use Directive::*;

        match self {
            Transaction(x) => x.date(),
            Open(x) => x.date(),
            Commodity(x) => x.date(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Pragma<'a> {
    // TODO
    Placeholder(&'a str),
    Include(&'a str),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Transaction<'a> {
    date: Spanned<NaiveDate>,
    flag: Spanned<Flag>,
    payee: Option<Spanned<&'a str>>,
    narration: Option<Spanned<&'a str>>,
    tags: Vec<Spanned<&'a Tag<'a>>>,
    links: Vec<Spanned<&'a Link<'a>>>,
    metadata: Metadata<'a>,
    postings: Vec<Spanned<Posting<'a>>>,
}

impl<'a> Transaction<'a> {
    pub fn new(
        date: Spanned<NaiveDate>,
        flag: Spanned<Flag>,
        payee: Option<Spanned<&'a str>>,
        narration: Option<Spanned<&'a str>>,

        tags: Vec<Spanned<&'a Tag<'a>>>,
        links: Vec<Spanned<&'a Link<'a>>>,
        metadata: Metadata<'a>,
        postings: Vec<Spanned<Posting<'a>>>,
    ) -> Self {
        Transaction {
            date,
            flag,
            payee,
            narration,
            tags,
            links,
            metadata,
            postings,
        }
    }
}

impl<'a> Display for Transaction<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}{}{}{}{}\n{}{}",
            self.date,
            self.flag,
            format_option_with_padding(&self.payee, double_quote),
            format_option_with_padding(&self.narration, double_quote),
            join(&self.tags, " "),
            pad_if(!self.tags.is_empty()),
            join(&self.links, " "),
            &self.metadata,
            join(&self.postings, ""),
        )
    }
}

impl<'a> Date for Transaction<'a> {
    fn date(&self) -> &NaiveDate {
        &self.date.value
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Open<'a> {
    pub date: Spanned<NaiveDate>,
    pub account: Spanned<&'a Account<'a>>,
    pub currencies: Vec<Spanned<&'a Currency<'a>>>,
    pub booking: Option<Spanned<&'a str>>,
    pub tags: Vec<Spanned<&'a Tag<'a>>>,
    pub links: Vec<Spanned<&'a Link<'a>>>,
    pub metadata: Metadata<'a>,
}

impl<'a> Open<'a> {
    pub fn new(
        date: Spanned<NaiveDate>,
        account: Spanned<&'a Account<'a>>,
        currencies: Vec<Spanned<&'a Currency<'a>>>,
        booking: Option<Spanned<&'a str>>,
        tags: Vec<Spanned<&'a Tag<'a>>>,
        links: Vec<Spanned<&'a Link<'a>>>,
        metadata: Metadata<'a>,
    ) -> Self {
        Open {
            date,
            account,
            currencies,
            booking,
            tags,
            links,
            metadata,
        }
    }
}

impl<'a> Display for Open<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} open {} {} {} {}\n{}",
            self.date,
            self.account,
            join(&self.currencies, ", "),
            join(&self.tags, ", "),
            join(&self.links, ", "),
            &self.metadata,
        )
    }
}

impl<'a> Date for Open<'a> {
    fn date(&self) -> &NaiveDate {
        &self.date.value
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Commodity<'a> {
    pub date: Spanned<NaiveDate>,
    pub currency: Spanned<&'a Currency<'a>>,
    pub tags: Vec<Spanned<&'a Tag<'a>>>,
    pub links: Vec<Spanned<&'a Link<'a>>>,
    pub metadata: Metadata<'a>,
}

impl<'a> Commodity<'a> {
    pub fn new(
        date: Spanned<NaiveDate>,
        currency: Spanned<&'a Currency<'a>>,
        tags: Vec<Spanned<&'a Tag<'a>>>,
        links: Vec<Spanned<&'a Link<'a>>>,
        metadata: Metadata<'a>,
    ) -> Self {
        Commodity {
            date,
            currency,
            tags,
            links,
            metadata,
        }
    }
}

impl<'a> Display for Commodity<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} commodity {} {}{}{}\n{}",
            self.date,
            self.currency,
            join(&self.tags, " "),
            pad_if(!self.tags.is_empty()),
            join(&self.links, " "),
            &self.metadata,
        )
    }
}

impl<'a> Date for Commodity<'a> {
    fn date(&self) -> &NaiveDate {
        &self.date.value
    }
}

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
        write!(f, "{}:{}", self.account_type, join(&self.names, ":"))
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
            Subsequent(bad_chars) => write!(
                f,
                "invalid characters {} for account name - must be alphanumeric or '-'",
                format_and_join(bad_chars, single_quote, ", ")
            ),
        }
    }
}

impl std::error::Error for AccountNameError {}

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
            Intermediate(bad_chars) => write!(
                f,
                "invalid intermediate characters {} for currency - must be upppercase ASCII alphanumeric or one of {}",
                format_and_join(bad_chars, single_quote, ", "),
                format_and_join(CURRENCY_INTERMEDIATE_EXTRA_CHARS, single_quote, ", ") ),
            Final(bad_char) => write!(
                f,
                "invalid final character '{}' for currency - must be uppercase ASCII alphanumeric",
                bad_char
            ),
            MissingLetter => write!(f, "currency must contain at least one letter")
        }
    }
}

impl std::error::Error for CurrencyError {}

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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Posting<'a> {
    pub flag: Option<Spanned<Flag>>,
    pub account: Spanned<&'a Account<'a>>,
    pub amount: Option<Spanned<Expr>>,
    pub currency: Option<Spanned<&'a Currency<'a>>>,
    pub cost_spec: Option<Spanned<CostSpec<'a>>>,
    pub price_annotation: Option<Spanned<ScopedAmount<'a>>>,
    pub metadata: Metadata<'a>,
}

impl<'a> Display for Posting<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{} {}{}{}{}\n{}",
            INDENT,
            option_with_padding(&self.flag),
            &self.account.value,
            option_with_padding(&self.amount),
            option_with_padding(&self.currency),
            option_with_padding(&self.cost_spec),
            option_with_padding(&self.price_annotation),
            &self.metadata,
        )
    }
}

#[derive(PartialEq, Eq, Clone, Default, Debug)]
pub struct Metadata<'a> {
    pub key_values: Vec<(Spanned<&'a Key<'a>>, Spanned<MetaValue<'a>>)>,
    pub tags: Vec<Spanned<&'a Tag<'a>>>,
    pub links: Vec<Spanned<&'a Link<'a>>>,
}

impl<'a> Display for Metadata<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            format_and_join(
                &self.key_values,
                |kv| format!("{}{}: \"{}\"\n", INDENT, kv.0, kv.1),
                ""
            ),
            format_and_join(&self.tags, |tag| format!("{}{}\n", INDENT, tag), ""),
            format_and_join(&self.links, |link| format!("{}{}\n", INDENT, link), ""),
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MetaValue<'a> {
    Simple(SimpleValue<'a>),
    Amount(Amount<'a>),
}

impl<'a> Display for MetaValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use MetaValue::*;

        match self {
            Simple(simple_value) => simple_value.fmt(f),
            Amount(amount) => amount.fmt(f),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum SimpleValue<'a> {
    String(&'a str),
    Currency(&'a Currency<'a>),
    Account(&'a Account<'a>),
    Tag(&'a Tag<'a>),
    Link(&'a Link<'a>),
    Date(NaiveDate),
    Bool(bool),
    None,
    Expr(Expr),
}

impl<'a> Display for SimpleValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use SimpleValue::*;

        match self {
            String(x) => x.fmt(f),
            Currency(x) => x.fmt(f),
            Account(x) => x.fmt(f),
            Tag(x) => x.fmt(f),
            Link(x) => x.fmt(f),
            Date(x) => x.fmt(f),
            Bool(x) => x.fmt(f),
            None => Ok(()),
            Expr(x) => x.fmt(f),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Tag<'a>(TagOrLinkIdentifier<'a>);

impl<'a> Tag<'a> {
    pub fn new(id: TagOrLinkIdentifier<'a>) -> Self {
        Tag(id)
    }
}

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

impl<'a> Link<'a> {
    pub fn new(id: TagOrLinkIdentifier<'a>) -> Self {
        Link(id)
    }
}

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
            format_and_join(&self.0, single_quote, ", "),
            format_and_join(TAG_OR_LINK_EXTRA_CHARS, single_quote, ", ")
        )
    }
}

impl std::error::Error for TagOrLinkIdentifierError {}

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
                format_and_join(bad_chars, single_quote, ", ")
            ),
        }
    }
}

impl std::error::Error for KeyError {}

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

#[derive(PartialEq, Eq, Clone)]
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

impl fmt::Debug for Expr {
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

/// An expression which quantifies a total or per-unit.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ScopedExpr {
    PerUnit(Expr),
    Total(Expr),
}

impl Display for ScopedExpr {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::ScopedExpr::*;

        match self {
            PerUnit(e) => write!(format, "{}", e),
            Total(e) => write!(format, "# {}", e),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Amount<'a> {
    number: Spanned<Expr>,
    currency: Spanned<&'a Currency<'a>>,
}

impl<'a> Display for Amount<'a> {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        write!(format, "{} {}", &self.number.value, &self.currency.value)
    }
}

impl<'a> Amount<'a> {
    pub fn new(amount: (Spanned<Expr>, Spanned<&'a Currency<'a>>)) -> Self {
        Amount {
            number: amount.0,
            currency: amount.1,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// An amount where each element may not actually be specified.
pub struct LooseAmount<'a> {
    number: Option<Spanned<Expr>>,
    currency: Option<Spanned<&'a Currency<'a>>>,
}

impl<'a> LooseAmount<'a> {
    pub fn new(amount: (Option<Spanned<Expr>>, Option<Spanned<&'a Currency<'a>>>)) -> Self {
        LooseAmount {
            number: amount.0,
            currency: amount.1,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// An amount which specifies a total or per-unit, or simply just a currency.
pub enum ScopedAmount<'a> {
    BareCurrency(&'a Currency<'a>),
    BareAmount(ScopedExpr),
    CurrencyAmount(ScopedExpr, &'a Currency<'a>),
}

impl<'a> Display for ScopedAmount<'a> {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::ScopedAmount::*;
        match self {
            BareCurrency(cur) => write!(format, "{}", cur),
            BareAmount(ce) => write!(format, "{}", ce),
            CurrencyAmount(ce, cur) => write!(format, "{} {}", ce, cur),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct CostSpec<'a> {
    per_unit: Option<Spanned<Expr>>,
    total: Option<Spanned<Expr>>,
    currency: Option<Spanned<&'a Currency<'a>>>,
    date: Option<Spanned<NaiveDate>>,
    label: Option<Spanned<&'a str>>,
    merge: bool,
}

impl<'a> Display for CostSpec<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(per_unit) = &self.per_unit {
            write!(f, "{} ", per_unit.value)?;
        }

        if let Some(total) = &self.total {
            write!(f, "# {} ", total.value)?;
        }

        if let Some(currency) = &self.currency {
            write!(f, "{} ", currency.value)?;
        }

        if let Some(date) = &self.date {
            write!(f, "{} ", date.value)?;
        }

        if let Some(label) = &self.label {
            write!(f, "\"{}\" ", label.value)?;
        }

        if self.merge {
            f.write_str("* ")?;
        }

        Ok(())
    }
}

#[derive(Default, Debug)]
/// Only allows setting each field once, and requires at least one field to be set before building.
pub struct CostSpecBuilder<'a> {
    per_unit: Option<Spanned<Expr>>,
    total: Option<Spanned<Expr>>,
    currency: Option<Spanned<&'a Currency<'a>>>,
    date: Option<Spanned<NaiveDate>>,
    label: Option<Spanned<&'a str>>,
    merge: bool,
    errors: Vec<CostSpecError>,
}

impl<'a> CostSpecBuilder<'a> {
    pub fn compound_expr(self, value: ScopedExpr, span: Span) -> Self {
        use ScopedExpr::*;

        match value {
            PerUnit(value) => self.per_unit(spanned(value, span)),
            Total(value) => self.total(spanned(value, span)),
        }
    }

    fn per_unit(mut self, value: Spanned<Expr>) -> Self {
        if self.per_unit.is_none() {
            self.per_unit = Some(value);
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::PerUnit))
        }
        self
    }

    fn total(mut self, value: Spanned<Expr>) -> Self {
        if self.total.is_none() {
            self.total = Some(value);
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Total))
        }
        self
    }

    pub fn currency(mut self, value: &'a Currency<'a>, span: Span) -> Self {
        if self.currency.is_none() {
            self.currency = Some(spanned(value, span));
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Currency))
        }
        self
    }

    pub fn date(mut self, value: NaiveDate, span: Span) -> Self {
        if self.date.is_none() {
            self.date = Some(spanned(value, span));
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Date))
        }
        self
    }

    pub fn label(mut self, value: &'a str, span: Span) -> Self {
        if self.label.is_none() {
            self.label = Some(spanned(value, span));
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Label))
        }
        self
    }

    pub fn merge(mut self, _span: Span) -> Self {
        if !self.merge {
            // TODO find a way to keep a span iff merge is true
            self.merge = true;
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Merge))
        }
        self
    }

    // the lifetime `'a` of the CostSpec returned outlives the builder lifetime `'b`
    pub fn build<'b>(&'b mut self) -> Result<CostSpec<'a>, CostSpecErrors>
    where
        'a: 'b,
    {
        let per_unit = self.per_unit.take();
        let total = self.total.take();
        let currency = self.currency.take();
        let date = self.date.take();
        let label = self.label.take();
        let merge = self.merge;
        self.merge = false;

        if !self.errors.is_empty() {
            let mut errors = Vec::new();
            swap(&mut self.errors, &mut errors);
            Err(CostSpecErrors(errors))
        } else if per_unit.is_none()
            && total.is_none()
            && currency.is_none()
            && date.is_none()
            && label.is_none()
            && !merge
        {
            Err(CostSpecErrors(vec![CostSpecError(
                CostSpecErrorKind::Empty,
            )]))
        } else {
            Ok(CostSpec {
                per_unit,
                total,
                currency,
                date,
                label,
                merge,
            })
        }
        // let errors: Vec<CostSpecError>,
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct CostSpecError(CostSpecErrorKind);

#[derive(PartialEq, Eq, Display, Debug)]
enum CostSpecErrorKind {
    #[strum(to_string = "per-unit")]
    PerUnit,
    #[strum(to_string = "total")]
    Total,
    #[strum(to_string = "currency")]
    Currency,
    #[strum(to_string = "date")]
    Date,
    #[strum(to_string = "label")]
    Label,
    #[strum(to_string = "merge-cost")]
    Merge,
    #[strum(to_string = "empty")]
    Empty,
}

impl CostSpecErrorKind {
    fn unless(self, condition: bool) -> Result<(), CostSpecError> {
        if condition {
            Ok(())
        } else {
            Err(CostSpecError(self))
        }
    }
}

impl Display for CostSpecError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.0 == CostSpecErrorKind::Empty {
            write!(f, "empty cost specification")
        } else {
            write!(f, "duplicate {} field in cost specification", self.0)
        }
    }
}

impl std::error::Error for CostSpecError {}

#[derive(PartialEq, Eq, Debug)]
pub struct CostSpecErrors(Vec<CostSpecError>);

impl Display for CostSpecErrors {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(&join(&self.0, ", "))
    }
}

impl std::error::Error for CostSpecErrors {}

// TODO move this formatting stuff somewhere else, like format submodule

/// Join a collection.
fn join<C, T, S>(collection: C, separator: S) -> String
where
    C: IntoIterator<Item = T>,
    T: ToString,
    S: ToString,
{
    format_and_join(collection, |item| item.to_string(), separator)
}

/// Join a collection with custom formatting
fn format_and_join<C, T, F, S>(collection: C, formatter: F, separator: S) -> String
where
    C: IntoIterator<Item = T>,
    F: Fn(T) -> String,
    S: ToString,
{
    itertools::Itertools::intersperse(collection.into_iter().map(formatter), separator.to_string())
        .collect::<String>()
}

fn option_with_padding<T>(optional: &Option<Spanned<T>>) -> Cow<'_, str>
where
    T: Display,
{
    format_option_with_padding(optional, |x| x.to_string())
}

fn format_option_with_padding<'a, T, F>(
    optional: &'a Option<Spanned<T>>,
    formatter: F,
) -> Cow<'a, str>
where
    F: Fn(&'a T) -> String,
{
    match optional {
        Some(spanned) => Cow::Owned(format!("{} ", formatter(&spanned.value))),
        None => Cow::Borrowed(""),
    }
}

/// Format in single quotes.
fn single_quote<S>(s: S) -> String
where
    S: Display,
{
    format!("'{}'", s)
}

/// Format in double quotes.
fn double_quote<S>(s: S) -> String
where
    S: Display,
{
    format!("\"{}\"", s)
}

fn pad_if(condition: bool) -> &'static str {
    if condition {
        " "
    } else {
        ""
    }
}

const INDENT: &str = "    ";

mod tests;
