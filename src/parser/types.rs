use super::lexer::Token;
use crate::types::*;
use chumsky::error::Rich;
use chumsky::span::SimpleSpan;
use lazy_format::lazy_format;
use nonempty::NonEmpty;
use rust_decimal::Decimal;
use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
    iter::empty,
    mem::swap,
    path::Path,
};
use strum_macros::Display;
use time::Date;

pub type ParserError<'a> = Rich<'a, Token<'a>, SimpleSpan>;

/// Spanned and additionally with source path
/// TODO: eventually this should perhaps be using &Path as the Span::Context,
/// which requires bleeding edge chumsky at this time.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Sourced<'a, T> {
    pub spanned: Spanned<T>,
    pub source_path: &'a Path,
}

/// Our public error type
#[derive(Debug)]
pub struct SourcedError<'a> {
    pub(crate) source_path: &'a Path,
    pub(crate) span: SimpleSpan,
    pub(crate) message: String,
    pub(crate) reason: Option<String>,
    pub(crate) contexts: Vec<(String, SimpleSpan)>,
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

#[derive(Clone, Debug)]
pub enum Declaration<'a> {
    Directive(Directive<'a>),
    // TODO actually support Pragma
    Pragma(Pragma<'a>),
}

#[derive(Clone, Debug)]
pub enum Directive<'a> {
    Transaction(Transaction<'a>),
    Open(Open<'a>),
    Close(Close<'a>),
    Commodity(Commodity<'a>),
    // TODO other directives
}

impl<'a> Display for Directive<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Directive::*;

        match &self {
            Transaction(x) => x.fmt(f),
            Open(x) => x.fmt(f),
            Close(x) => x.fmt(f),
            Commodity(x) => x.fmt(f),
        }
    }
}

/// A trait for items which have a date
pub trait Dated {
    fn date(&self) -> &Date;
}

impl<'a> Dated for Directive<'a> {
    fn date(&self) -> &Date {
        use Directive::*;

        match self {
            Transaction(x) => x.date(),
            Open(x) => x.date(),
            Close(x) => x.date(),
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

#[derive(Clone, Debug)]
pub struct Transaction<'a> {
    pub(crate) date: Spanned<Date>,
    pub(crate) flag: Spanned<Flag>,
    pub(crate) payee: Option<Spanned<&'a str>>,
    pub(crate) narration: Option<Spanned<&'a str>>,
    pub(crate) tags: HashSet<Spanned<&'a Tag<'a>>>,
    pub(crate) links: HashSet<Spanned<&'a Link<'a>>>,
    pub(crate) metadata: Metadata<'a>,
    pub(crate) postings: Vec<Spanned<Posting<'a>>>,
}

impl<'a> Display for Transaction<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.date, self.flag)?;

        format(f, &self.payee, double_quoted, " ", Some(" "))?;
        format(f, &self.narration, double_quoted, " ", Some(" "))?;
        format(f, &self.tags, plain, " ", Some(" "))?;
        format(f, &self.links, plain, " ", Some(" "))?;
        self.metadata.fmt(f)?;
        format(
            f,
            &self.postings,
            plain,
            NEWLINE_INDENT,
            Some(NEWLINE_INDENT),
        )?;
        Ok(())
    }
}

impl<'a> Dated for Transaction<'a> {
    fn date(&self) -> &Date {
        &self.date.value
    }
}

#[derive(Clone, Debug)]
pub struct Open<'a> {
    pub(crate) date: Spanned<Date>,
    pub(crate) account: Spanned<&'a Account<'a>>,
    pub(crate) currencies: HashSet<Spanned<&'a Currency<'a>>>,
    pub(crate) booking: Option<Spanned<Booking>>,
    pub(crate) tags: HashSet<Spanned<&'a Tag<'a>>>,
    pub(crate) links: HashSet<Spanned<&'a Link<'a>>>,
    pub(crate) metadata: Metadata<'a>,
}

impl<'a> Display for Open<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} open {}", self.date, self.account)?;
        format(f, &self.currencies, plain, " ", Some(" "))?;
        format(f, &self.booking, double_quoted, " ", Some(" "))?;
        format(f, &self.tags, plain, " ", Some(" "))?;
        format(f, &self.links, plain, " ", Some(" "))?;
        self.metadata.fmt(f)
    }
}

impl<'a> Dated for Open<'a> {
    fn date(&self) -> &Date {
        &self.date.value
    }
}

#[derive(Clone, Debug)]
pub struct Close<'a> {
    pub(crate) date: Spanned<Date>,
    pub(crate) account: Spanned<&'a Account<'a>>,
    pub(crate) tags: HashSet<Spanned<&'a Tag<'a>>>,
    pub(crate) links: HashSet<Spanned<&'a Link<'a>>>,
    pub(crate) metadata: Metadata<'a>,
}

impl<'a> Display for Close<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} close {}", self.date, self.account)?;
        format(f, &self.tags, plain, " ", Some(" "))?;
        format(f, &self.links, plain, " ", Some(" "))?;
        self.metadata.fmt(f)
    }
}

impl<'a> Dated for Close<'a> {
    fn date(&self) -> &Date {
        &self.date.value
    }
}

#[derive(Clone, Debug)]
pub struct Commodity<'a> {
    pub date: Spanned<Date>,
    pub currency: Spanned<&'a Currency<'a>>,
    pub tags: HashSet<Spanned<&'a Tag<'a>>>,
    pub links: HashSet<Spanned<&'a Link<'a>>>,
    pub metadata: Metadata<'a>,
}

impl<'a> Display for Commodity<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} commodity {}", self.date, self.currency)?;
        format(f, &self.tags, plain, " ", Some(" "))?;
        format(f, &self.links, plain, " ", Some(" "))?;
        self.metadata.fmt(f)
    }
}

impl<'a> Dated for Commodity<'a> {
    fn date(&self) -> &Date {
        &self.date.value
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Account<'a> {
    pub(crate) account_type: AccountType,
    pub(crate) names: NonEmpty<AccountName<'a>>,
}

impl<'a> Display for Account<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.account_type)?;
        format(f, &self.names, plain, ":", Some(":"))?;
        Ok(())
    }
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

impl<'a> AsRef<str> for AccountName<'a> {
    fn as_ref(&self) -> &str {
        self.0
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
            Subsequent(bad_chars) => {
                format(f, bad_chars, single_quoted, ", ", Some("invalid characters "))?;
                f.write_str(" - must be alphanumeric or '-'")
            }
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

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
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

impl<'a> AsRef<str> for Currency<'a> {
    fn as_ref(&self) -> &str {
        self.0
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
            Intermediate(bad_chars) => {
                format(f, bad_chars, single_quoted, ", ", Some("invalid intermediate characters "))?;
                format(f, CURRENCY_INTERMEDIATE_EXTRA_CHARS, single_quoted, ", ", Some(" for currency - must be upppercase ASCII alphanumeric or one of "))
            }
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

#[derive(Clone, Debug)]
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
        simple_format(f, &self.flag, None)?;

        write!(
            f,
            "{}{}",
            if self.flag.is_some() { " " } else { "" },
            &self.account.value
        )?;

        simple_format(f, &self.amount, Some(" "))?;
        simple_format(f, &self.currency, Some(" "))?;
        simple_format(f, &self.cost_spec, Some(" "))?;
        simple_format(f, &self.price_annotation, Some(" "))?;

        self.metadata.fmt(f)
    }
}

#[derive(Clone, Default, Debug)]
pub struct Metadata<'a> {
    pub key_values: HashMap<Spanned<&'a Key<'a>>, Spanned<MetaValue<'a>>>,
    pub tags: HashSet<Spanned<&'a Tag<'a>>>,
    pub links: HashSet<Spanned<&'a Link<'a>>>,
}

impl<'a> Display for Metadata<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format(
            f,
            &self.key_values,
            key_value,
            NEWLINE_INDENT,
            Some(NEWLINE_INDENT),
        )?;
        format(f, &self.tags, plain, NEWLINE_INDENT, Some(NEWLINE_INDENT))?;
        format(f, &self.links, plain, NEWLINE_INDENT, Some(NEWLINE_INDENT))
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct MetaKeyValue<'a> {
    pub key: Spanned<&'a Key<'a>>,
    pub value: Spanned<MetaValue<'a>>,
}

impl<'a> Display for MetaKeyValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: \"{}\"", &self.key, &self.value)
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
    Date(Date),
    Bool(bool),
    None,
    Expr(ExprValue),
}

impl<'a> Display for SimpleValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use SimpleValue::*;

        match self {
            String(x) => write!(f, r#""{}""#, x),
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

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Tag<'a>(pub(crate) TagOrLinkIdentifier<'a>);

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

impl<'a> AsRef<str> for Tag<'a> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<'a> Display for Tag<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0 .0)
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Link<'a>(pub(crate) TagOrLinkIdentifier<'a>);

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

impl<'a> AsRef<str> for Link<'a> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
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
        format(f, &self.0, single_quoted, ", ", Some("invalid characters "))?;
        format(
            f,
            TAG_OR_LINK_EXTRA_CHARS,
            single_quoted,
            ", ",
            Some(" for tag or link identifier - must be alphanumeric or one of "),
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

impl<'a> AsRef<str> for TagOrLinkIdentifier<'a> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
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
            Subsequent(bad_chars) => {
                format(
                    f,
                    bad_chars,
                    single_quoted,
                    ", ",
                    Some("invalid characters "),
                )?;
                f.write_str(" for key - must be alphanumeric or '-'")
            }
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

#[derive(PartialEq, Eq, Clone, Debug)]
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
    number: Spanned<ExprValue>,
    currency: Spanned<&'a Currency<'a>>,
}

impl<'a> Display for Amount<'a> {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        write!(format, "{} {}", &self.number.value, &self.currency.value)
    }
}

impl<'a> Amount<'a> {
    pub fn new(amount: (Spanned<ExprValue>, Spanned<&'a Currency<'a>>)) -> Self {
        Amount {
            number: amount.0,
            currency: amount.1,
        }
    }

    pub fn number(&self) -> &Spanned<ExprValue> {
        &self.number
    }

    pub fn currency(&self) -> &Spanned<&Currency> {
        &self.currency
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
    date: Option<Spanned<Date>>,
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
    date: Option<Spanned<Date>>,
    label: Option<Spanned<&'a str>>,
    merge: bool,
    errors: Vec<CostSpecError>,
}

impl<'a> CostSpecBuilder<'a> {
    pub fn compound_expr(self, value: ScopedExpr, span: SimpleSpan) -> Self {
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

    pub fn currency(mut self, value: &'a Currency<'a>, span: SimpleSpan) -> Self {
        if self.currency.is_none() {
            self.currency = Some(spanned(value, span));
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Currency))
        }
        self
    }

    pub fn date(mut self, value: Date, span: SimpleSpan) -> Self {
        if self.date.is_none() {
            self.date = Some(spanned(value, span));
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Date))
        }
        self
    }

    pub fn label(mut self, value: &'a str, span: SimpleSpan) -> Self {
        if self.label.is_none() {
            self.label = Some(spanned(value, span));
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Label))
        }
        self
    }

    pub fn merge(mut self, _span: SimpleSpan) -> Self {
        if !self.merge {
            // TODO find a way to keep a span iff merge is true
            self.merge = true;
        } else {
            self.errors
                .push(CostSpecError(CostSpecErrorKind::MergeCost))
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
#[strum(serialize_all = "kebab-case")]
enum CostSpecErrorKind {
    PerUnit,
    Total,
    Currency,
    Date,
    Label,
    MergeCost,
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
        format(f, &self.0, plain, ", ", None)
    }
}

impl std::error::Error for CostSpecErrors {}

// TODO move this formatting stuff somewhere else, like format submodule

fn format<C, T, M, D>(
    f: &mut Formatter<'_>,
    container: C,
    mapper: M,
    separator: &'static str,
    prefix: Option<&'static str>,
) -> fmt::Result
where
    C: IntoIterator<Item = T>,
    M: Fn(T) -> D,
    D: Display,
{
    let mut container = container.into_iter();
    if let Some(item) = container.by_ref().next() {
        if let Some(prefix) = prefix {
            f.write_str(prefix)?;
        }

        mapper(item).fmt(f)?;
    }

    for item in container {
        f.write_str(separator)?;
        mapper(item).fmt(f)?;
    }

    Ok(())
}

/// Simple format with no mapper or separator
fn simple_format<C, T>(
    f: &mut Formatter<'_>,
    container: C,
    prefix: Option<&'static str>,
) -> fmt::Result
where
    C: IntoIterator<Item = T>,
    T: Display,
{
    format(f, container, plain, "", prefix)
}

/// Format plain.
fn plain<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("{s}")
}

/// Format in single quotes.
fn single_quoted<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("'{s}'")
}

/// Format in double quotes.
fn double_quoted<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("\"{s}\"")
}

/// Format plain.
fn key_value<K, V>(kv: (K, V)) -> impl Display
where
    K: Display,
    V: Display,
{
    lazy_format!("{}: {}", kv.0, kv.1)
}

fn pad_if(condition: bool) -> &'static str {
    if condition {
        " "
    } else {
        ""
    }
}

const NEWLINE: &str = "\n";
const INDENT: &str = "  ";
const NEWLINE_INDENT: &str = "\n  ";

mod tests;
