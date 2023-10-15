use chumsky::{
    extra::ParserExtra,
    input::{Input, MapExtra},
};
use lazy_format::lazy_format;
use nonempty::NonEmpty;
use rust_decimal::Decimal;
use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
    hash::{Hash, Hasher},
    iter::empty,
    mem::swap,
    ops::Deref,
};
use strum_macros::{Display, EnumString};
use time::Date;

/// Our public error type
#[derive(Debug)]
pub struct Error {
    pub(crate) span: Span,
    pub(crate) message: String,
    pub(crate) reason: Option<String>,
    pub(crate) contexts: Vec<(String, Span)>,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use chumsky::span::Span;

        write!(f, "{} ", self.message)?;
        if let Some(reason) = &self.reason {
            write!(f, "({}) ", reason)?;
        }
        write!(
            f,
            "at {}..{} of source {}",
            self.span.start(),
            self.span.end(),
            self.span.context()
        )?;
        for context in self.contexts.iter() {
            write!(f, " while parsing {} at {}", context.0, context.1)?;
        }
        Ok(())
    }
}

impl std::error::Error for Error {}

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
    pub fn char(&self) -> char {
        self.0
    }

    pub(crate) fn is_valid(c: &char) -> bool {
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

/// a `SourceId` identifies a source file.
#[derive(PartialEq, Eq, Copy, Clone, Default, Debug)]
pub struct SourceId(u32);

impl From<usize> for SourceId {
    fn from(value: usize) -> Self {
        SourceId(value as u32)
    }
}

impl From<SourceId> for usize {
    fn from(value: SourceId) -> Self {
        value.0 as usize
    }
}

impl Display for SourceId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
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

/// Spanned item may deref as simple item for convenience.
impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
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

#[derive(Clone, Debug)]
pub enum Declaration<'a> {
    Directive(Directive<'a>),
    // TODO actually support Pragma
    Pragma(Pragma<'a>),
}

#[derive(Clone, Debug)]
pub struct Directive<'a> {
    pub(crate) date: Spanned<Date>,
    pub(crate) metadata: Metadata<'a>,
    pub(crate) variant: DirectiveVariant<'a>,
}

impl<'a> Directive<'a> {
    pub fn date(&self) -> Date {
        self.date.item
    }

    pub fn metadata(&self) -> &Metadata {
        &self.metadata
    }

    pub fn variant(&self) -> &DirectiveVariant {
        &self.variant
    }
}

impl<'a> Display for Directive<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use DirectiveVariant::*;

        match &self.variant {
            Transaction(x) => x.fmt(f, self.date.item, &self.metadata),
            Open(x) => x.fmt(f, self.date.item, &self.metadata),
            Close(x) => x.fmt(f, self.date.item, &self.metadata),
            Commodity(x) => x.fmt(f, self.date.item, &self.metadata),
        }
    }
}

#[derive(Clone, Debug)]
pub enum DirectiveVariant<'a> {
    Transaction(Transaction<'a>),
    Open(Open<'a>),
    Close(Close<'a>),
    Commodity(Commodity<'a>),
    // TODO other directives
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Pragma<'a> {
    // we keep pushed tags with their span
    // since it may be useful downstream to know where these came from
    Pushtag(Spanned<&'a Tag<'a>>),
    Poptag(Spanned<&'a Tag<'a>>),
    Pushmeta(MetaKeyValue<'a>),
    Popmeta(Spanned<&'a Key<'a>>),
    Include(&'a str),
    // TODO option, and probably not plugin
}

#[derive(Clone, Debug)]
pub struct Transaction<'a> {
    pub(crate) flag: Spanned<Flag>,
    pub(crate) payee: Option<Spanned<&'a str>>,
    pub(crate) narration: Option<Spanned<&'a str>>,
    pub(crate) postings: Vec<Spanned<Posting<'a>>>,
}

impl<'a> Transaction<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} {}", date, self.flag)?;

        format(f, &self.payee, double_quoted, " ", Some(" "))?;
        format(f, &self.narration, double_quoted, " ", Some(" "))?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)?;
        format(
            f,
            &self.postings,
            plain,
            NEWLINE_INDENT,
            Some(NEWLINE_INDENT),
        )?;
        Ok(())
    }

    pub fn flag(&self) -> &Spanned<Flag> {
        &self.flag
    }

    pub fn payee(&self) -> Option<&Spanned<&str>> {
        self.payee.as_ref()
    }

    pub fn narration(&self) -> Option<&Spanned<&str>> {
        self.narration.as_ref()
    }

    pub fn postings(&self) -> impl Iterator<Item = &Spanned<Posting>> {
        self.postings.iter()
    }
}

#[derive(Clone, Debug)]
pub struct Open<'a> {
    pub(crate) account: Spanned<&'a Account<'a>>,
    pub(crate) currencies: HashSet<Spanned<&'a Currency<'a>>>,
    pub(crate) booking: Option<Spanned<Booking>>,
}

impl<'a> Open<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} open {}", date, self.account)?;
        format(f, &self.currencies, plain, " ", Some(" "))?;
        format(f, &self.booking, double_quoted, " ", Some(" "))?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }
}

#[derive(Clone, Debug)]
pub struct Close<'a> {
    pub(crate) account: Spanned<&'a Account<'a>>,
}

impl<'a> Close<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} close {}", date, self.account)?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }
}

#[derive(Clone, Debug)]
pub struct Commodity<'a> {
    pub currency: Spanned<&'a Currency<'a>>,
}

impl<'a> Commodity<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} commodity {}", date, self.currency)?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Account<'a> {
    pub(crate) account_type: AccountType,
    pub(crate) names: NonEmpty<AccountName<'a>>,
}

impl<'a> Account<'a> {
    pub fn account_type(&self) -> AccountType {
        self.account_type
    }

    pub fn names(&self) -> impl Iterator<Item = &AccountName> {
        self.names.iter()
    }
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
                    .filter(|c| (!AccountName::is_valid_subsequent(c)))
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
                    .filter(|c| (!Currency::is_valid_intermediate(c)))
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
    pub(crate) flag: Option<Spanned<Flag>>,
    pub(crate) account: Spanned<&'a Account<'a>>,
    pub(crate) amount: Option<Spanned<ExprValue>>,
    pub(crate) currency: Option<Spanned<&'a Currency<'a>>>,
    pub(crate) cost_spec: Option<Spanned<CostSpec<'a>>>,
    pub(crate) price_annotation: Option<Spanned<ScopedAmount<'a>>>,
    pub(crate) metadata: Metadata<'a>,
}

impl<'a> Posting<'a> {
    pub fn flag(&self) -> Option<&Spanned<Flag>> {
        self.flag.as_ref()
    }

    pub fn account(&self) -> &Spanned<&Account> {
        &self.account
    }

    pub fn amount(&self) -> Option<&Spanned<ExprValue>> {
        self.amount.as_ref()
    }

    pub fn currency(&self) -> Option<&Spanned<&Currency>> {
        self.currency.as_ref()
    }

    pub fn cost_spec(&self) -> Option<&Spanned<CostSpec>> {
        self.cost_spec.as_ref()
    }

    pub fn price_annotation(&self) -> Option<&Spanned<ScopedAmount>> {
        self.price_annotation.as_ref()
    }

    pub fn metadata(&self) -> &Metadata<'a> {
        &self.metadata
    }
}

impl<'a> Display for Posting<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        simple_format(f, self.flag, None)?;

        write!(
            f,
            "{}{}",
            if self.flag.is_some() { " " } else { "" },
            &self.account
        )?;

        simple_format(f, &self.amount, Some(" "))?;
        simple_format(f, self.currency, Some(" "))?;
        simple_format(f, &self.cost_spec, Some(" "))?;
        simple_format(f, &self.price_annotation, Some(" "))?;

        self.metadata.fmt(f)
    }
}

#[derive(Clone, Default, Debug)]
pub struct Metadata<'a> {
    pub(crate) key_values: HashMap<Spanned<&'a Key<'a>>, Spanned<MetaValue<'a>>>,
    pub(crate) tags: HashSet<Spanned<&'a Tag<'a>>>,
    pub(crate) links: HashSet<Spanned<&'a Link<'a>>>,
}

impl<'a> Metadata<'a> {
    pub fn key_values(&self) -> impl Iterator<Item = (&Spanned<&Key>, &Spanned<MetaValue>)> {
        self.key_values.iter()
    }

    pub fn tags(&self) -> impl Iterator<Item = &Spanned<&Tag>> {
        self.tags.iter()
    }

    pub fn links(&self) -> impl Iterator<Item = &Spanned<&Link>> {
        self.links.iter()
    }

    pub(crate) fn fmt_tags_links_inline(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format(f, &self.tags, plain, SPACE, Some(SPACE))?;
        format(f, &self.links, plain, SPACE, Some(SPACE))
    }

    pub(crate) fn fmt_keys_values(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format(
            f,
            &self.key_values,
            key_value,
            NEWLINE_INDENT,
            Some(NEWLINE_INDENT),
        )
    }
}

impl<'a> Display for Metadata<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_keys_values(f)?;
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
            .filter(|c| (!TagOrLinkIdentifier::is_valid_char(c)))
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

impl<'a> AsRef<str> for Key<'a> {
    fn as_ref(&self) -> &str {
        self.0
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
                    .filter(|c| (!Key::is_valid_subsequent(c)))
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
    value: Decimal,
    expr: Expr,
}

impl ExprValue {
    pub fn value(&self) -> Decimal {
        self.value
    }
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
pub enum ScopedExprValue {
    PerUnit(ExprValue),
    Total(ExprValue),
}

impl Display for ScopedExprValue {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        use self::ScopedExprValue::*;

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

impl<'a> Display for Amount<'a> {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        write!(format, "{} {}", &self.number, &self.currency)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// An amount where each element may not actually be specified.
pub struct LooseAmount<'a> {
    number: Option<Spanned<ExprValue>>,
    currency: Option<Spanned<&'a Currency<'a>>>,
}

impl<'a> LooseAmount<'a> {
    pub fn new(
        amount: (
            Option<Spanned<ExprValue>>,
            Option<Spanned<&'a Currency<'a>>>,
        ),
    ) -> Self {
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
    BareAmount(ScopedExprValue),
    CurrencyAmount(ScopedExprValue, &'a Currency<'a>),
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
    per_unit: Option<Spanned<ExprValue>>,
    total: Option<Spanned<ExprValue>>,
    currency: Option<Spanned<&'a Currency<'a>>>,
    date: Option<Spanned<Date>>,
    label: Option<Spanned<&'a str>>,
    merge: bool,
}

impl<'a> CostSpec<'a> {
    pub fn per_unit(&self) -> Option<&Spanned<ExprValue>> {
        self.per_unit.as_ref()
    }

    pub fn total(&self) -> Option<&Spanned<ExprValue>> {
        self.total.as_ref()
    }

    pub fn currency(&self) -> Option<&Spanned<&Currency<'_>>> {
        self.currency.as_ref()
    }

    pub fn date(&self) -> Option<&Spanned<Date>> {
        self.date.as_ref()
    }

    pub fn label(&self) -> Option<&Spanned<&str>> {
        self.label.as_ref()
    }

    pub fn merge(&self) -> bool {
        self.merge
    }
}

impl<'a> Display for CostSpec<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(per_unit) = &self.per_unit {
            write!(f, "{} ", per_unit)?;
        }

        if let Some(total) = &self.total {
            write!(f, "# {} ", total)?;
        }

        if let Some(currency) = &self.currency {
            write!(f, "{} ", currency)?;
        }

        if let Some(date) = &self.date {
            write!(f, "{} ", date)?;
        }

        if let Some(label) = &self.label {
            write!(f, "\"{}\" ", label)?;
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
    per_unit: Option<Spanned<ExprValue>>,
    total: Option<Spanned<ExprValue>>,
    currency: Option<Spanned<&'a Currency<'a>>>,
    date: Option<Spanned<Date>>,
    label: Option<Spanned<&'a str>>,
    merge: bool,
    errors: Vec<CostSpecError>,
}

impl<'a> CostSpecBuilder<'a> {
    pub fn compound_expr(self, value: ScopedExprValue, span: Span) -> Self {
        use ScopedExprValue::*;

        match value {
            PerUnit(value) => self.per_unit(spanned(value, span)),
            Total(value) => self.total(spanned(value, span)),
        }
    }

    fn per_unit(mut self, value: Spanned<ExprValue>) -> Self {
        if self.per_unit.is_none() {
            self.per_unit = Some(value);
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::PerUnit))
        }
        self
    }

    fn total(mut self, value: Spanned<ExprValue>) -> Self {
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

    pub fn date(mut self, value: Date, span: Span) -> Self {
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

const SPACE: &str = " ";
const NEWLINE: &str = "\n";
const INDENT: &str = "  ";
const NEWLINE_INDENT: &str = "\n  ";

mod tests;
