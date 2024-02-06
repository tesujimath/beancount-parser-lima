use crate::{format::*, options::BeancountOption};
use chumsky::{
    extra::ParserExtra,
    input::{Input, MapExtra},
};
use rust_decimal::Decimal;
use smallvec::SmallVec;
use std::marker::PhantomData;
use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
    hash::{Hash, Hasher},
    iter::empty,
    mem::swap,
    ops::Deref,
};
use strum_macros::{Display, EnumIter, EnumString, IntoStaticStr};
use time::Date;

/// Error or warning, according to the marker type with which it is instantiated.
#[derive(Clone, Debug)]
pub struct ErrorOrWarning<K>
where
    K: ErrorOrWarningKind,
{
    pub(crate) message: String,
    pub(crate) reason: String,
    pub(crate) span: Span,
    pub(crate) contexts: Vec<(String, Span)>,
    pub(crate) related: Vec<(String, Span)>,
    kind: PhantomData<K>,
}

/// Marker type for [ErrorOrWarning] error.
#[derive(Clone, Debug)]
pub struct ErrorKind;

/// The type of errors returned by the parser.
/// All that can usefully be done with these is write them via `BeancountSources`.
pub type Error = ErrorOrWarning<ErrorKind>;

/// Marker type for [ErrorOrWarning] warning.
#[derive(Clone, Debug)]
pub struct WarningKind;

/// The type of warnings returned by the parser.
/// All that can usefully be done with these is write them via `BeancountSources`.
pub type Warning = ErrorOrWarning<WarningKind>;

impl Error {
    pub(crate) fn new<M: Into<String>, R: Into<String>>(message: M, reason: R, span: Span) -> Self {
        ErrorOrWarning {
            message: message.into(),
            reason: reason.into(),
            span,
            contexts: Vec::new(),
            related: Vec::new(),
            kind: PhantomData,
        }
    }

    pub(crate) fn with_contexts<M: Into<String>, R: Into<String>>(
        message: M,
        reason: R,
        span: Span,
        contexts: Vec<(String, Span)>,
    ) -> Self {
        ErrorOrWarning {
            message: message.into(),
            reason: reason.into(),
            span,
            contexts,
            related: Vec::new(),
            kind: PhantomData,
        }
    }
}

impl Warning {
    pub(crate) fn new<M: Into<String>, R: Into<String>>(message: M, reason: R, span: Span) -> Self {
        ErrorOrWarning {
            message: message.into(),
            reason: reason.into(),
            span,
            contexts: Vec::new(),
            related: Vec::new(),
            kind: PhantomData,
        }
    }
}

impl<K> ErrorOrWarning<K>
where
    K: ErrorOrWarningKind,
{
    /// Annotate an error or warning as being related to another parsed element.
    pub fn related_to<'a, T>(self, element: &'a Spanned<T>) -> Self
    where
        T: ElementType + 'a,
    {
        let mut e = self;
        e.related
            .push((element.element_type().to_string(), element.span));
        e
    }

    /// Annotate an error or warning as being related to a number of parsed elements.
    pub fn related_to_all<'a, T>(self, elements: impl IntoIterator<Item = &'a Spanned<T>>) -> Self
    where
        T: ElementType + 'a,
    {
        let mut e = self;
        let mut new_related = elements
            .into_iter()
            .map(|element| (element.element_type().to_string(), element.span))
            .collect::<Vec<_>>();
        e.related.append(&mut new_related);
        e
    }

    pub(crate) fn related_to_named_span<S>(self, name: S, span: Span) -> Self
    where
        S: ToString,
    {
        let mut e = self;
        e.related.push((name.to_string(), span));
        e
    }

    /// Annotate an error or warning as being in the context of another parsed elememt,
    /// for example an error on a [Posting] being in the context of its [Transaction].
    pub fn in_context<T>(self, element: &Spanned<T>) -> Self
    where
        T: ElementType,
    {
        let mut e = self;
        e.contexts
            .push((element.element_type().to_string(), element.span));
        e
    }

    pub(crate) fn color(&self) -> ariadne::Color {
        K::color()
    }

    pub(crate) fn report_kind(&self) -> ariadne::ReportKind<'static> {
        K::report_kind()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use chumsky::span::Span;

        write!(f, "{} ({}) ", self.message, self.reason)?;
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

/// Trait for formatting of [Error] separately from [Warning].
pub trait ErrorOrWarningKind {
    fn report_kind() -> ariadne::ReportKind<'static>;

    fn color() -> ariadne::Color;
}

impl ErrorOrWarningKind for ErrorKind {
    fn report_kind() -> ariadne::ReportKind<'static> {
        ariadne::ReportKind::Error
    }

    fn color() -> ariadne::Color {
        ariadne::Color::Red
    }
}

impl ErrorOrWarningKind for WarningKind {
    fn report_kind() -> ariadne::ReportKind<'static> {
        ariadne::ReportKind::Warning
    }

    fn color() -> ariadne::Color {
        ariadne::Color::Yellow
    }
}

/// Top-level account type, the prefix of any fully-qualified [Account].
#[derive(PartialEq, Eq, Hash, Clone, Copy, EnumString, EnumIter, IntoStaticStr, Debug)]
pub enum AccountType {
    Assets,
    Liabilities,
    Equity,
    Income,
    Expenses,
}

impl AsRef<str> for AccountType {
    fn as_ref(&self) -> &'static str {
        self.into()
    }
}

/// A flag on a [Posting] or [Transaction].
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

impl ElementType for Flag {
    fn element_type(&self) -> &'static str {
        "flag"
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
            Letter(FlagLetter(c)) => (Some('\''), *c),
        };

        match prefix {
            Some(prefix) => write!(f, "{}{}", prefix, c),
            None => write!(f, "{}", c),
        }
    }
}

/// A flag other than one of the builtin ones.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub struct FlagLetter(char);

impl FlagLetter {
    /// Field accessor.
    pub fn char(&self) -> char {
        self.0
    }

    pub(crate) fn is_valid(c: &char) -> bool {
        c.is_ascii_uppercase()
    }
}

/// Error type for invalid [FlagLetter].
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

/// The booking method for an account.
#[derive(
    EnumString, EnumIter, IntoStaticStr, PartialEq, Eq, Default, Clone, Copy, Display, Debug,
)]
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

impl AsRef<str> for Booking {
    fn as_ref(&self) -> &'static str {
        self.into()
    }
}

impl ElementType for Booking {
    fn element_type(&self) -> &'static str {
        "booking"
    }
}

#[derive(
    EnumString, EnumIter, IntoStaticStr, PartialEq, Eq, Default, Clone, Copy, Display, Debug,
)]
#[strum(serialize_all = "snake_case")]
pub enum PluginProcessingMode {
    #[default]
    Default,
    Raw,
}

/// Identifies a source file.
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

/// Identifies a span of text within its source file.
pub type Span = chumsky::span::SimpleSpan<usize, SourceId>;

/// A Spanned item is one located within its source file.
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

pub(crate) fn spanned<T>(item: T, span: Span) -> Spanned<T> {
    Spanned { item, span }
}

/// for use with `map_with`
pub(crate) fn spanned_extra<'a, 'b, T, I, E>(item: T, e: &mut MapExtra<'a, 'b, I, E>) -> Spanned<T>
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
    /// Field accessor.
    pub fn item(&self) -> &T {
        &self.item
    }

    /// Field accessor.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Converts from `&Spanned<T>` to `Spanned<&T>`.
    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            item: &self.item,
            span: self.span,
        }
    }

    /// Maps a `Spanned<T>` to `Spanned<U>` by applying a function to the contained item, keeping the same span.
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

impl<T> Spanned<T>
where
    T: ElementType,
{
    /// Create a new `Error` referring to the spanned element.
    pub fn error<S: Into<String>>(&self, reason: S) -> Error {
        Error::new(
            format!("invalid {}", self.element_type()),
            reason,
            self.span,
        )
    }

    /// Create a new `Warning` referring to the spanned element.
    pub fn warning<S: Into<String>>(&self, reason: S) -> Warning {
        Warning::new(
            format!("questionable {}", self.element_type()),
            reason,
            self.span,
        )
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

/// Implemented by any element, for access to its kind in error reporting.
pub trait ElementType {
    fn element_type(&self) -> &'static str;
}

// blanket implementation for references
impl<T> ElementType for &T
where
    T: ElementType,
{
    fn element_type(&self) -> &'static str {
        (**self).element_type()
    }
}

impl<T> ElementType for &mut T
where
    T: ElementType,
{
    fn element_type(&self) -> &'static str {
        (**self).element_type()
    }
}

impl<T> ElementType for Box<T>
where
    T: ElementType,
{
    fn element_type(&self) -> &'static str {
        (**self).element_type()
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) enum Declaration<'a> {
    Directive(Directive<'a>),
    Pragma(Pragma<'a>),
}

/// A Beancount directive of a particular [DirectiveVariant].
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Directive<'a> {
    pub(crate) date: Spanned<Date>,
    pub(crate) metadata: Metadata<'a>,
    pub(crate) variant: DirectiveVariant<'a>,
}

impl<'a> Directive<'a> {
    /// Field accessor.
    pub fn date(&self) -> &Spanned<Date> {
        &self.date
    }

    /// Field accessor.
    pub fn metadata(&self) -> &Metadata {
        &self.metadata
    }

    /// Field accessor.
    pub fn variant(&self) -> &DirectiveVariant {
        &self.variant
    }
}

impl<'a> ElementType for Directive<'a> {
    fn element_type(&self) -> &'static str {
        use DirectiveVariant::*;

        match &self.variant {
            Transaction(_) => "transaction",
            Price(_) => "price",
            Balance(_) => "balance",
            Open(_) => "open",
            Close(_) => "close",
            Commodity(_) => "commodity",
            Pad(_) => "pad",
            Document(_) => "document",
            Note(_) => "note",
            Event(_) => "event",
            Query(_) => "query",
        }
    }
}

impl<'a> Display for Directive<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use DirectiveVariant::*;

        match &self.variant {
            Transaction(x) => x.fmt(f, self.date.item, &self.metadata),
            Price(x) => x.fmt(f, self.date.item, &self.metadata),
            Balance(x) => x.fmt(f, self.date.item, &self.metadata),
            Open(x) => x.fmt(f, self.date.item, &self.metadata),
            Close(x) => x.fmt(f, self.date.item, &self.metadata),
            Commodity(x) => x.fmt(f, self.date.item, &self.metadata),
            Pad(x) => x.fmt(f, self.date.item, &self.metadata),
            Document(x) => x.fmt(f, self.date.item, &self.metadata),
            Note(x) => x.fmt(f, self.date.item, &self.metadata),
            Event(x) => x.fmt(f, self.date.item, &self.metadata),
            Query(x) => x.fmt(f, self.date.item, &self.metadata),
        }
    }
}

/// A Beancount directive, without the fields common to all, which belong to [Directive].
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum DirectiveVariant<'a> {
    Transaction(Transaction<'a>),
    Price(Price<'a>),
    Balance(Balance<'a>),
    Open(Open<'a>),
    Close(Close<'a>),
    Commodity(Commodity<'a>),
    Pad(Pad<'a>),
    Document(Document<'a>),
    Note(Note<'a>),
    Event(Event<'a>),
    Query(Query<'a>),
    // TODO Custom
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) enum Pragma<'a> {
    // we keep pushed tags with their span
    // since it may be useful downstream to know where these came from
    Pushtag(Spanned<&'a Tag<'a>>),
    Poptag(Spanned<&'a Tag<'a>>),
    Pushmeta(MetaKeyValue<'a>),
    Popmeta(Spanned<Key<'a>>),
    Include(&'a str),
    Option(BeancountOption<'a>),
    Plugin(Plugin<'a>),
}

/// A Beancount transaction directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
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
        )
    }

    /// Field accessor.
    pub fn flag(&self) -> &Spanned<Flag> {
        &self.flag
    }

    /// Field accessor.
    pub fn payee(&self) -> Option<&Spanned<&str>> {
        self.payee.as_ref()
    }

    /// Field accessor.
    pub fn narration(&self) -> Option<&Spanned<&str>> {
        self.narration.as_ref()
    }

    /// Field accessor.
    pub fn postings(&self) -> impl ExactSizeIterator<Item = &Spanned<Posting>> {
        self.postings.iter()
    }
}

/// A Beancount price directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Price<'a> {
    pub(crate) currency: Spanned<&'a Currency<'a>>,
    pub(crate) amount: Spanned<Amount<'a>>,
}

impl<'a> Price<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} price {} {}", date, &self.currency, &self.amount)?;

        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)?;

        Ok(())
    }

    /// Field accessor.
    pub fn currency(&self) -> &Spanned<&Currency> {
        &self.currency
    }

    /// Field accessor.
    pub fn amount(&self) -> &Spanned<Amount> {
        &self.amount
    }
}

impl<'a> ElementType for Price<'a> {
    fn element_type(&self) -> &'static str {
        "price"
    }
}

/// A Beancount balance directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Balance<'a> {
    pub(crate) account: Spanned<Account<'a>>,
    pub(crate) atol: Spanned<AmountWithTolerance<'a>>,
}

impl<'a> Balance<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} balance {} {}", date, &self.account, &self.atol)?;

        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)?;

        Ok(())
    }

    /// Field accessor.
    pub fn account(&self) -> &Spanned<Account> {
        &self.account
    }

    /// Field accessor.
    pub fn atol(&self) -> &Spanned<AmountWithTolerance> {
        &self.atol
    }
}

impl<'a> ElementType for Balance<'a> {
    fn element_type(&self) -> &'static str {
        "balance"
    }
}

/// A Beancount open directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Open<'a> {
    pub(crate) account: Spanned<Account<'a>>,
    pub(crate) currencies: HashSet<Spanned<&'a Currency<'a>>>,
    pub(crate) booking: Option<Spanned<Booking>>,
}

impl<'a> Open<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} open {}", date, self.account)?;
        format(f, &self.currencies, plain, ",", Some(" "))?;
        format(f, &self.booking, double_quoted, " ", Some(" "))?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }

    /// Field accessor.
    pub fn account(&self) -> &Spanned<Account> {
        &self.account
    }

    /// Field accessor.
    pub fn currencies(&self) -> impl ExactSizeIterator<Item = &Spanned<&Currency>> {
        self.currencies.iter()
    }

    /// Field accessor.
    pub fn booking(&self) -> Option<&Spanned<Booking>> {
        self.booking.as_ref()
    }
}

/// A Beancount close directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Close<'a> {
    pub(crate) account: Spanned<Account<'a>>,
}

impl<'a> Close<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} close {}", date, self.account)?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }

    /// Field accessor.
    pub fn account(&self) -> &Spanned<Account> {
        &self.account
    }
}

/// A Beancount commodity directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Commodity<'a> {
    pub(crate) currency: Spanned<&'a Currency<'a>>,
}

impl<'a> Commodity<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} commodity {}", date, self.currency)?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }

    /// Field accessor.
    pub fn currency(&self) -> &Spanned<&Currency> {
        &self.currency
    }
}

/// A Beancount pad directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Pad<'a> {
    pub(crate) account: Spanned<Account<'a>>,
    pub(crate) source: Spanned<Account<'a>>,
}

impl<'a> Pad<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} pad {} {}", date, self.account, self.source)?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }

    /// Field accessor.
    pub fn account(&self) -> &Spanned<Account> {
        &self.account
    }

    /// Field accessor.
    pub fn source(&self) -> &Spanned<Account> {
        &self.source
    }
}

/// A Beancount document directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Document<'a> {
    pub(crate) account: Spanned<Account<'a>>,
    pub(crate) path: Spanned<&'a str>,
}

impl<'a> Document<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} document {} \"{}\"", date, self.account, self.path)?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }

    /// Field accessor.
    pub fn account(&self) -> &Spanned<Account> {
        &self.account
    }

    /// The path to the document, possibly relative to a directory given by options.
    /// No check is made as to validity of the path or existence of the file.
    pub fn path(&self) -> &Spanned<&str> {
        &self.path
    }
}

/// A Beancount note directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Note<'a> {
    pub(crate) account: Spanned<Account<'a>>,
    pub(crate) comment: Spanned<&'a str>,
}

impl<'a> Note<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} note {} \"{}\"", date, self.account, self.comment)?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }

    /// Field accessor.
    pub fn account(&self) -> &Spanned<Account> {
        &self.account
    }

    /// Field accessor.
    pub fn comment(&self) -> &Spanned<&str> {
        &self.comment
    }
}

/// A Beancount event directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Event<'a> {
    pub(crate) event_type: Spanned<&'a str>,
    pub(crate) description: Spanned<&'a str>,
}

impl<'a> Event<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(
            f,
            "{} event \"{}\" \"{}\"",
            date, self.event_type, self.description
        )?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }

    /// Field accessor.
    pub fn event_type(&self) -> &Spanned<&str> {
        &self.event_type
    }

    /// Field accessor.
    pub fn description(&self) -> &Spanned<&str> {
        &self.description
    }
}

/// A Beancount query directive, without the common [Directive] fields.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Query<'a> {
    pub(crate) name: Spanned<&'a str>,
    pub(crate) content: Spanned<&'a str>,
}

impl<'a> Query<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, date: Date, metadata: &Metadata) -> fmt::Result {
        write!(f, "{} query \"{}\" \"{}\"", date, self.name, self.content)?;
        // we prefer to show tags and links inline rather then line by line in metadata
        metadata.fmt_tags_links_inline(f)?;
        metadata.fmt_keys_values(f)
    }

    /// Field accessor.
    pub fn name(&self) -> &Spanned<&str> {
        &self.name
    }

    /// Field accessor.
    pub fn content(&self) -> &Spanned<&str> {
        &self.content
    }
}

/// A Beancount plugin pragma.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Plugin<'a> {
    pub(crate) module_name: Spanned<&'a str>,
    pub(crate) config: Option<Spanned<&'a str>>,
}

impl<'a> Plugin<'a> {
    /// Field accessor.
    pub fn module_name(&self) -> &Spanned<&str> {
        &self.module_name
    }

    /// Field accessor.
    pub fn config(&self) -> Option<&Spanned<&str>> {
        self.config.as_ref()
    }
}

impl<'a> Display for Plugin<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "plugin \"{}\"", self.module_name)?;
        if let Some(config) = &self.config {
            write!(f, " \"{}\"", config)?;
        }
        writeln!(f)
    }
}

/// A Beancount account with account type and subaccount names.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Account<'a> {
    pub(crate) account_type: AccountType,
    pub(crate) candidate: &'a CandidateAccount<'a>,
}

impl<'a> Account<'a> {
    /// Field accessor.
    pub fn account_type(&self) -> AccountType {
        self.account_type
    }

    /// Field accessor.
    pub fn names(&self) -> &Subaccount {
        &self.candidate.subaccount
    }
}

impl<'a> ElementType for Account<'a> {
    fn element_type(&self) -> &'static str {
        "account"
    }
}

impl<'a> Display for Account<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.account_type.as_ref())?;
        format(f, self.names(), plain, ":", Some(":"))
    }
}

/// The individual colon-separated components of an account, without the [AccountType] prefix.
/// `SmallVec` stores a small number of these inline, before making use of the heap.
pub type Subaccount<'a> = SmallVec<AccountName<'a>, 4>;

/// A CandidateAccount is one where the account_type_name has not yet been resolved against current options.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct CandidateAccount<'a> {
    pub(crate) account_type_name: AccountTypeName<'a>,
    pub(crate) subaccount: Subaccount<'a>,
}

impl<'a> Display for CandidateAccount<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.account_type_name)?;
        format(f, &self.subaccount, plain, ":", Some(":"))
    }
}

/// A validated name for an account type.
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub struct AccountTypeName<'a>(&'a str);

impl<'a> AccountTypeName<'a> {
    pub(crate) fn is_valid_initial(c: &char) -> bool {
        c.is_ascii_uppercase()
    }

    pub(crate) fn is_valid_subsequent(c: &char) -> bool {
        c.is_alphanumeric() || *c == '-'
    }
}

impl<'a> TryFrom<&'a str> for AccountTypeName<'a> {
    type Error = AccountTypeNameError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        use AccountTypeNameErrorKind::*;
        if s.is_empty() {
            Err(AccountTypeNameError(Empty))
        } else {
            let mut chars = s.chars();
            let initial = chars.next().unwrap();
            if !AccountTypeName::is_valid_initial(&initial) {
                Err(AccountTypeNameError(Initial(initial)))
            } else {
                let bad_chars = chars
                    .filter(|c| (!AccountTypeName::is_valid_subsequent(c)))
                    .collect::<Vec<char>>();
                if bad_chars.is_empty() {
                    Ok(AccountTypeName(s))
                } else {
                    Err(AccountTypeNameError(Subsequent(bad_chars)))
                }
            }
        }
    }
}

impl<'a> ElementType for AccountTypeName<'a> {
    fn element_type(&self) -> &'static str {
        "account type name"
    }
}

impl<'a> AsRef<str> for AccountTypeName<'a> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl<'a> Display for AccountTypeName<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

/// Error type for [AccountTypeName] creation.
#[derive(PartialEq, Eq, Debug)]
pub struct AccountTypeNameError(AccountTypeNameErrorKind);

#[derive(PartialEq, Eq, Debug)]
enum AccountTypeNameErrorKind {
    Empty,
    Initial(char),
    Subsequent(Vec<char>),
}

impl Display for AccountTypeNameError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AccountTypeNameErrorKind::*;
        match &self.0 {
            Empty => write!(f, "empty account name"),
            Initial(bad_char) => write!(
                f,
                "invalid character '{}' for account name initial - must be uppercase ASCII letter",
                bad_char
            ),
            Subsequent(bad_chars) => {
                format(
                    f,
                    bad_chars,
                    single_quoted,
                    ", ",
                    Some("invalid character "),
                )?;
                f.write_str(" in account name - must be alphanumeric or '-'")
            }
        }
    }
}

impl std::error::Error for AccountTypeNameError {}

/// One component of a colon-separated account.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub struct AccountName<'a>(&'a str);

impl<'a> AccountName<'a> {
    pub(crate) fn is_valid_initial(c: &char) -> bool {
        c.is_ascii_uppercase() || c.is_ascii_digit()
    }

    pub(crate) fn is_valid_subsequent(c: &char) -> bool {
        c.is_alphanumeric() || *c == '-'
    }
}

impl<'a> ElementType for AccountName<'a> {
    fn element_type(&self) -> &'static str {
        "account name"
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

/// Error type for [AccountName] creation.
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
                format(f, bad_chars, single_quoted, ", ", Some("invalid character "))?;
                f.write_str(" in account name - must be alphanumeric or '-'")
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

/// A Beancount currency.
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
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

impl<'a> ElementType for Currency<'a> {
    fn element_type(&self) -> &'static str {
        "currency"
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

/// Error type for [Currency] creation.
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

/// A single posting within a [Transaction].
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Posting<'a> {
    pub(crate) flag: Option<Spanned<Flag>>,
    pub(crate) account: Spanned<Account<'a>>,
    pub(crate) amount: Option<Spanned<ExprValue>>,
    pub(crate) currency: Option<Spanned<&'a Currency<'a>>>,
    pub(crate) cost_spec: Option<Spanned<CostSpec<'a>>>,
    pub(crate) price_annotation: Option<Spanned<ScopedAmount<'a>>>,
    pub(crate) metadata: Metadata<'a>,
}

impl<'a> Posting<'a> {
    /// Field accessor.
    pub fn flag(&self) -> Option<&Spanned<Flag>> {
        self.flag.as_ref()
    }

    /// Field accessor.
    pub fn account(&self) -> &Spanned<Account> {
        &self.account
    }

    /// Field accessor.
    pub fn amount(&self) -> Option<&Spanned<ExprValue>> {
        self.amount.as_ref()
    }

    /// Field accessor.
    pub fn currency(&self) -> Option<&Spanned<&Currency>> {
        self.currency.as_ref()
    }

    /// Field accessor.
    pub fn cost_spec(&self) -> Option<&Spanned<CostSpec>> {
        self.cost_spec.as_ref()
    }

    /// Field accessor.
    pub fn price_annotation(&self) -> Option<&Spanned<ScopedAmount>> {
        self.price_annotation.as_ref()
    }

    /// Field accessor.
    pub fn metadata(&self) -> &Metadata<'a> {
        &self.metadata
    }
}

impl<'a> ElementType for Posting<'a> {
    fn element_type(&self) -> &'static str {
        "posting"
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
        simple_format(f, &self.price_annotation, Some(" @ "))?;

        self.metadata.fmt(f)
    }
}

/// Metadata associated with a [Directive] or a [Posting].
///
/// Note that tags and links and key/values that may have been specified at the top-level
/// of a directive are subsumed into the metadata element of the directive.
#[derive(PartialEq, Eq, Clone, Default, Debug)]
pub struct Metadata<'a> {
    pub(crate) key_values: HashMap<Spanned<Key<'a>>, Spanned<MetaValue<'a>>>,
    pub(crate) tags: HashSet<Spanned<&'a Tag<'a>>>,
    pub(crate) links: HashSet<Spanned<&'a Link<'a>>>,
}

impl<'a> Metadata<'a> {
    /// Field accessor.
    pub fn key_values(
        &self,
    ) -> impl ExactSizeIterator<Item = (&Spanned<Key>, &Spanned<MetaValue>)> {
        self.key_values.iter()
    }

    /// Field accessor.
    pub fn tags(&self) -> impl ExactSizeIterator<Item = &Spanned<&Tag>> {
        self.tags.iter()
    }

    /// Field accessor.
    pub fn links(&self) -> impl ExactSizeIterator<Item = &Spanned<&Link>> {
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
pub(crate) struct MetaKeyValue<'a> {
    pub(crate) key: Spanned<Key<'a>>,
    pub(crate) value: Spanned<MetaValue<'a>>,
}

impl<'a> Display for MetaKeyValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: \"{}\"", &self.key, &self.value)
    }
}

/// A value of metadata key/value.
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

/// One possible type of [MetaValue].
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum SimpleValue<'a> {
    String(&'a str),
    Currency(&'a Currency<'a>),
    Account(Account<'a>),
    Tag(&'a Tag<'a>),
    Link(&'a Link<'a>),
    Date(Date),
    Bool(bool),
    None,
    Expr(ExprValue),
}

impl<'a> ElementType for SimpleValue<'a> {
    fn element_type(&self) -> &'static str {
        "simple value"
    }
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
            Bool(x) => f.write_str(if *x { "TRUE" } else { "FALSE" }),
            None => Ok(()),
            Expr(x) => x.fmt(f),
        }
    }
}

/// A tag.
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
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

impl<'a> ElementType for Tag<'a> {
    fn element_type(&self) -> &'static str {
        "tag"
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

/// A link.
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
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

impl<'a> ElementType for Link<'a> {
    fn element_type(&self) -> &'static str {
        "link"
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

/// The validated identifier part of a `Tag` or `Link` without the `#` or `^` prefix.
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub struct TagOrLinkIdentifier<'a>(&'a str);

/// The valid characters for tags and links besides alphanumeric.
const TAG_OR_LINK_EXTRA_CHARS: [char; 4] = ['-', '_', '/', '.'];

impl<'a> TagOrLinkIdentifier<'a> {
    pub(crate) fn is_valid_char(c: &char) -> bool {
        c.is_alphanumeric() || TAG_OR_LINK_EXTRA_CHARS.contains(c)
    }
}

/// Error type for [TagOrLinkIdentifier] creation.
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

/// A key for a [Metadata] [MetaValue].
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub struct Key<'a>(&'a str);

impl<'a> Key<'a> {
    pub(crate) fn is_valid_initial(c: &char) -> bool {
        c.is_ascii_lowercase()
    }

    pub(crate) fn is_valid_subsequent(c: &char) -> bool {
        c.is_alphanumeric() || *c == '-' || *c == '_'
    }
}

impl<'a> AsRef<str> for Key<'a> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl<'a> ElementType for Key<'a> {
    fn element_type(&self) -> &'static str {
        "key"
    }
}

impl<'a> Display for Key<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.0)
    }
}

/// Error type for [Key] creation.
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
                f.write_str(" for key - must be alphanumeric or '-' or '_'")
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
///
/// Note that the [decimal scale](https://docs.rs/rust_decimal/latest/rust_decimal/index.html) is set according to the maximum of the scales used within the expression.
pub struct ExprValue {
    value: Decimal,
    expr: Expr,
}

impl ExprValue {
    /// Field accessor.
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

//
impl ElementType for ExprValue {
    fn element_type(&self) -> &'static str {
        "amount" // is there a better user-facing name?
    }
}

impl Display for ExprValue {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.expr, format)
    }
}

/// A numeric expression which respects standard operator precedence.
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

/// An `ExprValue` which quantifies a total or per-unit.
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

/// A `ExprValue` and `Currency`.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Amount<'a> {
    number: Spanned<ExprValue>,
    currency: Spanned<&'a Currency<'a>>,
}

impl<'a> Amount<'a> {
    pub(crate) fn new(amount: (Spanned<ExprValue>, Spanned<&'a Currency<'a>>)) -> Self {
        Amount {
            number: amount.0,
            currency: amount.1,
        }
    }

    /// Field accessor.
    pub fn number(&self) -> &Spanned<ExprValue> {
        &self.number
    }

    /// Field accessor.
    pub fn currency(&self) -> &Spanned<&Currency> {
        &self.currency
    }
}

impl<'a> Display for Amount<'a> {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        write!(format, "{} {}", &self.number, &self.currency)
    }
}

/// An `Amount` with optional tolerance.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct AmountWithTolerance<'a> {
    amount: Spanned<Amount<'a>>,
    tolerance: Option<Spanned<Decimal>>,
}

impl<'a> AmountWithTolerance<'a> {
    pub(crate) fn new(awt: (Spanned<Amount<'a>>, Option<Spanned<Decimal>>)) -> Self {
        AmountWithTolerance {
            amount: awt.0,
            tolerance: awt.1,
        }
    }

    /// Field accessor.
    pub fn amount(&self) -> &Spanned<Amount> {
        &self.amount
    }

    /// Field accessor.
    pub fn tolerance(&self) -> Option<&Spanned<Decimal>> {
        self.tolerance.as_ref()
    }
}

impl<'a> Display for AmountWithTolerance<'a> {
    fn fmt(&self, format: &mut Formatter<'_>) -> fmt::Result {
        if let Some(tolerance) = self.tolerance {
            write!(format, "{} ~ {}", &self.amount, tolerance)
        } else {
            write!(format, "{}", &self.amount)
        }
    }
}

/// An amount where each element of `ExprValue` and `Currency` may not actually be specified.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LooseAmount<'a> {
    number: Option<Spanned<ExprValue>>,
    currency: Option<Spanned<&'a Currency<'a>>>,
}

impl<'a> LooseAmount<'a> {
    pub(crate) fn new(
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

    /// Field accessor.
    pub fn number(&self) -> Option<&Spanned<ExprValue>> {
        self.number.as_ref()
    }

    /// Field accessor.
    pub fn currency(&self) -> Option<&Spanned<&Currency>> {
        self.currency.as_ref()
    }
}

/// An amount which specifies a total or per-unit `ScopedExprValue`, or simply just a `Currency`.
#[derive(PartialEq, Eq, Clone, Debug)]
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

/// A cost specification.
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
    /// Field accessor.
    pub fn per_unit(&self) -> Option<&Spanned<ExprValue>> {
        self.per_unit.as_ref()
    }

    /// Field accessor.
    pub fn total(&self) -> Option<&Spanned<ExprValue>> {
        self.total.as_ref()
    }

    /// Field accessor.
    pub fn currency(&self) -> Option<&Spanned<&Currency<'_>>> {
        self.currency.as_ref()
    }

    /// Field accessor.
    pub fn date(&self) -> Option<&Spanned<Date>> {
        self.date.as_ref()
    }

    /// Field accessor.
    pub fn label(&self) -> Option<&Spanned<&str>> {
        self.label.as_ref()
    }

    /// Field accessor.
    pub fn merge(&self) -> bool {
        self.merge
    }
}

impl<'a> ElementType for CostSpec<'a> {
    fn element_type(&self) -> &'static str {
        "cost specification"
    }
}

impl<'a> Display for CostSpec<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut prefix = "";
        let space = " ";

        f.write_str("{")?;

        if let Some(per_unit) = &self.per_unit {
            write!(f, "{}{}", prefix, per_unit)?;
            prefix = space;
        }

        if let Some(total) = &self.total {
            write!(f, "{}# {}", prefix, total)?;
            prefix = space;
        }

        if let Some(currency) = &self.currency {
            write!(f, "{}{}", prefix, currency)?;
            prefix = space;
        }

        if let Some(date) = &self.date {
            write!(f, "{}{}", prefix, date)?;
            prefix = space;
        }

        if let Some(label) = &self.label {
            write!(f, "{}\"{}\"", prefix, label)?;
            prefix = space;
        }

        if self.merge {
            write!(f, "{}*", prefix)?;
        }

        f.write_str("}")
    }
}

#[derive(Default, Debug)]
/// Only allows setting each field once, and requires at least one field to be set before building.
pub(crate) struct CostSpecBuilder<'a> {
    per_unit: Option<Spanned<ExprValue>>,
    total: Option<Spanned<ExprValue>>,
    currency: Option<Spanned<&'a Currency<'a>>>,
    date: Option<Spanned<Date>>,
    label: Option<Spanned<&'a str>>,
    merge: bool,
    errors: Vec<CostSpecError>,
}

impl<'a> CostSpecBuilder<'a> {
    pub(crate) fn compound_expr(self, value: ScopedExprValue, span: Span) -> Self {
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

    pub(crate) fn currency(mut self, value: &'a Currency<'a>, span: Span) -> Self {
        if self.currency.is_none() {
            self.currency = Some(spanned(value, span));
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Currency))
        }
        self
    }

    pub(crate) fn date(mut self, value: Date, span: Span) -> Self {
        if self.date.is_none() {
            self.date = Some(spanned(value, span));
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Date))
        }
        self
    }

    pub(crate) fn label(mut self, value: &'a str, span: Span) -> Self {
        if self.label.is_none() {
            self.label = Some(spanned(value, span));
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Label))
        }
        self
    }

    pub(crate) fn merge(mut self, _span: Span) -> Self {
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
    pub(crate) fn build<'b>(&'b mut self) -> Result<CostSpec<'a>, CostSpecErrors>
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

/// Error type for [CostSpec] creation.
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

/// Multiple errors arising from creation of a [CostSpec].
#[derive(PartialEq, Eq, Debug)]
pub struct CostSpecErrors(Vec<CostSpecError>);

impl Display for CostSpecErrors {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format(f, &self.0, plain, ", ", None)
    }
}

impl std::error::Error for CostSpecErrors {}

mod tests;
