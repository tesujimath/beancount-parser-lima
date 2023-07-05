use chrono::NaiveDate;
use nonempty::NonEmpty;
use rust_decimal::Decimal;
use std::{
    cmp::max,
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    iter::empty,
    mem::swap,
};
use strum_macros::{Display, EnumString};

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

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Pragma<'a> {
    // TODO
    Placeholder(&'a str),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Transaction<'a> {
    date: NaiveDate,
    flag: Flag,
    payee: Option<&'a str>,
    narration: Option<&'a str>,
    tags: Vec<&'a Tag<'a>>,
    links: Vec<&'a Link<'a>>,
    metadata: Metadata<'a>,
    postings: Vec<Posting<'a>>,
}

impl<'a> Transaction<'a> {
    pub fn new(
        date: NaiveDate,
        flag: Flag,
        payee: Option<&'a str>,
        narration: Option<&'a str>,
        tags: Vec<&'a Tag<'a>>,
        links: Vec<&'a Link<'a>>,
        metadata: Metadata<'a>,
        postings: Vec<Posting<'a>>,
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
            "{} {} {} {} {} {}",
            self.date,
            self.flag,
            self.payee.unwrap_or("-"),
            self.narration.unwrap_or("-"),
            itertools::Itertools::intersperse(
                self.tags.iter().map(|tag| format!("{}", tag)),
                " ".to_string()
            )
            .collect::<String>(),
            itertools::Itertools::intersperse(
                self.links.iter().map(|link| format!("{}", link)),
                " ".to_string()
            )
            .collect::<String>(),
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Open<'a> {
    pub date: NaiveDate,
    pub account: &'a Account<'a>,
    pub currencies: Vec<&'a Currency<'a>>,
    pub booking: Option<&'a str>,
    pub tags: Vec<&'a Tag<'a>>,
    pub links: Vec<&'a Link<'a>>,
    pub metadata: Metadata<'a>,
}

impl<'a> Open<'a> {
    pub fn new(
        date: NaiveDate,
        account: &'a Account<'a>,
        currencies: Vec<&'a Currency<'a>>,
        booking: Option<&'a str>,
        tags: Vec<&'a Tag<'a>>,
        links: Vec<&'a Link<'a>>,
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
            "{} {} {} {} {}",
            self.date,
            self.account,
            itertools::Itertools::intersperse(
                self.currencies.iter().map(|cur| format!("{}", cur)),
                ", ".to_string()
            )
            .collect::<String>(),
            itertools::Itertools::intersperse(
                self.tags.iter().map(|tag| format!("{}", tag)),
                " ".to_string()
            )
            .collect::<String>(),
            itertools::Itertools::intersperse(
                self.links.iter().map(|link| format!("{}", link)),
                " ".to_string()
            )
            .collect::<String>(),
            // TODO metadata
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Commodity<'a> {
    pub date: NaiveDate,
    pub currency: &'a Currency<'a>,
    pub tags: Vec<&'a Tag<'a>>,
    pub links: Vec<&'a Link<'a>>,
    pub metadata: Metadata<'a>,
}

impl<'a> Commodity<'a> {
    pub fn new(
        date: NaiveDate,
        currency: &'a Currency<'a>,
        tags: Vec<&'a Tag<'a>>,
        links: Vec<&'a Link<'a>>,
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
            "{} {} {} {}",
            self.date,
            self.currency,
            itertools::Itertools::intersperse(
                self.tags.iter().map(|tag| format!("{}", tag)),
                " ".to_string()
            )
            .collect::<String>(),
            itertools::Itertools::intersperse(
                self.links.iter().map(|link| format!("{}", link)),
                " ".to_string()
            )
            .collect::<String>(),
            // TODO metadata
        )
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
pub struct Posting<'a> {
    pub flag: Option<Flag>,
    pub account: &'a Account<'a>,
    pub amount: Option<Expr>,
    pub currency: Option<&'a Currency<'a>>,
    pub cost_spec: Option<CostSpec<'a>>,
    pub metadata: Metadata<'a>,
}

#[derive(PartialEq, Eq, Clone, Default, Debug)]
pub struct Metadata<'a> {
    pub key_values: Vec<(&'a Key<'a>, MetaValue<'a>)>,
    pub tags: Vec<&'a Tag<'a>>,
    pub links: Vec<&'a Link<'a>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MetaValue<'a> {
    Simple(SimpleValue<'a>),
    Amount(Amount<'a>),
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
#[derive(PartialEq, Eq, Clone, Debug)]
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Amount<'a> {
    number: Expr,
    currency: &'a Currency<'a>,
}

impl<'a> Amount<'a> {
    pub fn new(amount: (Expr, &'a Currency<'a>)) -> Self {
        Amount {
            number: amount.0,
            currency: amount.1,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// An amount where each element may not actually be specified.
pub struct LooseAmount<'a> {
    number: Option<Expr>,
    currency: Option<&'a Currency<'a>>,
}

impl<'a> LooseAmount<'a> {
    pub fn new(amount: (Option<Expr>, Option<&'a Currency<'a>>)) -> Self {
        LooseAmount {
            number: amount.0,
            currency: amount.1,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct CostSpec<'a> {
    per_unit: Option<Expr>,
    total: Option<Expr>,
    currency: Option<&'a Currency<'a>>,
    date: Option<NaiveDate>,
    label: Option<&'a str>,
    merge: bool,
}

#[derive(Default, Debug)]
/// Only allows setting each field once, and requires at least one field to be set before building.
pub struct CostSpecBuilder<'a> {
    per_unit: Option<Expr>,
    total: Option<Expr>,
    currency: Option<&'a Currency<'a>>,
    date: Option<NaiveDate>,
    label: Option<&'a str>,
    merge: bool,
    errors: Vec<CostSpecError>,
}

impl<'a> CostSpecBuilder<'a> {
    pub fn compound_expr(self, value: CompoundExpr) -> Self {
        use CompoundExpr::*;

        match value {
            PerUnit(value) => self.per_unit(value),
            Total(value) => self.total(value),
        }
    }

    fn per_unit(mut self, value: Expr) -> Self {
        if self.per_unit.is_none() {
            self.per_unit = Some(value);
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::PerUnit))
        }
        self
    }

    fn total(mut self, value: Expr) -> Self {
        if self.total.is_none() {
            self.total = Some(value);
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Total))
        }
        self
    }

    pub fn currency(mut self, value: &'a Currency<'a>) -> Self {
        if self.currency.is_none() {
            self.currency = Some(value);
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Currency))
        }
        self
    }

    pub fn date(mut self, value: NaiveDate) -> Self {
        if self.date.is_none() {
            self.date = Some(value);
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Date))
        }
        self
    }

    pub fn label(mut self, value: &'a str) -> Self {
        if self.label.is_none() {
            self.label = Some(value);
        } else {
            self.errors.push(CostSpecError(CostSpecErrorKind::Label))
        }
        self
    }

    pub fn merge(mut self) -> Self {
        if !self.merge {
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

impl Error for CostSpecError {}

#[derive(PartialEq, Eq, Debug)]
pub struct CostSpecErrors(Vec<CostSpecError>);

impl Display for CostSpecErrors {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            itertools::Itertools::intersperse(
                self.0.iter().map(|e| format!("{}", e)),
                ", ".to_string()
            )
            .collect::<String>(),
        )
    }
}

impl Error for CostSpecErrors {}

mod tests;
