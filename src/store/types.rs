use crate::parser::types as parser_types;
use crate::types::*;
use nonempty::NonEmpty;
use rust_decimal::Decimal;
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    marker::PhantomData,
};
use time::Date;

/// The same Symbol type for all internalized strings,
/// differentiated by type parameter at compile time.
pub struct Symbol<T>(string_interner::DefaultSymbol, PhantomData<T>);

impl<T> Symbol<T> {
    pub fn new(sym: string_interner::DefaultSymbol) -> Self {
        Symbol(sym, PhantomData)
    }
}

// Because of limitations with #derive as described in
// https://github.com/rust-lang/rust/issues/26925
// we have to manually implement the traits we want for Symbol,
// to avoid requiring them for our phantom types.
impl<T> PartialEq for Symbol<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Symbol<T> {}

impl<T> Hash for Symbol<T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.0.hash(state)
    }
}

impl<T> Copy for Symbol<T> {}

impl<T> Clone for Symbol<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Debug for Symbol<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

/// Type conversion like `From` trait, using a symbol table.
pub trait SymbolFrom<T> {
    fn symbol_from(value: T, symbol_table: &mut impl SymbolTable) -> Self;
}

pub trait SymbolTable {
    fn get_or_intern<S, T>(&mut self, s: S) -> Symbol<T>
    where
        S: AsRef<str>;
}

/// symbol types, for type-checking Symbol's only
pub struct Currency();
pub struct Payee();
pub struct Tag();
pub struct Link();
pub struct Key();
pub struct AccountName();
pub struct Label();

#[derive(Clone, Debug)]
pub struct Transaction {
    pub(crate) date: Spanned<Date>,
    pub(crate) flag: Spanned<Flag>,
    pub(crate) payee: Option<Spanned<Symbol<Payee>>>,
    pub(crate) narration: Option<Spanned<String>>,
    pub(crate) tags: HashSet<Spanned<Symbol<Tag>>>,
    pub(crate) links: HashSet<Spanned<Symbol<Link>>>,
    // tags and links from metadata are folded into above, leaving only key/values
    pub(crate) metadata: HashMap<Spanned<Symbol<Key>>, Spanned<MetaValue>>,
    pub(crate) postings: Vec<Spanned<Posting>>,
}

#[derive(Clone, Debug)]
pub struct Posting {
    pub flag: Option<Spanned<Flag>>,
    pub account: Spanned<Account>,
    pub amount: Option<Spanned<Decimal>>,
    pub currency: Option<Spanned<Symbol<Currency>>>,
    pub cost_spec: Option<Spanned<CostSpec>>,
    pub price_annotation: Option<Spanned<ScopedAmount>>,
    pub metadata: HashMap<Spanned<Symbol<Key>>, Spanned<MetaValue>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct MetaKeyValue {
    pub key: Spanned<Symbol<Key>>,
    pub value: Spanned<MetaValue>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MetaValue {
    Simple(SimpleValue),
    Amount(Amount),
}

impl<'a> SymbolFrom<&'a parser_types::MetaValue<'a>> for MetaValue {
    fn symbol_from(
        value: &'a parser_types::MetaValue<'a>,
        symbol_table: &mut impl SymbolTable,
    ) -> MetaValue {
        use parser_types::MetaValue as PMV;
        //use MetaValue::*;

        match value {
            PMV::Simple(simple) => {
                MetaValue::Simple(SimpleValue::symbol_from(simple, symbol_table))
            }
            PMV::Amount(amount) => MetaValue::Amount(Amount::symbol_from(amount, symbol_table)),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum SimpleValue {
    String(String),
    Currency(Symbol<Currency>),
    Account(Account),
    Tag(Symbol<Tag>),
    Link(Symbol<Link>),
    Date(Date),
    Bool(bool),
    None,
    Value(Decimal),
}

impl<'a> SymbolFrom<&'a parser_types::SimpleValue<'a>> for SimpleValue {
    fn symbol_from(
        value: &'a parser_types::SimpleValue<'a>,
        symbol_table: &mut impl SymbolTable,
    ) -> SimpleValue {
        use parser_types::SimpleValue as PSV;
        use SimpleValue::*;

        match value {
            PSV::String(s) => String(s.to_string()),
            PSV::Currency(cur) => Currency(symbol_table.get_or_intern(cur.as_ref())),
            PSV::Account(account) => Account(self::Account::symbol_from(account, symbol_table)),
            PSV::Tag(tag) => Tag(symbol_table.get_or_intern(tag.as_ref())),
            PSV::Link(link) => Link(symbol_table.get_or_intern(link.as_ref())),
            PSV::Date(date) => Date(*date),
            PSV::Bool(v) => Bool(*v),
            PSV::None => None,
            PSV::Expr(expr) => Value(expr.value),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct Account {
    pub(crate) account_type: AccountType,
    pub(crate) names: NonEmpty<Symbol<AccountName>>,
}

impl<'a> SymbolFrom<&'a parser_types::Account<'a>> for Account {
    fn symbol_from(
        value: &'a parser_types::Account<'a>,
        symbol_table: &mut impl SymbolTable,
    ) -> Account {
        Account {
            account_type: value.account_type,
            names: NonEmpty::collect(
                value
                    .names
                    .iter()
                    .map(|name| symbol_table.get_or_intern(name)),
            )
            .unwrap(),
        }
    }
}

/// A value which quantifies a total or per-unit.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ScopedValue {
    PerUnit(Decimal),
    Total(Decimal),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Amount {
    number: Spanned<Decimal>,
    currency: Spanned<Symbol<Currency>>,
}

impl<'a> SymbolFrom<&'a parser_types::Amount<'a>> for Amount {
    fn symbol_from(
        value: &'a parser_types::Amount<'a>,
        symbol_table: &mut impl SymbolTable,
    ) -> Amount {
        Amount {
            number: value.number().map(|expr| expr.value),
            currency: value.currency().map(|cur| symbol_table.get_or_intern(cur)),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// An amount where each element may not actually be specified.
pub struct LooseAmount {
    number: Option<Spanned<Decimal>>,
    currency: Option<Spanned<Symbol<Currency>>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// An amount which specifies a total or per-unit, or simply just a currency.
pub enum ScopedAmount {
    BareCurrency(Symbol<Currency>),
    BareAmount(ScopedValue),
    CurrencyAmount(ScopedValue, Symbol<Currency>),
}

#[derive(Clone, Debug)]
pub struct CostSpec {
    per_unit: Option<Spanned<Decimal>>,
    total: Option<Spanned<Decimal>>,
    currency: Option<Spanned<Symbol<Currency>>>,
    date: Option<Spanned<Date>>,
    label: Option<Spanned<Symbol<Label>>>,
    merge: bool,
}
