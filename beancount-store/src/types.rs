use std::{
    collections::HashSet,
    fmt::{self, Debug, Formatter},
    marker::PhantomData,
};

use beancount_types::{AccountType, Flag};
use nonempty::NonEmpty;
use rust_decimal::Decimal;
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

/// symbol types, for type-checking Symbol's only
pub struct Currency();
pub struct Payee();
pub struct Tag();
pub struct Link();
pub struct Key();
pub struct AccountName();
pub struct Label();

#[derive(PartialEq, Eq, Clone, Debug)]
// TODO consider hashing for Augmented, maybe don't want the augmentation to count?
// consider how we will look things up.  Bah!
pub struct Augmented<T, A> {
    item: T,
    augmentation: A,
}

#[derive(Clone, Debug)]
pub struct Transaction<A> {
    pub(crate) date: Augmented<Date, A>,
    pub(crate) flag: Augmented<Flag, A>,
    pub(crate) payee: Option<Augmented<Symbol<Payee>, A>>,
    pub(crate) narration: Option<Augmented<String, A>>,
    pub(crate) tags: HashSet<Augmented<Symbol<Tag>, A>>,
    pub(crate) links: HashSet<Augmented<Symbol<Link>, A>>,
    pub(crate) metadata: Metadata<A>,
    pub(crate) postings: Vec<Augmented<Posting<A>, A>>,
}

#[derive(Clone, Debug)]
pub struct Posting<A> {
    pub flag: Option<Augmented<Flag, A>>,
    pub account: Augmented<Account, A>,
    pub amount: Option<Augmented<Decimal, A>>,
    pub currency: Option<Augmented<Symbol<Currency>, A>>,
    pub cost_spec: Option<Augmented<CostSpec<A>, A>>,
    pub price_annotation: Option<Augmented<ScopedAmount, A>>,
    pub metadata: Metadata<A>,
}

#[derive(Clone, Default, Debug)]
pub struct Metadata<A> {
    // TODO should this be a hashmap?
    pub key_values: Vec<Augmented<MetaKeyValue<A>, A>>,
    pub tags: HashSet<Augmented<Symbol<Tag>, A>>,
    pub links: HashSet<Augmented<Symbol<Link>, A>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct MetaKeyValue<A> {
    pub key: Augmented<Symbol<Key>, A>,
    pub value: Augmented<MetaValue<A>, A>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MetaValue<A> {
    Simple(SimpleValue),
    Amount(Amount<A>),
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

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Account {
    pub(crate) account_type: AccountType,
    pub(crate) names: NonEmpty<Symbol<AccountName>>,
}

/// A value which quantifies a total or per-unit.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ScopedValue {
    PerUnit(Decimal),
    Total(Decimal),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Amount<A> {
    number: Augmented<Decimal, A>,
    currency: Augmented<Symbol<Currency>, A>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// An amount where each element may not actually be specified.
pub struct LooseAmount<A> {
    number: Option<Augmented<Decimal, A>>,
    currency: Option<Augmented<Symbol<Currency>, A>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// An amount which specifies a total or per-unit, or simply just a currency.
pub enum ScopedAmount {
    BareCurrency(Symbol<Currency>),
    BareAmount(ScopedValue),
    CurrencyAmount(ScopedValue, Symbol<Currency>),
}

#[derive(Clone, Debug)]
pub struct CostSpec<A> {
    per_unit: Option<Augmented<Decimal, A>>,
    total: Option<Augmented<Decimal, A>>,
    currency: Option<Augmented<Symbol<Currency>, A>>,
    date: Option<Augmented<Date, A>>,
    label: Option<Augmented<Symbol<Label>, A>>,
    merge: bool,
}
