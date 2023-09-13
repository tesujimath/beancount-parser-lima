use std::collections::HashSet;

use beancount_types::{AccountType, Flag};
use nonempty::NonEmpty;
use rust_decimal::Decimal;
use time::Date;

pub type Symbol = string_interner::DefaultSymbol;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Augmented<T, A> {
    item: T,
    augmentation: A,
}

#[derive(Clone, Debug)]
pub struct Transaction<A> {
    pub(crate) date: Augmented<Date, A>,
    pub(crate) flag: Augmented<Flag, A>,
    pub(crate) payee: Option<Augmented<Symbol, A>>,
    pub(crate) narration: Option<Augmented<String, A>>,
    pub(crate) tags: HashSet<Augmented<Symbol, A>>,
    pub(crate) links: HashSet<Augmented<Symbol, A>>,
    pub(crate) metadata: Metadata<A>,
    pub(crate) postings: Vec<Augmented<Posting<A>, A>>,
}

#[derive(Clone, Debug)]
pub struct Posting<A> {
    pub flag: Option<Augmented<Flag, A>>,
    pub account: Augmented<Account, A>,
    pub amount: Option<Augmented<Decimal, A>>,
    pub currency: Option<Augmented<Symbol, A>>,
    // TODO pub cost_spec: Option<Augmented<CostSpec, A<A>>>,
    // TODO pub price_annotation: Option<Augmented<ScopedAmount, A<A>>>,
    pub metadata: Metadata<A>,
}

#[derive(Clone, Default, Debug)]
pub struct Metadata<A> {
    // TODO pub key_values: Vec<Spanned<MetaKeyValue<'a>>>,
    pub tags: HashSet<Augmented<Symbol, A>>,
    pub links: HashSet<Augmented<Symbol, A>>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Account {
    pub(crate) account_type: AccountType,
    pub(crate) names: NonEmpty<Symbol>,
}
