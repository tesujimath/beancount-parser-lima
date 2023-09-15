use super::{types::*, SymbolTable};
use crate::{
    parsed_to_store::build_metavalue,
    types::{spanned, Spanned},
};
use std::{
    collections::{hash_map, HashMap, HashSet},
    fmt::{self, Debug, Display, Formatter},
};
use time::Date;

#[derive(Default, Debug)]
pub struct BeancountStoreBuilder {
    accounts: HashMap<Account, AccountBuilder>,
    // accumulate errors as we go
    errors: BuilderErrors,
}

impl BeancountStoreBuilder {
    // pass in SourceId to all these?  How to store that?  Consider open/close in different source files.
    // need to think carefully how augmentation works, and error reporting
    pub fn open(&mut self, account: Account, builder: AccountBuilder) {
        use hash_map::Entry::*;

        match self.accounts.entry(account) {
            Occupied(ref mut entry) => {
                entry.get_mut().merge(builder);
            }
            Vacant(entry) => {
                entry.insert(builder);
            }
        }
    }

    pub fn close(&mut self, account: Account, builder: AccountBuilder) {
        use hash_map::Entry::*;

        match self.accounts.entry(account) {
            Occupied(ref mut entry) => {
                entry.get_mut().merge(builder);
            }
            Vacant(entry) => {
                entry.insert(builder);
            }
        }
    }

    // TODO pub fn build(&self) -> Vec<Error> {}
}

#[derive(Default, Debug)]
pub struct AccountBuilder {
    opened: Option<Spanned<Date>>,
    closed: Option<Spanned<Date>>,
    currencies: HashSet<Spanned<Symbol<Currency>>>,
    booking: Option<Spanned<String>>,
    tags: HashSet<Spanned<Symbol<Tag>>>,
    links: HashSet<Spanned<Symbol<Link>>>,
    metadata: MetadataBuilder,
    errors: BuilderErrors,
}

impl AccountBuilder {
    pub fn open(date: Spanned<Date>) -> AccountBuilder {
        AccountBuilder {
            opened: Some(date),
            ..AccountBuilder::default()
        }
    }

    pub fn close(date: Spanned<Date>) -> AccountBuilder {
        AccountBuilder {
            closed: Some(date),
            ..AccountBuilder::default()
        }
    }

    pub fn currencies<C, S>(mut self, symbol_table: &mut SymbolTable, currencies: C) -> Self
    where
        C: IntoIterator<Item = Spanned<S>>,
        S: AsRef<str>,
    {
        for currency in currencies {
            let sym = symbol_table.intern(currency.value);
            // TODO check duplicates
            self.currencies.insert(spanned(sym, currency.span));
        }

        self
    }

    pub fn booking<S>(mut self, booking: Option<Spanned<S>>) -> Self
    where
        S: ToString,
    {
        if let Some(booking) = booking {
            // TODO check duplicates
            self.booking = Some(spanned(booking.value.to_string(), booking.span));
        }

        self
    }

    pub fn tags<C, S>(mut self, symbol_table: &mut SymbolTable, tags: C) -> Self
    where
        C: IntoIterator<Item = Spanned<S>>,
        S: AsRef<str>,
    {
        for tag in tags {
            let sym = symbol_table.intern(tag.value);
            // TODO check duplicates
            self.tags.insert(spanned(sym, tag.span));
        }

        self
    }

    pub fn links<C, S>(mut self, symbol_table: &mut SymbolTable, links: C) -> Self
    where
        C: IntoIterator<Item = Spanned<S>>,
        S: AsRef<str>,
    {
        for link in links {
            let sym = symbol_table.intern(link.value);
            // TODO check duplicates
            self.links.insert(spanned(sym, link.span));
        }

        self
    }

    pub fn merge(&mut self, other: Self) {
        // TODO actually merge the builders, accumulating errors in that case
    }
}

#[derive(Default, Debug)]
pub struct MetadataBuilder {
    tags: HashSet<Spanned<Symbol<Tag>>>,
    links: HashSet<Spanned<Symbol<Link>>>,
    key_values: HashMap<Spanned<Symbol<Key>>, Spanned<MetaValue>>,
}

impl MetadataBuilder {
    pub fn tags<C, S>(mut self, symbol_table: &mut SymbolTable, tags: C) -> Self
    where
        C: IntoIterator<Item = Spanned<S>>,
        S: AsRef<str>,
    {
        for tag in tags {
            let sym = symbol_table.intern(tag.value);
            // TODO check duplicates
            self.tags.insert(spanned(sym, tag.span));
        }

        self
    }

    pub fn links<C, S>(mut self, symbol_table: &mut SymbolTable, links: C) -> Self
    where
        C: IntoIterator<Item = Spanned<S>>,
        S: AsRef<str>,
    {
        for link in links {
            let sym = symbol_table.intern(link.value);
            // TODO check duplicates
            self.links.insert(spanned(sym, link.span));
        }

        self
    }

    pub fn key_values<'a, C, K>(mut self, symbol_table: &mut SymbolTable, keys_values: C) -> Self
    where
        // TODO hide the metavalue type a bit better
        C: IntoIterator<Item = (Spanned<K>, Spanned<crate::parser::types::MetaValue<'a>>)>,
        K: AsRef<str>,
    {
        for (k, v) in keys_values {
            let sym = symbol_table.intern(k.value);
            // TODO check duplicates
            // TODO insert actual value
            self.key_values.insert(
                spanned(sym, k.span),
                spanned(build_metavalue(v.value, symbol_table), v.span),
            );
        }

        self
    }
}

// TODO BuilderErrors type
#[derive(PartialEq, Eq, Default, Debug)]
pub struct BuilderErrors(Vec<u8>);

impl Display for BuilderErrors {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} builder errors ", self.0.len())
    }
}

impl std::error::Error for BuilderErrors {}

pub trait StoreBuilder {
    type Output;

    fn build(self, symbol_table: &mut SymbolTable) -> Self::Output;
}
