use super::{types::*, StringInterner};
use crate::types::*;
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
    booking: Option<Spanned<Booking>>,
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

    pub fn currencies<C, S>(mut self, symbol_table: &mut impl SymbolTable, currencies: C) -> Self
    where
        C: IntoIterator<Item = Spanned<S>>,
        S: AsRef<str>,
    {
        for currency in currencies {
            // TODO check duplicates
            self.currencies
                .insert(currency.map(|cur| symbol_table.get_or_intern(cur)));
        }

        self
    }

    pub fn booking(mut self, booking: Option<Spanned<Booking>>) -> Self {
        if let Some(booking) = booking {
            // TODO check duplicates
            self.booking = Some(booking);
        }

        self
    }

    pub fn tags<C, S>(mut self, symbol_table: &mut impl SymbolTable, tags: C) -> Self
    where
        C: IntoIterator<Item = Spanned<S>>,
        S: AsRef<str>,
    {
        for tag in tags {
            // TODO check duplicates
            self.tags
                .insert(tag.map(|tag| symbol_table.get_or_intern(tag)));
        }

        self
    }

    pub fn links<C, S>(mut self, symbol_table: &mut impl SymbolTable, links: C) -> Self
    where
        C: IntoIterator<Item = Spanned<S>>,
        S: AsRef<str>,
    {
        for link in links {
            // TODO check duplicates
            self.links
                .insert(link.map(|link| symbol_table.get_or_intern(link)));
        }

        self
    }

    pub fn merge(&mut self, _other: Self) {
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
    pub fn tags<C, S>(mut self, symbol_table: &mut StringInterner, tags: C) -> Self
    where
        C: IntoIterator<Item = Spanned<S>>,
        S: AsRef<str>,
    {
        for tag in tags {
            // TODO check duplicates
            self.tags
                .insert(tag.map(|tag| symbol_table.get_or_intern(tag)));
        }

        self
    }

    pub fn links<C, S>(mut self, symbol_table: &mut StringInterner, links: C) -> Self
    where
        C: IntoIterator<Item = Spanned<S>>,
        S: AsRef<str>,
    {
        for link in links {
            // TODO check duplicates
            self.links
                .insert(link.map(|link| symbol_table.get_or_intern(link)));
        }

        self
    }

    pub fn key_values<'a, C, K>(
        mut self,
        symbol_table: &mut impl SymbolTable,
        keys_values: C,
    ) -> Self
    where
        C: IntoIterator<Item = (Spanned<K>, Spanned<crate::parser::types::MetaValue<'a>>)>,
        K: AsRef<str>,
    {
        for (k, v) in keys_values {
            // TODO check duplicates
            self.key_values.insert(
                k.map(|k| symbol_table.get_or_intern(k)),
                v.map(|v| MetaValue::symbol_from(v, symbol_table)),
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
