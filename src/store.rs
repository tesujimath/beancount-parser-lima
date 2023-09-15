// TODO remove suppression for dead code warning
#![allow(dead_code)]

use string_interner::StringInterner;

#[derive(Default, Debug)]
pub struct BeancountStore {
    symbol_table: SymbolTable,
}

#[derive(Default, Debug)]
pub struct SymbolTable(StringInterner);

impl SymbolTable {
    pub fn intern<S, T>(&mut self, s: S) -> Symbol<T>
    where
        S: AsRef<str>,
    {
        Symbol::new(self.0.get_or_intern(s))
    }
}

pub mod builder;
pub mod types;
pub use types::*;
