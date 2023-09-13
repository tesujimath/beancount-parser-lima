// TODO remove suppression for dead code warning
#![allow(dead_code)]

use string_interner::StringInterner;

#[derive(Default, Debug)]
pub struct BeancountStore {
    string_interner: StringInterner,
}

impl BeancountStore {
    pub fn intern<S, T>(&mut self, s: S) -> Symbol<T>
    where
        S: AsRef<str>,
    {
        Symbol::new(self.string_interner.get_or_intern(s))
    }
}

mod types;
pub use types::*;
