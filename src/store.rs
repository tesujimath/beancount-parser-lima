// TODO remove suppression for dead code warning
#![allow(dead_code)]

#[derive(Default, Debug)]
pub struct BeancountStore {
    string_interner: StringInterner,
}

#[derive(Default, Debug)]
pub struct StringInterner(string_interner::StringInterner);

impl SymbolTable for StringInterner {
    fn get_or_intern<S, T>(&mut self, s: S) -> Symbol<T>
    where
        S: AsRef<str>,
    {
        Symbol::new(self.0.get_or_intern(s))
    }
}

pub mod builder;
pub mod types;
pub use types::*;
