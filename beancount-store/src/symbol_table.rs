use std::fmt::Display;

use log::info;
use string_interner::{DefaultSymbol, StringInterner};

#[derive(Default, Debug)]
pub struct SymbolTable {
    string_interner: StringInterner,
    types_per_symbol: Vec<SymbolTypes>, // bitfield
}

pub type Symbol = DefaultSymbol;

pub struct Currency(Symbol);

/// SymTypes is a bitfield
type SymTypes = u16;

const CURRENCY_SYM: SymTypes     = 1 <<  0;
const ACCOUNT_NAME_SYM: SymTypes = 1 <<  1;

fn sym_type_names(sym_types: SymTypes) ->  TODO
impl Display for SymTypes
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut  written = false;
        let write = |s: &str| {
            if written {
                f.write("|")?;
            }
            f.write(s)
        }

        if self & CURRENCY_SYM {
            write(f, "CURRENCY")?;
        }

    Ok(())
    }
}

impl SymbolTable {
    pub fn get_or_create_symbol<S>(&mut self, s: S, symbol_type: SymbolTypes) -> Currency
    where
        S: AsRef<str>,
    {
        let sym = self.string_interner.get_or_intern(s);
        info!("")

        Currency(sym)
    }
}
