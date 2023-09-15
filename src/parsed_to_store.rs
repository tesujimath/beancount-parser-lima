use super::parser::{self, types::*};
use super::store::{self, builder::*, *};

impl<'a> StoreBuilder for Open<'a> {
    type Output = AccountBuilder;

    fn build(self, symbol_table: &mut SymbolTable) -> AccountBuilder {
        AccountBuilder::open(self.date)
            .currencies(symbol_table, self.currencies)
            .booking(self.booking)
            .tags(symbol_table, self.tags)
            .links(symbol_table, self.links)
    }
}

pub fn build_metavalue<'a>(
    v: parser::types::MetaValue<'a>,
    symbol_table: &mut SymbolTable,
) -> store::types::MetaValue {
    use parser::types::MetaValue as PMV;
    use parser::types::SimpleValue as PSV;
    use store::types::MetaValue as SMV;
    use store::types::SimpleValue as SSV;

    let dummy_value = SMV::Simple(SSV::None);
    // TODO actually convert the value
    match v {
        PMV::Simple(sv) => dummy_value,
        /*SMV::Simple(
        SSV::None, // TODO
                       match sv {
                       PSV::String(s) => SSV::String(s.to_string()),
                   }*/
        PMV::Amount(amount) => dummy_value,
        /*SMV::Amount(amount),
        }
            String(&'a str),
            Currency(&'a Currency<'a>),
            Account(&'a Account<'a>),
            Tag(&'a Tag<'a>),
            Link(&'a Link<'a>),
            Date(Date),
            Bool(bool),
            None,
            Expr(Expr),
        */
    }
}
