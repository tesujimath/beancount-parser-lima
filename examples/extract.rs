use anyhow::Result;
use rust_decimal::Decimal;
use std::fmt::{self, Display, Formatter};
use std::iter::empty;
use std::path::PathBuf;
use std::{io, iter::once};
use time::Date;

use beancount_parser::{
    Account, AccountType, Amount, BeancountParser, BeancountSources, CostSpec, Currency, Directive,
    DirectiveVariant, ExprValue, Flag, Link, MetaValue, Metadata, Posting, ScopedAmount,
    ScopedExprValue, SimpleValue, Tag, Transaction,
};

/// This example is really a test that there is sufficient public access to parser output types.
/// We need to avoid leaning on the Display implementations to be sure we can extract a usable value in every case.
fn main() -> Result<()> {
    let flags = xflags::parse_or_exit! {
        /// File to parse
        required path: PathBuf
    };

    let error_w = &io::stderr();
    let sources = BeancountSources::new(flags.path);
    let beancount_parser = BeancountParser::new(&sources);

    match beancount_parser.parse() {
        Ok(directives) => {
            for d in directives {
                use DirectiveVariant::*;

                match d.variant() {
                    Transaction(t) => {
                        for p in transaction(t, &d) {
                            print!("{}", p);
                        }
                    }
                    _ => {
                        println!("{}", &d);
                    }
                }
                println!();
            }

            Ok(())
        }
        Err(errors) => sources.write_errors(error_w, errors).map_err(|e| e.into()),
    }
}

// we turn the whole output into a sequence of primitives
#[derive(Clone, Debug)]
enum Primitive<'a> {
    Str(&'a str, Decoration),
    AccountType(AccountType),
    Flag(Flag),
    Decimal(Decimal),
    Date(Date),
    Bool(bool),
}

#[derive(Clone, Debug)]
enum Decoration {
    None,
    DoubleQuote,
    ColonPrefix,
    ColonSuffix,
    HashPrefix,
    CaretPrefix,
}

impl<'a> Display for Primitive<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Primitive::*;

        match self {
            Str(x, d) => {
                use Decoration::*;
                match d {
                    None => write!(f, "{}", x),
                    DoubleQuote => write!(f, "\"{}\"", x),
                    ColonPrefix => write!(f, ":{}", x),
                    ColonSuffix => write!(f, "{}:", x),
                    HashPrefix => write!(f, "#{}", x),
                    CaretPrefix => write!(f, "^{}", x),
                }
            }

            AccountType(x) => write!(f, "{}", x),
            Flag(x) => {
                if let beancount_parser::Flag::Letter(flag_letter) = x {
                    let c = flag_letter.char();
                    write!(f, "'{}", c)
                } else {
                    write!(f, "{}", x)
                }
            }
            Decimal(x) => write!(f, "{}", x),
            Date(x) => write!(f, "{}", x),
            Bool(x) => write!(f, "{}", x),
        }
    }
}

fn transaction<'a>(t: &'a Transaction, d: &'a Directive) -> impl Iterator<Item = Primitive<'a>> {
    let m = d.metadata();

    itertools::intersperse(
        date(d.date())
            .chain(flag(t.flag()))
            .chain(
                t.payee()
                    .into_iter()
                    .flat_map(|x| string(x.item(), Decoration::DoubleQuote)),
            )
            .chain(
                t.narration()
                    .into_iter()
                    .flat_map(|x| string(x.item(), Decoration::DoubleQuote)),
            )
            .chain(tags_links_inline(m)),
        SPACE,
    )
    .chain(keys_values(m))
    .chain(newline())
    .chain(t.postings().flat_map(|x| posting(x.item())))
}

fn posting<'a>(x: &'a Posting) -> impl Iterator<Item = Primitive<'a>> {
    indent()
        .chain(itertools::intersperse(
            x.flag()
                .into_iter()
                .flat_map(|x| flag(x.item()))
                .chain(account(x.account()))
                // TODO
                .chain(x.amount().into_iter().flat_map(|x| expr_value(x)))
                .chain(x.currency().into_iter().flat_map(|x| currency(x)))
                .chain(x.cost_spec().into_iter().flat_map(|x| cost_spec(x)))
                .chain(
                    x.price_annotation()
                        .into_iter()
                        .flat_map(|x| scoped_amount(x)),
                ),
            SPACE,
        ))
        // pub(crate) amount: Option<Spanned<ExprValue>>,
        // pub(crate) currency: Option<Spanned<&'a Currency<'a>>>,
        // pub(crate) cost_spec: Option<Spanned<CostSpec<'a>>>,
        // pub(crate) price_annotation: Option<Spanned<ScopedAmount<'a>>>,
        .chain(newline())
    // pub(crate) metadata: Metadata<'a>,
}

fn tags_links_inline<'a>(x: &'a Metadata) -> impl Iterator<Item = Primitive<'a>> {
    x.tags()
        .flat_map(|x| tag(x.item()))
        .chain(x.links().flat_map(|x| link(x.item())))
}

fn keys_values<'a>(x: &'a Metadata) -> impl Iterator<Item = Primitive<'a>> {
    x.key_values().flat_map(|(k, v)| {
        newline()
            .chain(indent())
            .chain(string_as_ref(k.item(), Decoration::ColonSuffix))
            .chain(meta_value(v))
    })
}

fn meta_value<'a>(x: &'a MetaValue) -> impl Iterator<Item = Primitive<'a>> {
    use MetaValue::*;

    match x {
        Simple(x) => v(simple_value(x)),
        Amount(x) => v(amount(x)),
    }
    .into_iter()
}

fn amount<'a>(x: &'a Amount) -> impl Iterator<Item = Primitive<'a>> {
    decimal(x.number().value()).chain(currency(x.currency()))
}

fn simple_value<'a>(x: &'a SimpleValue) -> impl Iterator<Item = Primitive<'a>> {
    use SimpleValue::*;

    match x {
        String(x) => v(string(x, Decoration::DoubleQuote)),
        Currency(x) => v(currency(x)),
        Account(x) => v(account(x)),
        Tag(x) => v(tag(x)),
        Link(x) => v(link(x)),
        Date(x) => v(date(*x)),
        Bool(x) => v(bool(*x)),
        None => v(empty()),
        Expr(x) => v(expr_value(x)),
    }
    .into_iter()
}

fn currency<'a>(x: &'a Currency) -> impl Iterator<Item = Primitive<'a>> {
    string_as_ref(x, Decoration::None)
}

fn account<'a>(x: &'a Account) -> impl Iterator<Item = Primitive<'a>> {
    account_type(x.account_type()).chain(
        x.names()
            .flat_map(|x| string_as_ref(x, Decoration::ColonPrefix)),
    )
}

fn tag<'a>(x: &'a Tag) -> impl Iterator<Item = Primitive<'a>> {
    string_as_ref(x, Decoration::HashPrefix)
}

fn link<'a>(x: &'a Link) -> impl Iterator<Item = Primitive<'a>> {
    string_as_ref(x, Decoration::CaretPrefix)
}

fn cost_spec<'a>(x: &'a CostSpec<'a>) -> impl Iterator<Item = Primitive<'a>> {
    x.per_unit()
        .into_iter()
        .flat_map(|x| expr_value(x))
        .chain(x.total().into_iter().flat_map(|x| expr_value(x)))
        .chain(x.currency().into_iter().flat_map(|x| currency(x)))
        .chain(x.date().into_iter().flat_map(|x| date(*x.item())))
        .chain(
            x.label()
                .into_iter()
                .flat_map(|x| string(x, Decoration::DoubleQuote)),
        )
        .chain(bool(x.merge()))
}

fn scoped_amount<'a>(x: &'a ScopedAmount<'a>) -> impl Iterator<Item = Primitive<'a>> {
    use ScopedAmount::*;

    match x {
        BareCurrency(c) => v(currency(c)),
        BareAmount(x) => v(scoped_expr_value(x)),
        CurrencyAmount(x, c) => v(scoped_expr_value(x).chain(currency(c))),
    }
    .into_iter()
}

fn scoped_expr_value(x: &ScopedExprValue) -> impl Iterator<Item = Primitive<'_>> {
    use ScopedExprValue::*;

    match x {
        PerUnit(x) => v(expr_value(x)),
        Total(x) => v(expr_value(x)),
    }
    .into_iter()
}

fn expr_value(x: &ExprValue) -> impl Iterator<Item = Primitive<'_>> {
    once(Primitive::Decimal(x.value()))
}

fn account_type<'a>(x: AccountType) -> impl Iterator<Item = Primitive<'a>> {
    once(Primitive::AccountType(x))
}

fn flag<'a>(x: &Flag) -> impl Iterator<Item = Primitive<'a>> {
    once(Primitive::Flag(*x))
}

fn decimal<'a>(x: Decimal) -> impl Iterator<Item = Primitive<'a>> {
    once(Primitive::Decimal(x))
}

fn date<'a>(x: Date) -> impl Iterator<Item = Primitive<'a>> {
    once(Primitive::Date(x))
}

fn bool<'a>(x: bool) -> impl Iterator<Item = Primitive<'a>> {
    once(Primitive::Bool(x))
}

fn string_as_ref<S>(x: &S, d: Decoration) -> impl Iterator<Item = Primitive<'_>>
where
    S: AsRef<str>,
{
    string(x.as_ref(), d)
}

fn string(x: &str, d: Decoration) -> impl Iterator<Item = Primitive<'_>> {
    once(Primitive::Str(x, d))
}

fn newline<'a>() -> impl Iterator<Item = Primitive<'a>> {
    string("\n", Decoration::None)
}

fn indent<'a>() -> impl Iterator<Item = Primitive<'a>> {
    string("  ", Decoration::None)
}

const SPACE: Primitive = Primitive::Str(" ", Decoration::None);

fn v<I, T>(iter: I) -> Vec<T>
where
    I: Iterator<Item = T>,
{
    iter.collect::<Vec<T>>()
}
