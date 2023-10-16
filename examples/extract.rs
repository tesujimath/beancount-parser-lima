use anyhow::Result;
use rust_decimal::Decimal;
use std::fmt::{self, Display, Formatter};
use std::iter::{empty, Once};
use std::path::PathBuf;
use std::{io, iter::once};
use time::Date;

use beancount_parser::{
    Account, AccountType, Amount, BeancountParser, BeancountSources, Booking, Close, Commodity,
    CostSpec, Currency, Directive, DirectiveVariant, ExprValue, Flag, Key, Link, MetaValue,
    Metadata, Open, Posting, Price, ScopedAmount, ScopedExprValue, SimpleValue, Tag, Transaction,
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

                for p in match d.variant() {
                    Transaction(x) => transaction(x, &d),

                    Price(x) => price(x, &d),

                    Open(x) => open(x, &d),

                    Close(x) => close(x, &d),

                    Commodity(x) => commodity(x, &d),
                } {
                    print!("{}", p);
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
    Booking(Booking),
    Bool(bool),
    Spliced(Vec<Primitive<'a>>),
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

            Booking(x) => write!(f, "\"{}\"", x),

            Bool(x) => write!(f, "{}", x),

            Spliced(xs) => {
                for x in xs {
                    write!(f, "{}", x)?;
                }

                Ok(())
            }
        }
    }
}

fn transaction<'a>(
    x: &'a Transaction,
    d: &'a Directive,
) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(flag(x.flag()))
            .chain(
                x.payee()
                    .into_iter()
                    .flat_map(|x| string(x, Decoration::DoubleQuote)),
            )
            .chain(
                x.narration()
                    .into_iter()
                    .flat_map(|x| string(x, Decoration::DoubleQuote)),
            )
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline())
            .chain(x.postings().flat_map(|x| posting(x))),
    )
}

fn price<'a>(x: &'a Price, d: &'a Directive) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(once(Primitive::Str("price", Decoration::None)))
            .chain(currency(x.currency()))
            .chain(amount(x.amount()))
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline()),
    )
}

fn open<'a>(x: &'a Open, d: &'a Directive) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(once(Primitive::Str("open", Decoration::None)))
            .chain(account(x.account()))
            .chain(x.currencies().flat_map(|x| currency(x)))
            .chain(x.booking().into_iter().flat_map(|x| booking(x)))
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline()),
    )
}

fn close<'a>(x: &'a Close, d: &'a Directive) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(once(Primitive::Str("close", Decoration::None)))
            .chain(account(x.account()))
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline()),
    )
}

fn commodity<'a>(
    x: &'a Commodity,
    d: &'a Directive,
) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(once(Primitive::Str("commodity", Decoration::None)))
            .chain(currency(x.currency()))
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline()),
    )
}

fn posting<'a>(x: &'a Posting) -> impl Iterator<Item = Primitive<'a>> {
    let m = x.metadata();

    indent()
        .chain(
            x.flag()
                .into_iter()
                .flat_map(|x| flag(x))
                .chain(account(x.account()))
                // TODO
                .chain(x.amount().into_iter().flat_map(|x| expr_value(x)))
                .chain(x.currency().into_iter().flat_map(|x| currency(x)))
                .chain(x.cost_spec().into_iter().flat_map(|x| cost_spec(x)))
                .chain(
                    x.price_annotation()
                        .into_iter()
                        .flat_map(|x| scoped_amount(x)),
                )
                .spaced(),
        )
        // pub(crate) amount: Option<Spanned<ExprValue>>,
        // pub(crate) currency: Option<Spanned<&'a Currency<'a>>>,
        // pub(crate) cost_spec: Option<Spanned<CostSpec<'a>>>,
        // pub(crate) price_annotation: Option<Spanned<ScopedAmount<'a>>>,
        .chain(keys_values(m))
        .chain(newline())
    // pub(crate) metadata: Metadata<'a>,
}

fn tags_links_inline<'a>(x: &'a Metadata) -> impl Iterator<Item = Primitive<'a>> {
    x.tags()
        .flat_map(|x| tag(x))
        .chain(x.links().flat_map(|x| link(x)))
}

fn keys_values<'a>(x: &'a Metadata) -> impl Iterator<Item = Primitive<'a>> {
    x.key_values().flat_map(|(k, v)| {
        newline()
            .chain(indent())
            .chain(key(k).chain(meta_value(v)).spaced())
    })
}

fn meta_value<'a>(x: &'a MetaValue) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    use MetaValue::*;

    match x {
        Simple(x) => Box::new(simple_value(x)),
        Amount(x) => Box::new(amount(x)),
    }
}

fn amount<'a>(x: &'a Amount) -> impl Iterator<Item = Primitive<'a>> {
    decimal(x.number().value()).chain(currency(x.currency()))
}

fn simple_value<'a>(x: &'a SimpleValue) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    use SimpleValue::*;

    match x {
        String(x) => Box::new(string(x, Decoration::DoubleQuote)),
        Currency(x) => Box::new(currency(x)),
        Account(x) => Box::new(account(x)),
        Tag(x) => Box::new(tag(x)),
        Link(x) => Box::new(link(x)),
        Date(x) => Box::new(date(x)),
        Bool(x) => Box::new(bool(*x)),
        None => Box::new(empty()),
        Expr(x) => Box::new(expr_value(x)),
    }
}

fn currency<'a>(x: &'a Currency) -> impl Iterator<Item = Primitive<'a>> {
    string_as_ref(x, Decoration::None)
}

fn account<'a>(x: &'a Account) -> impl Iterator<Item = Primitive<'a>> {
    account_type(x.account_type())
        .chain(
            x.names()
                .flat_map(|x| string_as_ref(x, Decoration::ColonPrefix)),
        )
        .spliced()
}

fn tag<'a>(x: &'a Tag) -> impl Iterator<Item = Primitive<'a>> {
    string_as_ref(x, Decoration::HashPrefix)
}

fn link<'a>(x: &'a Link) -> impl Iterator<Item = Primitive<'a>> {
    string_as_ref(x, Decoration::CaretPrefix)
}

fn key<'a>(x: &'a Key) -> impl Iterator<Item = Primitive<'a>> {
    string_as_ref(x, Decoration::ColonSuffix)
}

fn cost_spec<'a>(x: &'a CostSpec<'a>) -> impl Iterator<Item = Primitive<'a>> {
    x.per_unit()
        .into_iter()
        .flat_map(|x| expr_value(x))
        .chain(x.total().into_iter().flat_map(|x| expr_value(x)))
        .chain(x.currency().into_iter().flat_map(|x| currency(x)))
        .chain(x.date().into_iter().flat_map(|x| date(x)))
        .chain(
            x.label()
                .into_iter()
                .flat_map(|x| string(x, Decoration::DoubleQuote)),
        )
        .chain(bool(x.merge()))
}

fn scoped_amount<'a>(x: &'a ScopedAmount<'a>) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    use ScopedAmount::*;

    match x {
        BareCurrency(c) => Box::new(currency(c)),
        BareAmount(x) => Box::new(scoped_expr_value(x)),
        CurrencyAmount(x, c) => Box::new(scoped_expr_value(x).chain(currency(c))),
    }
}

fn scoped_expr_value(x: &ScopedExprValue) -> impl Iterator<Item = Primitive<'_>> {
    use ScopedExprValue::*;

    match x {
        PerUnit(x) => Box::new(expr_value(x)),
        Total(x) => Box::new(expr_value(x)),
    }
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

fn date<'a>(x: &Date) -> impl Iterator<Item = Primitive<'a>> {
    once(Primitive::Date(*x))
}

fn booking<'a>(x: &Booking) -> impl Iterator<Item = Primitive<'a>> {
    once(Primitive::Booking(*x))
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

fn newline<'a>() -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    Box::new(string("\n", Decoration::None))
}

fn indent<'a>() -> impl Iterator<Item = Primitive<'a>> {
    string("  ", Decoration::None)
}

const SPACE: Primitive = Primitive::Str(" ", Decoration::None);

trait SpacedIteratorAdaptor<'a>: Iterator<Item = Primitive<'a>> + Sized {
    /// Iterator adapter for spacing primtives
    fn spaced(self) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a>
    where
        Self: 'a,
    {
        Box::new(itertools::intersperse(self, SPACE))
    }
}

impl<'a, I: Iterator<Item = Primitive<'a>>> SpacedIteratorAdaptor<'a> for I {}

trait SplicedIteratorAdaptor<'a>: Iterator<Item = Primitive<'a>> + Sized {
    /// Iterator adapter for splicing primtives
    fn spliced(self) -> Once<Primitive<'a>>
    where
        Self: 'a,
    {
        once(Primitive::Spliced(self.collect()))
    }
}

impl<'a, I: Iterator<Item = Primitive<'a>>> SplicedIteratorAdaptor<'a> for I {}
