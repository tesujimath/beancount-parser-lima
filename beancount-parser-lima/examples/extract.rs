use rust_decimal::Decimal;
use std::borrow::Cow;
use std::io::Write;
use std::iter::{empty, Once};
use std::path::PathBuf;
use std::{io, iter::once};
use strum::IntoEnumIterator;
use time::Date;

use beancount_parser_lima::{
    Account, AccountType, Amount, AmountWithTolerance, Balance, BeancountParser, BeancountSources,
    Booking, Close, Commodity, CostSpec, Currency, Directive, DirectiveVariant, Document, Event,
    ExprValue, Flag, Key, Link, MetaValue, Metadata, Note, Open, Options, Pad, ParseError,
    ParseSuccess, Plugin, PluginProcessingMode, Posting, Price, Query, ScopedAmount,
    ScopedExprValue, SimpleValue, Subaccount, Tag, Transaction,
};

/// This example is really a test that there is sufficient public access to parser output types.
/// We need to avoid leaning on the Display implementations to be sure we can extract a usable value in every case.
/// This is why all values are mapped onto Primitive without recourse to Display.
fn main() {
    let flags = xflags::parse_or_exit! {
        /// File to parse
        required path: PathBuf
    };

    let stderr = &io::stderr();
    let sources = BeancountSources::from(flags.path);
    let parser = BeancountParser::new(&sources);

    parse(&sources, &parser, stderr);
}

fn parse<W>(sources: &BeancountSources, parser: &BeancountParser, error_w: W)
where
    W: Write + Copy,
{
    match parser.parse() {
        Ok(ParseSuccess {
            directives,
            options,
            plugins,
            warnings,
        }) => {
            for p in extract_options(&options)
                .chain(extract_plugins(&plugins))
                .chain(directives.iter().flat_map(|d| {
                    use DirectiveVariant::*;

                    match d.variant() {
                        Transaction(x) => transaction(x, d),

                        Price(x) => price(x, d),

                        Balance(x) => balance(x, d),

                        Open(x) => open(x, d),

                        Close(x) => close(x, d),

                        Commodity(x) => commodity(x, d),

                        Pad(x) => pad(x, d),

                        Document(x) => document(x, d),

                        Note(x) => note(x, d),

                        Event(x) => event(x, d),

                        Query(x) => query(x, d),
                    }
                }))
            {
                p.write(&io::stdout(), &options).unwrap();
            }
            println!();

            sources.write(error_w, warnings).unwrap();
        }
        Err(ParseError { errors, warnings }) => {
            sources.write(error_w, errors).unwrap();
            sources.write(error_w, warnings).unwrap();
        }
    }
}

// we turn the whole output into a sequence of primitives
// just to confirm that we actually can, using the crate public interface
#[derive(Clone, Debug)]
enum Primitive<'a> {
    Str(Cow<'a, str>, Decoration),
    AccountType(AccountType),
    Flag(Flag),
    Decimal(Decimal),
    Date(Date),
    Booking(Booking),
    PluginProcessingMode(PluginProcessingMode),
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

impl<'a> Primitive<'a> {
    fn write<W>(&self, mut w: W, options: &Options) -> io::Result<()>
    where
        W: io::Write + Copy,
    {
        use Primitive::*;

        match self {
            Str(x, d) => {
                use Decoration::*;
                match d {
                    None => write!(w, "{}", x),
                    DoubleQuote => write!(w, "\"{}\"", x),
                    ColonPrefix => write!(w, ":{}", x),
                    ColonSuffix => write!(w, "{}:", x),
                    HashPrefix => write!(w, "#{}", x),
                    CaretPrefix => write!(w, "^{}", x),
                }
            }

            AccountType(x) => write!(w, "{}", options.account_type_name(*x)),
            Flag(x) => {
                if let beancount_parser_lima::Flag::Letter(flag_letter) = x {
                    let c = flag_letter.char();
                    write!(w, "'{}", c)
                } else {
                    write!(w, "{}", x)
                }
            }
            Decimal(x) => write!(w, "{}", x),
            Date(x) => write!(w, "{}", x),

            Booking(x) => write!(w, "\"{}\"", x),

            PluginProcessingMode(x) => write!(w, "\"{}\"", x),

            Bool(x) => write!(w, "{}", x),

            Spliced(xs) => {
                for x in xs {
                    x.write(w, options)?;
                }

                Ok(())
            }
        }
    }
}

fn extract_options<'a>(options: &'a Options) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    Box::new(
        option("title", string(options.title(), Decoration::None))
            .chain(AccountType::iter().flat_map(|account_type| {
                option(
                    format!("name_{}", account_type.as_ref().to_lowercase()),
                    string_as_ref(options.account_type_name(account_type), Decoration::None),
                )
            }))
            .chain(option(
                "account_previous_balances",
                subaccount(options.account_previous_balances()),
            ))
            .chain(option(
                "account_previous_earnings",
                subaccount(options.account_previous_earnings()),
            ))
            .chain(option(
                "account_previous_conversions",
                subaccount(options.account_previous_conversions()),
            ))
            .chain(option(
                "account_current_earnings",
                subaccount(options.account_current_earnings()),
            ))
            .chain(option(
                "account_current_conversions",
                subaccount(options.account_current_conversions()),
            ))
            .chain(option(
                "account_unrealized_gains",
                subaccount(options.account_unrealized_gains()),
            ))
            .chain(
                options
                    .account_rounding()
                    .map(|account_rounding| {
                        option("account_rounding", subaccount(account_rounding))
                    })
                    .unwrap_or(Box::new(empty())),
            )
            .chain(option(
                "conversion_currency",
                currency(options.conversion_currency()),
            ))
            .chain(
                // some representative currencies, just to check we can pull out tolerances by currency
                ["NZD", "GBP", "EUR", "USD"]
                    .into_iter()
                    .flat_map(|currency_str| {
                        let currency = Currency::try_from(currency_str).unwrap();
                        options
                            .inferred_tolerance_default(&currency)
                            .map(|tolerance| {
                                option(
                                    format!("inferred_tolerance_default({})", currency_str),
                                    decimal(tolerance),
                                )
                            })
                            .unwrap_or(Box::new(empty()))
                    }),
            )
            .chain(option(
                "inferred_tolerance_multiplier",
                decimal(options.inferred_tolerance_multiplier()),
            ))
            .chain(option(
                "infer_tolerance_from_cost",
                bool(options.infer_tolerance_from_cost()),
            ))
            .chain(options.documents().flat_map(|documents| {
                option(
                    "documents",
                    cow_string(documents.to_string_lossy(), Decoration::None),
                )
            }))
            .chain(
                options
                    .operating_currency()
                    .flat_map(|x| option("operating_currency", currency(x))),
            )
            .chain(option("render_commas", bool(options.render_commas())))
            .chain(bare_option(
                "booking_method",
                booking(&options.booking_method()),
            ))
            .chain(bare_option(
                "plugin_processing_mode",
                plugin_processing_mode(&options.plugin_processing_mode()),
            )),
    )
}

fn option<'a, S>(
    name: S,
    value: impl Iterator<Item = Primitive<'a>> + 'a,
) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a>
where
    S: std::fmt::Display,
{
    Box::new(
        owned_string(format!("option \"{}\" \"", name), Decoration::None)
            .chain(value)
            .chain(string("\"", Decoration::None))
            .chain(newline()),
    )
}

fn bare_option<'a, S>(
    name: S,
    value: impl Iterator<Item = Primitive<'a>> + 'a,
) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a>
where
    S: std::fmt::Display,
{
    Box::new(
        owned_string(format!("option \"{}\" ", name), Decoration::None)
            .chain(value)
            .chain(newline()),
    )
}

fn extract_plugins<'a>(plugins: &'a [Plugin<'a>]) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    Box::new(plugins.iter().flat_map(|plugin| {
        string("plugin", Decoration::None)
            .chain(string(plugin.module_name(), Decoration::DoubleQuote))
            .chain(
                plugin
                    .config()
                    .into_iter()
                    .flat_map(|config| string(config.item(), Decoration::DoubleQuote)),
            )
            .spaced()
            .chain(newline())
    }))
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
            .chain(once(Primitive::Str(
                Cow::Borrowed("price"),
                Decoration::None,
            )))
            .chain(currency(x.currency()))
            .chain(amount(x.amount()))
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline()),
    )
}

fn balance<'a>(x: &'a Balance, d: &'a Directive) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(once(Primitive::Str(
                Cow::Borrowed("balance"),
                Decoration::None,
            )))
            .chain(account(x.account()))
            .chain(amount_with_tolerance(x.atol()))
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
            .chain(once(Primitive::Str(
                Cow::Borrowed("open"),
                Decoration::None,
            )))
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
            .chain(once(Primitive::Str(
                Cow::Borrowed("close"),
                Decoration::None,
            )))
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
            .chain(once(Primitive::Str(
                Cow::Borrowed("commodity"),
                Decoration::None,
            )))
            .chain(currency(x.currency()))
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline()),
    )
}

fn pad<'a>(x: &'a Pad, d: &'a Directive) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(once(Primitive::Str(Cow::Borrowed("pad"), Decoration::None)))
            .chain(account(x.account()))
            .chain(account(x.source()))
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline()),
    )
}

fn document<'a>(x: &'a Document, d: &'a Directive) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(once(Primitive::Str(
                Cow::Borrowed("document"),
                Decoration::None,
            )))
            .chain(account(x.account()))
            .chain(string(x.path(), Decoration::DoubleQuote))
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline()),
    )
}

fn note<'a>(x: &'a Note, d: &'a Directive) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(once(Primitive::Str(
                Cow::Borrowed("note"),
                Decoration::None,
            )))
            .chain(account(x.account()))
            .chain(string(x.comment(), Decoration::DoubleQuote))
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline()),
    )
}

fn event<'a>(x: &'a Event, d: &'a Directive) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(once(Primitive::Str(
                Cow::Borrowed("event"),
                Decoration::None,
            )))
            .chain(string(x.event_type(), Decoration::DoubleQuote))
            .chain(string(x.description(), Decoration::DoubleQuote))
            .chain(tags_links_inline(m))
            .spaced()
            .chain(keys_values(m))
            .chain(newline()),
    )
}

fn query<'a>(x: &'a Query, d: &'a Directive) -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    let m = d.metadata();

    Box::new(
        date(d.date())
            .chain(once(Primitive::Str(
                Cow::Borrowed("query"),
                Decoration::None,
            )))
            .chain(string(x.name(), Decoration::DoubleQuote))
            .chain(string(x.content(), Decoration::DoubleQuote))
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

fn amount_with_tolerance<'a>(x: &'a AmountWithTolerance) -> impl Iterator<Item = Primitive<'a>> {
    amount(x.amount()).chain(
        x.tolerance()
            .into_iter()
            .flat_map(|x| string("~", Decoration::None).chain(decimal(*x.item()))),
    )
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
                .iter()
                .flat_map(|x| string_as_ref(x, Decoration::ColonPrefix)),
        )
        .spliced()
}

fn subaccount<'a>(x: &'a Subaccount) -> impl Iterator<Item = Primitive<'a>> {
    let mut names = x.iter();
    let first_name = names.next().unwrap(); // is NonEmpty so can't fail
    string_as_ref(first_name, Decoration::None)
        .chain(names.flat_map(|x| string_as_ref(x, Decoration::ColonPrefix)))
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

fn plugin_processing_mode<'a>(x: &PluginProcessingMode) -> impl Iterator<Item = Primitive<'a>> {
    once(Primitive::PluginProcessingMode(*x))
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
    once(Primitive::Str(Cow::Borrowed(x), d))
}

fn owned_string<'a>(x: String, d: Decoration) -> impl Iterator<Item = Primitive<'a>> {
    once(Primitive::Str(Cow::Owned(x), d))
}

fn cow_string(x: Cow<str>, d: Decoration) -> impl Iterator<Item = Primitive> {
    once(Primitive::Str(x, d))
}

fn newline<'a>() -> Box<dyn Iterator<Item = Primitive<'a>> + 'a> {
    Box::new(string("\n", Decoration::None))
}

fn indent<'a>() -> impl Iterator<Item = Primitive<'a>> {
    string("  ", Decoration::None)
}

const SPACE: Primitive = Primitive::Str(Cow::Borrowed(" "), Decoration::None);

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
    /// Iterator adapter for splicing primitives
    fn spliced(self) -> Once<Primitive<'a>>
    where
        Self: 'a,
    {
        once(Primitive::Spliced(self.collect()))
    }
}

impl<'a, I: Iterator<Item = Primitive<'a>>> SplicedIteratorAdaptor<'a> for I {}
