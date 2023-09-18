use std::{
    collections::{hash_map, HashSet},
    ops::Deref,
};

use super::lexer::Token;
use super::types::*;
use crate::types::*;
use chumsky::{
    input::{BorrowInput, ValueInput},
    prelude::*,
};
use either::Either;
use time::Date;

/// Matches all the includes in the file, ignoring everything else.
pub fn includes<'src, I>() -> impl Parser<'src, I, Vec<String>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let string = select_ref!(Token::StringLiteral(s) => s.deref());

    (just(Token::Include).ignore_then(string).map(Some))
        .or(any().map(|_| None))
        .repeated()
        .collect::<Vec<_>>()
        .map(|includes| {
            includes
                .into_iter()
                .filter_map(|s| s.as_ref().map(|s| s.to_string()))
                .collect::<Vec<_>>()
        })
}

/// Matches the whole file.
pub fn file<'src, I>(
) -> impl Parser<'src, I, Vec<Spanned<Declaration<'src>>>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    declaration().repeated().collect::<Vec<_>>()
}

/// Matches a `Declaration`, and returns with Span.
pub fn declaration<'src, I>(
) -> impl Parser<'src, I, Spanned<Declaration<'src>>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use Declaration::*;

    choice((directive().map(Directive), pragma().map(Pragma)))
        .map_with_span(spanned)
        .recover_with(skip_then_retry_until(any().ignored(), end()))
}

/// Matches a `Directive`.
pub fn directive<'src, I>() -> impl Parser<'src, I, Directive<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use Directive::*;

    choice((
        transaction()
            .map(Transaction)
            .labelled("transaction")
            .as_context(),
        choice((
            open().map(Open),
            close().map(Close),
            commodity().map(Commodity),
            // TODO other directives
        ))
        .labelled("directive")
        .as_context(),
    ))
}

/// Matches a `Pragma`.
pub fn pragma<'src, I>() -> impl Parser<'src, I, Pragma<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use Pragma::*;

    let string = select_ref!(Token::StringLiteral(s) => s.deref());

    choice((just(Token::Include).ignore_then(string).map(Include),))
        .then_ignore(just(Token::Eol))
        .labelled("pragma")
        .as_context()
}

/// Matches a transaction, including metadata and postings, over several lines.
pub fn transaction<'src, I>(
) -> impl Parser<'src, I, Transaction<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    group((
        transaction_header_line(),
        metadata(),
        posting()
            .map_with_span(spanned)
            .repeated()
            .collect::<Vec<_>>(),
    ))
    .map(
        |((date, flag, s1, s2, (tags, links)), metadata, postings)| match (s1, s2) {
            // a single string is narration
            (Some(s1), None) => Transaction {
                date,
                flag,
                payee: None,
                narration: Some(s1),
                tags,
                links,
                metadata,
                postings,
            },
            (s1, s2) => Transaction {
                date,
                flag,
                payee: s1,
                narration: s2,
                tags,
                links,
                metadata,
                postings,
            },
        },
    )
}

/// Matches the first line of a transaction.
fn transaction_header_line<'src, I>() -> impl Parser<
    'src,
    I,
    (
        Spanned<Date>,
        Spanned<Flag>,
        Option<Spanned<&'src str>>,
        Option<Spanned<&'src str>>,
        (
            HashSet<Spanned<&'src Tag<'src>>>,
            HashSet<Spanned<&'src Link<'src>>>,
        ),
    ),
    extra::Err<ParserError<'src>>,
>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let date = select_ref!(Token::Date(date) => *date);
    let string = select_ref!(Token::StringLiteral(s) => s.deref());

    group((
        date.map_with_span(spanned),
        txn().map_with_span(spanned),
        string.map_with_span(spanned).or_not(),
        string.map_with_span(spanned).or_not(),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
}

/// Matches a open, including metadata, over several lines.
pub fn open<'src, I>() -> impl Parser<'src, I, Open<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    group((open_header_line(), metadata())).map(
        |((date, account, currencies, booking, (tags, links)), metadata)| Open {
            date,
            account,
            currencies,
            booking,
            tags,
            links,
            metadata,
        },
    )
}

/// Matches the first line of a open.
fn open_header_line<'src, I>() -> impl Parser<
    'src,
    I,
    (
        Spanned<Date>,
        Spanned<&'src Account<'src>>,
        HashSet<Spanned<&'src Currency<'src>>>,
        Option<Spanned<&'src str>>,
        (
            HashSet<Spanned<&'src Tag<'src>>>,
            HashSet<Spanned<&'src Link<'src>>>,
        ),
    ),
    extra::Err<ParserError<'src>>,
>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let date = select_ref!(Token::Date(date) => *date);
    let account = select_ref!(Token::Account(acc) => acc);
    let currency = select_ref!(Token::Currency(cur) => cur);
    let string = select_ref!(Token::StringLiteral(s) => s.deref());

    group((
        date.map_with_span(spanned),
        just(Token::Open),
        account.map_with_span(spanned),
        currency
            .map_with_span(spanned)
            .repeated()
            .collect::<Vec<_>>()
            .validate(|currencies, _span, emitter| {
                currencies
                    .into_iter()
                    .fold(HashSet::new(), |mut currencies, currency| {
                        if currencies.contains(&currency) {
                            emitter.emit(Rich::custom(
                                currency.span,
                                format!("duplicate currency {}", currency.value),
                            ))
                        } else {
                            currencies.insert(currency);
                        }

                        currencies
                    })
            }),
        string.map_with_span(spanned).or_not(),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .map(|(date, _, account, currency, booking, tags_links)| {
        (date, account, currency, booking, tags_links)
    })
}

/// Matches a close, including metadata, over several lines.
pub fn close<'src, I>() -> impl Parser<'src, I, Close<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    group((close_header_line(), metadata())).map(|((date, account, (tags, links)), metadata)| {
        Close {
            date,
            account,
            tags,
            links,
            metadata,
        }
    })
}

/// Matches the first line of a close.
fn close_header_line<'src, I>() -> impl Parser<
    'src,
    I,
    (
        Spanned<Date>,
        Spanned<&'src Account<'src>>,
        (
            HashSet<Spanned<&'src Tag<'src>>>,
            HashSet<Spanned<&'src Link<'src>>>,
        ),
    ),
    extra::Err<ParserError<'src>>,
>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let date = select_ref!(Token::Date(date) => *date);
    let account = select_ref!(Token::Account(acc) => acc);

    group((
        date.map_with_span(spanned),
        just(Token::Close),
        account.map_with_span(spanned),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .map(|(date, _, account, tags_links)| (date, account, tags_links))
}

/// Matches a commodity, including metadata, over several lines.
pub fn commodity<'src, I>() -> impl Parser<'src, I, Commodity<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    group((commodity_header_line(), metadata())).map(
        |((date, currency, (tags, links)), metadata)| Commodity {
            date,
            currency,
            tags,
            links,
            metadata,
        },
    )
}

/// Matches the first line of a commodity.
fn commodity_header_line<'src, I>() -> impl Parser<
    'src,
    I,
    (
        Spanned<Date>,
        Spanned<&'src Currency<'src>>,
        (
            HashSet<Spanned<&'src Tag<'src>>>,
            HashSet<Spanned<&'src Link<'src>>>,
        ),
    ),
    extra::Err<ParserError<'src>>,
>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let date = select_ref!(Token::Date(date) => *date);
    let currency = select_ref!(Token::Currency(cur) => cur);

    group((
        date.map_with_span(spanned),
        just(Token::Commodity),
        currency.map_with_span(spanned),
        tags_links(),
    ))
    .then_ignore(just(Token::Eol))
    .map(|(date, _, currency, tags_link)| (date, currency, tags_link))
}

/// Matches the `txn` keyword or a flag.
pub fn txn<'src, I>() -> impl Parser<'src, I, Flag, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((just(Token::Txn).to(Flag::default()), flag()))
}

/// Matches any flag, dedicated or overloaded
pub fn flag<'src, I>() -> impl Parser<'src, I, Flag, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let dedicated_flag = select_ref!(Token::DedicatedFlag(flag) => *flag);

    choice((
        dedicated_flag,
        just(Token::Asterisk).to(Flag::Asterisk),
        just(Token::Hash).to(Flag::Hash),
    ))
}

/// Matches a `Posting` complete with `Metadata` over several lines.
fn posting<'src, I>() -> impl Parser<'src, I, Posting<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let account = select_ref!(Token::Account(acc) => acc);
    let currency = select_ref!(Token::Currency(cur) => cur);

    just(Token::Indent)
        .ignore_then(
            group((
                flag().map_with_span(spanned).or_not(),
                account.map_with_span(spanned),
                expr().map_with_span(spanned).or_not(),
                currency.map_with_span(spanned).or_not(),
                cost_spec().map_with_span(spanned).or_not(),
                price_annotation().map_with_span(spanned).or_not(),
            ))
            .then_ignore(just(Token::Eol))
            .then(metadata())
            .map(
                |((flag, account, amount, currency, cost_spec, price_annotation), metadata)| {
                    Posting {
                        flag,
                        account,
                        amount,
                        currency,
                        cost_spec,
                        price_annotation,
                        metadata,
                    }
                },
            ),
        )
        .labelled("posting")
        .as_context()
}

/// Matches `Metadata`, over several lines.
fn metadata<'src, I>() -> impl Parser<'src, I, Metadata<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use Metadatum::*;

    metadatum_line()
        .repeated()
        .collect::<Vec<_>>()
        .validate(|metadata, _span, emitter| {
            // collate by type of metadatum
            metadata
                .into_iter()
                .fold(Metadata::default(), |mut m, item| match item {
                    KeyValue(kv) => {
                        use hash_map::Entry::*;

                        let MetaKeyValue { key, value } = kv.value;

                        let key_span = key.span;
                        match m.key_values.entry(key) {
                            Occupied(entry) => emitter.emit(Rich::custom(
                                key_span,
                                format!("duplicate key {}", entry.key().value),
                            )),
                            Vacant(entry) => {
                                entry.insert(value);
                            }
                        }

                        m
                    }
                    Tag(tag) => {
                        if m.tags.contains(&tag) {
                            emitter.emit(Rich::custom(
                                tag.span,
                                format!("duplicate tag {}", tag.value),
                            ))
                        } else {
                            m.tags.insert(tag);
                        }

                        m
                    }
                    Link(link) => {
                        if m.links.contains(&link) {
                            emitter.emit(Rich::custom(
                                link.span,
                                format!("duplicate link {}", link.value),
                            ))
                        } else {
                            m.links.insert(link);
                        }

                        m
                    }
                })
        })
}

/// A single instance of `Metadata`
enum Metadatum<'a> {
    KeyValue(Spanned<MetaKeyValue<'a>>),
    Tag(Spanned<&'a Tag<'a>>),
    Link(Spanned<&'a Link<'a>>),
}

/// Matches a single Metadatum on a single line.
fn metadatum_line<'src, I>() -> impl Parser<'src, I, Metadatum<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use Metadatum::*;

    let key = select_ref!(Token::Key(key) => key);
    let tag = select_ref!(Token::Tag(tag) => tag);
    let link = select_ref!(Token::Link(link) => link);

    just(Token::Indent)
        .ignore_then(
            choice((
                key.map_with_span(spanned)
                    .then(just(Token::Colon).ignore_then(meta_value().map_with_span(spanned)))
                    .map_with_span(|(key, value), span| {
                        KeyValue(spanned(MetaKeyValue { key, value }, span))
                    }),
                tag.map_with_span(spanned).map(Tag),
                link.map_with_span(spanned).map(Link),
            ))
            .then_ignore(just(Token::Eol)),
        )
        .labelled("metadata")
        .as_context()
}

/// Matches a `MetaValue`.
pub fn meta_value<'src, I>() -> impl Parser<'src, I, MetaValue<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use MetaValue::*;

    choice((simple_value().map(Simple), amount().map(Amount)))
}

/// Matches a `SimpleValue`.
/// TODO: the original parser allowed for the SimpleValue to be empty, which we don't support here,
/// unless and until it becomes necessary, because it seems a bit nasty to me. 🤷
pub fn simple_value<'src, I>(
) -> impl Parser<'src, I, SimpleValue<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use SimpleValue::*;

    let string = select_ref!(Token::StringLiteral(s) => s.deref());
    let currency = select_ref!(Token::Currency(cur) => cur);
    let account = select_ref!(Token::Account(acc) => acc);
    let tag = select_ref!(Token::Tag(tag) => tag);
    let link = select_ref!(Token::Link(link) => link);
    let date = select_ref!(Token::Date(date) => *date);

    choice((
        string.map(String),
        currency.map(Currency),
        account.map(Account),
        tag.map(Tag),
        link.map(Link),
        date.map(Date),
        bool().map(Bool),
        just(Token::Null).to(None),
        expr_value().map(Expr),
    ))
}

pub fn amount<'src, I>() -> impl Parser<'src, I, Amount<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let currency = select_ref!(Token::Currency(cur) => cur);

    group((
        expr_value().map_with_span(spanned),
        currency.map_with_span(spanned),
    ))
    .map(Amount::new)
}

pub fn loose_amount<'src, I>(
) -> impl Parser<'src, I, LooseAmount<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let currency = select_ref!(Token::Currency(cur) => cur);

    group((
        expr().map_with_span(spanned).or_not(),
        currency.map_with_span(spanned).or_not(),
    ))
    .map(LooseAmount::new)
}

pub fn compound_amount<'src, I>(
) -> impl Parser<'src, I, ScopedAmount<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use ScopedAmount::*;

    let currency = select_ref!(Token::Currency(cur) => cur);

    choice((
        (compound_expr().then(currency)).map(|(amount, cur)| CurrencyAmount(amount, cur)),
        compound_expr().map(BareAmount),
        currency.map(BareCurrency),
    ))
}

pub fn compound_expr<'src, I>() -> impl Parser<'src, I, ScopedExpr, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use ScopedExpr::*;

    choice((
        expr().then_ignore(just(Token::Hash)).map(PerUnit),
        expr().map(PerUnit),
        just(Token::Hash).ignore_then(expr()).map(Total),
    ))
}

pub fn price_annotation<'src, I>(
) -> impl Parser<'src, I, ScopedAmount<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use ScopedAmount::*;

    let currency = select_ref!(Token::Currency(cur) => cur);
    fn scope(amount: Expr, is_total: bool) -> ScopedExpr {
        use ScopedExpr::*;

        if is_total {
            Total(amount)
        } else {
            PerUnit(amount)
        }
    }

    group((
        choice((just(Token::At).to(false), just(Token::AtAt).to(true))),
        expr().or_not(),
        currency.or_not(),
    ))
    .try_map(|(is_total, amount, cur), span| match (amount, cur) {
        (Some(amount), Some(cur)) => Ok(CurrencyAmount(scope(amount, is_total), cur)),
        (Some(amount), None) => Ok(BareAmount(scope(amount, is_total))),
        (None, Some(cur)) => Ok(BareCurrency(cur)),
        (None, None) => Err(Rich::custom(span, "empty price annotation")),
    })
}

/// Matches a `CostSpec`.
/// For now we only match the new syntax of single braces.
fn cost_spec<'src, I>() -> impl Parser<'src, I, CostSpec<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use self::ScopedAmount::*;
    use CostComp::*;

    just(Token::Lcurl)
        .ignore_then(
            cost_comp()
                .map_with_span(spanned)
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::Rcurl))
        .try_map(move |cost_comps, span| {
            cost_comps
                .into_iter()
                .fold(
                    // accumulate the `CostComp`s in a `CostSpecBuilder`
                    CostSpecBuilder::default(),
                    |builder, cost_comp| match cost_comp.value {
                        ScopedAmount(compound_amount) => match compound_amount {
                            BareCurrency(cur) => builder.currency(cur, cost_comp.span),
                            BareAmount(amount) => builder.compound_expr(amount, cost_comp.span),
                            CurrencyAmount(amount, cur) => builder
                                .compound_expr(amount, cost_comp.span)
                                .currency(cur, cost_comp.span),
                        },
                        Date(date) => builder.date(date, cost_comp.span),
                        Label(s) => builder.label(s, cost_comp.span),
                        Merge => builder.merge(cost_comp.span),
                    },
                )
                .build()
                .map_err(|e| Rich::custom(span, e.to_string()))
        })
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// One component of a cost specification.
/// Setting a field type multiple times is rejected by methods in `CostSpec`.
enum CostComp<'a> {
    ScopedAmount(ScopedAmount<'a>),
    Date(Date),
    Label(&'a str),
    Merge,
}

/// Matches one component of a `CostSpec`.
fn cost_comp<'src, I>() -> impl Parser<'src, I, CostComp<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use CostComp::*;

    let string = select_ref!(Token::StringLiteral(s) => s.deref());
    let date = select_ref!(Token::Date(date) => *date);

    choice((
        compound_amount().map(ScopedAmount),
        date.map(Date),
        string.map(Label),
        just(Token::Asterisk).to(Merge),
    ))
}

/// Matches zero or more tags or links.
/// Duplicates are errors.
pub fn tags_links<'src, I>() -> impl Parser<
    'src,
    I,
    (
        HashSet<Spanned<&'src Tag<'src>>>,
        HashSet<Spanned<&'src Link<'src>>>,
    ),
    extra::Err<ParserError<'src>>,
>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let tag = select_ref!(Token::Tag(tag) => tag);
    let link = select_ref!(Token::Link(link) => link);

    choice((
        tag.map_with_span(spanned).map(Either::Left),
        link.map_with_span(spanned).map(Either::Right),
    ))
    .repeated()
    .collect::<Vec<_>>()
    .validate(|tags_or_links, _span, emitter| {
        tags_or_links.into_iter().fold(
            (HashSet::new(), HashSet::new()),
            |(mut tags, mut links), item| match item {
                Either::Left(tag) => {
                    if tags.contains(&tag) {
                        emitter.emit(Rich::custom(
                            tag.span,
                            format!("duplicate tag {}", tag.value),
                        ))
                    } else {
                        tags.insert(tag);
                    }

                    (tags, links)
                }
                Either::Right(link) => {
                    if links.contains(&link) {
                        emitter.emit(Rich::custom(
                            link.span,
                            format!("duplicate link {}", link.value),
                        ))
                    } else {
                        links.insert(link);
                    }

                    (tags, links)
                }
            },
        )
    })
}

/// Matches a bool
pub fn bool<'src, I>() -> impl Parser<'src, I, bool, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((just(Token::True).to(true), just(Token::False).to(false)))
}

/// Match and evaluate an expression
pub fn expr_value<'src, I>() -> impl Parser<'src, I, ExprValue, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    expr().map(ExprValue::from)
}

/// Match an expression
pub fn expr<'src, I>() -> impl Parser<'src, I, Expr, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>
        + ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use Token::*;

    recursive(|expr| {
        // Match a parenthesized expression
        let parens = expr
            .clone()
            .delimited_by(just(Lparen), just(Rparen))
            .map(|x| Expr::Paren(Box::new(x)));

        // Match a negated expression
        let negated = just(Minus)
            .ignore_then(expr)
            .map(|x| Expr::Neg(Box::new(x)));

        // Match a bare number
        let number = select_ref! { Number(x) => Expr::Value(*x) };

        // Match a factor of an expression
        let factor = number.or(negated).or(parens.clone());

        // Match a product of factors
        let product = factor.clone().foldl(
            choice((
                just(Asterisk).to(Expr::Mul as fn(_, _) -> _),
                just(Slash).to(Expr::Div as fn(_, _) -> _),
            ))
            .then(factor.clone())
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        // Match an expression
        product.clone().foldl(
            choice((
                just(Plus).to(Expr::Add as fn(_, _) -> _),
                just(Minus).to(Expr::Sub as fn(_, _) -> _),
            ))
            .then(product.clone())
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        )
    })
}

mod tests;
