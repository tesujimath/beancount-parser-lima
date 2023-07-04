use std::ops::Deref;

use super::*;
use crate::lexer::Token;
use chumsky::{input::BorrowInput, prelude::*};
use either::Either;

pub type Span = SimpleSpan<usize>;

pub fn end_of_input(s: &str) -> Span {
    (s.len()..s.len()).into()
}

type ParserError<'a> = Rich<'a, Token<'a>, Span>;

/// Matches a transaction.
// TODO EOL and metadata/postings
pub fn transaction<'src, I>(
) -> impl Parser<'src, I, Transaction<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let date = select_ref!(Token::Date(date) => *date);
    let string = select_ref!(Token::StringLiteral(s) => s.deref());

    group((date, txn(), string.or_not(), string.or_not(), tags_links())).map(
        |(date, flag, s1, s2, (tags, links))| match (s1, s2) {
            // a single string is narration
            (Some(s1), None) => Transaction::new(date, flag, None, Some(s1), tags, links),
            (s1, s2) => Transaction::new(date, flag, s1, s2, tags, links),
        },
    )
}

/// Matches the `txn` keyword or a flag.
pub fn txn<'src, I>() -> impl Parser<'src, I, Flag, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((just(Token::Txn).to(Flag::default()), flag()))
}

/// Matches any flag, dedicated or overloaded
pub fn flag<'src, I>() -> impl Parser<'src, I, Flag, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let dedicated_flag = select_ref!(Token::DedicatedFlag(flag) => *flag);

    choice((
        dedicated_flag,
        just(Token::Asterisk).to(Flag::Asterisk),
        just(Token::Hash).to(Flag::Hash),
    ))
}

/// Matches `Metadata`, over several lines.
fn metadata<'src, I>() -> impl Parser<'src, I, Metadata<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use Metadatum::*;

    metadatum_line()
        .repeated()
        .collect::<Vec<_>>()
        .map(|metadata| {
            // collate by type of metadatum
            metadata
                .into_iter()
                .fold(Metadata::new(), |mut m, item| match item {
                    KeyValue(kv) => {
                        m.key_values.push(kv);
                        m
                    }
                    Tag(tag) => {
                        m.tags.push(tag);
                        m
                    }
                    Link(link) => {
                        m.links.push(link);
                        m
                    }
                })
        })
}

/// A single instance of `Metadata`
enum Metadatum<'a> {
    KeyValue((&'a Key<'a>, MetaValue<'a>)),
    Tag(&'a Tag<'a>),
    Link(&'a Link<'a>),
}

/// Matches a single Metadatum on a single line.
fn metadatum_line<'src, I>() -> impl Parser<'src, I, Metadatum<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use Metadatum::*;

    let key = select_ref!(Token::Key(key) => key);
    let tag = select_ref!(Token::Tag(tag) => tag);
    let link = select_ref!(Token::Link(link) => link);

    just(Token::Indent).ignore_then(
        choice((
            key.then(just(Token::Colon).ignore_then(meta_value()))
                .map(KeyValue),
            tag.map(Tag),
            link.map(Link),
        ))
        .then_ignore(just(Token::Eol)),
    )
}

/// Matches a `MetaValue`.
pub fn meta_value<'src, I>() -> impl Parser<'src, I, MetaValue<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
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
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
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
        expr().map(Expr),
    ))
}

pub fn amount<'src, I>() -> impl Parser<'src, I, Amount<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let currency = select_ref!(Token::Currency(cur) => cur);

    group((expr(), currency)).map(Amount::new)
}

pub fn loose_amount<'src, I>(
) -> impl Parser<'src, I, LooseAmount<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let currency = select_ref!(Token::Currency(cur) => cur);

    group((expr().or_not(), currency.or_not())).map(LooseAmount::new)
}

pub fn compound_amount<'src, I>(
) -> impl Parser<'src, I, CompoundAmount<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use CompoundAmount::*;

    let currency = select_ref!(Token::Currency(cur) => cur);

    choice((
        (compound_expr().then(currency)).map(|(amount, cur)| CurrencyAmount(amount, cur)),
        compound_expr().map(BareAmount),
        currency.map(BareCurrency),
    ))
}

pub fn compound_expr<'src, I>() -> impl Parser<'src, I, CompoundExpr, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use CompoundExpr::*;

    choice((
        expr().then_ignore(just(Token::Hash)).map(PerUnit),
        expr().map(PerUnit),
        just(Token::Hash).ignore_then(expr()).map(Total),
    ))
}

#[derive(PartialEq, Eq, Clone, Debug)]
/// One component of a cost specification.
/// Setting a field type multiple times is rejected by methods in `CostSpec`.
enum CostComp<'a> {
    CompoundAmount(CompoundAmount<'a>),
    Date(NaiveDate),
    Label(&'a str),
    Merge,
}

/// Matches one component of a `CostSpec`.
fn cost_comp<'src, I>() -> impl Parser<'src, I, CostComp<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use CostComp::*;

    let string = select_ref!(Token::StringLiteral(s) => s.deref());
    let date = select_ref!(Token::Date(date) => *date);

    choice((
        compound_amount().map(CompoundAmount),
        date.map(Date),
        string.map(Label),
        just(Token::Asterisk).to(Merge),
    ))
}

/// Matches zero or more tags or links.
pub fn tags_links<'src, I>(
) -> impl Parser<'src, I, (Vec<&'src Tag<'src>>, Vec<&'src Link<'src>>), extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let tag = select_ref!(Token::Tag(tag) => tag);
    let link = select_ref!(Token::Link(link) => link);

    choice((tag.map(Either::Left), link.map(Either::Right)))
        .repeated()
        .collect::<Vec<_>>()
        .map(|tags_or_links| {
            tags_or_links.into_iter().fold(
                (Vec::new(), Vec::new()),
                |(mut tags, mut links), item| match item {
                    Either::Left(tag) => (
                        {
                            tags.push(tag);
                            tags
                        },
                        links,
                    ),
                    Either::Right(link) => (tags, {
                        links.push(link);
                        links
                    }),
                },
            )
        })
}

/// Matches a bool
pub fn bool<'src, I>() -> impl Parser<'src, I, bool, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    choice((just(Token::True).to(true), just(Token::False).to(false)))
}

use expr::expr;
mod expr;
mod tests;
