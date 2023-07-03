use super::*;
use crate::lexer::{lex, Token};
use chumsky::{input::BorrowInput, prelude::*};
use either::Either;

pub type Span = SimpleSpan<usize>;

type ParserError<'a> = Rich<'a, Token<'a>, Span>;

fn tokenize(s: &str) -> Vec<(Token, SimpleSpan)> {
    let token_iter = lex(s).map(|(tok, span)| (tok, SimpleSpan::from(span)));
    token_iter.collect::<Vec<(Token, Span)>>()
}

fn end_of_input(s: &str) -> Span {
    (s.len()..s.len()).into()
}

/// Matches a transaction.
// TODO EOL and metadata/postings
pub fn transaction<'src, I>(
) -> impl Parser<'src, I, Transaction<'src>, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let date = select_ref!(Token::Date(date) => *date);
    let string = select_ref!(Token::StringLiteral(s) => s);

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

use expr::expr;
mod expr;
mod tests;
