use super::*;
use crate::lexer::{lex, Token};
use chumsky::{input::BorrowInput, prelude::*};

pub type Span = SimpleSpan<usize>;

type ParserError<'a> = Rich<'a, Token<'a>, Span>;

fn tokenize(s: &str) -> Vec<(Token, SimpleSpan)> {
    let token_iter = lex(s).map(|(tok, span)| (tok, SimpleSpan::from(span)));
    token_iter.collect::<Vec<(Token, Span)>>()
}

fn end_of_input(s: &str) -> Span {
    (s.len()..s.len()).into()
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

use expr::expr;
mod expr;
mod tests;
