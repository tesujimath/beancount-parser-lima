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
