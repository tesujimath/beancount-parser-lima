use crate::lexer::{lex, Token};
use chumsky::prelude::*;

pub type Span = SimpleSpan<usize>;

type ParserError<'a> = Rich<'a, Token<'a>, Span>;

fn tokenize(s: &str) -> Vec<(Token, SimpleSpan)> {
    let token_iter = lex(s).map(|(tok, span)| (tok, SimpleSpan::from(span)));
    token_iter.collect::<Vec<(Token, Span)>>()
}

fn end_of_input(s: &str) -> Span {
    (s.len()..s.len()).into()
}

mod expr;
mod tests;
