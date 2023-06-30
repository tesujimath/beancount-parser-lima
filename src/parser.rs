use super::Expr;
use crate::lexer::{lex, Token};
use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};

pub type Span = SimpleSpan<usize>;
type ParserError<'a> = Rich<'a, Token<'a>, Span>;

// can't quite get the types or traits or constraints to match up here
// fn parse<'src, F, P, I, O>(s: &'src str, f: F) -> ParseResult<O, ParserError<'src>>
// where
//     F: Fn() -> P,
//     P: Parser<'src, I, O, extra::Err<ParserError<'src>>>,
//     I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
// {
//     let token_iter = lex(s).map(|(tok, span)| (tok, SimpleSpan::from(span)));
//     let token_stream = Stream::from_iter(token_iter).spanned((s.len()..s.len()).into());

//     f().parse(token_stream)
// }

fn parse_expr<'src, I>(s: &'src str) -> ParseResult<Expr, ParserError<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    let token_iter = lex(s).map(|(tok, span)| (tok, SimpleSpan::from(span)));
    let token_stream = Stream::from_iter(token_iter).spanned((s.len()..s.len()).into());

    expr::expr().parse(token_stream)
}

// pub fn compound_expr<'src>(
// ) -> impl Parser<'src, &'src str, CompoundExpr, extra::Err<Rich<'src, char, Span>>> {
//     use CompoundExpr::*;

//     // we need to try for the variant with trailing # before the bare expression
//     expr()
//         .then_ignore(inline_whitespace().then(just('#')))
//         .map(PerUnit)
//         .or(expr().map(PerUnit))
//         .or((just('#').then(inline_whitespace()))
//             .ignore_then(expr())
//             .map(Total))
// }

mod expr;
mod tests;
