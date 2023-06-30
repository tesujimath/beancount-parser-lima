use chumsky::{
    input::{SpannedInput, Stream, ValueInput},
    prelude::*,
};

use crate::lexer::{lex, Token};

pub type Span = SimpleSpan<usize>;
type ParserError<'a> = extra::Err<Rich<'a, Token<'a>, Span>>;

// fn parse<'src>(s: &'src str) -> () {
//     let token_iter = lex(s).map(|(tok, span)| (tok, span.into()));
//     let token_stream = Stream::from_iter(token_iter).spanned(s.len()..s.len());

//     let result = expr::expr(token_stream);
// }

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
