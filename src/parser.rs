use chumsky::prelude::*;

use crate::lexer::{lex, Token};

pub type Span = SimpleSpan<usize>;

fn tokenize(s: &str) -> Vec<(Token, Span)> {
    lex(s).map(|(tok, span)| (tok, span.into())).collect()
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
