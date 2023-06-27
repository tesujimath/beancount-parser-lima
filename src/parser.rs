use super::{lexer::Span, *};
use chumsky::{
    prelude::*,
    text::{inline_whitespace, keyword},
};

use expr::expr;

/// Matches `AccountType`.
pub fn account_type<'src>(
) -> impl Parser<'src, &'src str, AccountType, extra::Err<Rich<'src, char, Span>>> {
    use AccountType::*;

    choice((
        keyword("Assets").to(Assets),
        keyword("Liabilities").to(Liabilities),
        keyword("Equity").to(Equity),
        keyword("Income").to(Income),
        keyword("Expenses").to(Expenses),
    ))
}

/// Matches `AccountName`.
pub fn account_name<'src>(
) -> impl Parser<'src, &'src str, AccountName, extra::Err<Rich<'src, char, Span>>> {
    any()
        .filter(AccountName::is_valid_initial)
        .then(any().filter(AccountName::is_valid_subsequent).repeated())
        .slice()
        .try_map(|s: &str, span| {
            s.parse::<AccountName>()
                .map_err(|e| chumsky::error::Rich::custom(span, e))
        })
}

pub fn currency<'src>() -> impl Parser<'src, &'src str, Currency, extra::Err<Rich<'src, char, Span>>>
{
    any()
        .filter(Currency::is_valid_initial)
        .then(
            // we recognize more than is legal, but the parse fails in that case
            any().filter(Currency::is_valid_intermediate).repeated(),
        )
        .slice()
        .try_map(|s: &str, span| {
            s.parse::<Currency>()
                .map_err(|e| chumsky::error::Rich::custom(span, e))
        })
}

pub fn compound_expr<'src>(
) -> impl Parser<'src, &'src str, CompoundExpr, extra::Err<Rich<'src, char, Span>>> {
    use CompoundExpr::*;

    // we need to try for the variant with trailing # before the bare expression
    expr()
        .then_ignore(inline_whitespace().then(just('#')))
        .map(PerUnit)
        .or(expr().map(PerUnit))
        .or((just('#').then(inline_whitespace()))
            .ignore_then(expr())
            .map(Total))
}

mod expr;
mod tests;
