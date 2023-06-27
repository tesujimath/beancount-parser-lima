use super::{lexer::Span, *};
use chumsky::{
    prelude::*,
    text::{inline_whitespace, keyword},
};

use expr::expr;

/// Matches `Account`.
pub fn account<'src>() -> impl Parser<'src, &'src str, Account, extra::Err<Rich<'src, char, Span>>>
{
    account_type()
        .then(
            just(':')
                .ignore_then(sub_account())
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .map(|(acc_type, sub_accounts)| {
            Account::new(
                acc_type,
                NonEmpty::collect(sub_accounts.into_iter()).unwrap(),
            )
        })
}

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

/// Matches `SubAccount`.
pub fn sub_account<'src>(
) -> impl Parser<'src, &'src str, SubAccount, extra::Err<Rich<'src, char, Span>>> {
    any()
        .filter(SubAccount::is_valid_initial)
        .then(any().filter(SubAccount::is_valid_subsequent).repeated())
        .slice()
        .try_map(|s: &str, span| {
            s.parse::<SubAccount>()
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
