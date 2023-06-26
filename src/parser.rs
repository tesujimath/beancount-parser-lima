use super::*;
use chumsky::{prelude::*, span::SimpleSpan, text::keyword};

pub type Span = SimpleSpan<usize>;

/// Matches `Account`.
pub fn account<'src>() -> impl Parser<'src, &'src str, Account, extra::Err<Rich<'src, char, Span>>>
{
    account_type()
        .then(
            just(':')
                .ignored()
                .then(sub_account())
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .map(|(acc_type, unit_subs)| {
            Account::new(
                acc_type,
                NonEmpty::collect(unit_subs.into_iter().map(|(_, sub)| sub)).unwrap(),
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
mod tests;
