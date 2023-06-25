use std::iter::once;

use super::*;
use chumsky::{prelude::*, text::keyword};

/// Matches `Account`.
pub fn account() -> impl Parser<char, Account, Error = Simple<char>> {
    account_type()
        .then(
            just(':')
                .ignored()
                .then(sub_account())
                .repeated()
                .at_least(1),
        )
        .map(|(acc_type, unit_subs)| {
            Account::new(
                acc_type,
                NonEmpty::collect(unit_subs.into_iter().map(|(_, sub)| sub)).unwrap(),
            )
        })
}

/// Matches `AccountType`.
pub fn account_type() -> impl Parser<char, AccountType, Error = Simple<char>> {
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
pub fn sub_account() -> impl Parser<char, SubAccount, Error = Simple<char>> {
    filter(|c| SubAccount::is_valid_initial(&c))
        .then(
            filter(|c| SubAccount::is_valid_subsequent(&c))
                .repeated()
                .at_least(0),
        )
        .try_map(|(initial, subsequent), span| {
            once(initial)
                .chain(subsequent.into_iter())
                .collect::<Vec<char>>()
                .into_iter()
                .collect::<String>()
                .parse::<SubAccount>()
                .map_err(|e| Simple::custom(span, format!("{}", e)))
        })
}

pub fn currency() -> impl Parser<char, Currency, Error = Simple<char>> {
    filter(|c| Currency::is_valid_initial(&c))
        .then(
            // we recognize more than is legal, but the parse fails in that case
            filter(|c| Currency::is_valid_intermediate(&c))
                .repeated()
                .at_least(0),
        )
        .try_map(|(initial, subsequent), span| {
            once(initial)
                .chain(subsequent.into_iter())
                .collect::<Vec<char>>()
                .into_iter()
                .collect::<String>()
                .parse::<Currency>()
                .map_err(|e| Simple::custom(span, format!("{}", e)))
        })
}
mod tests;
