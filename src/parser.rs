use std::iter::once;

use super::*;
use chumsky::{prelude::*, text::keyword};

/// Matches `Account`.
pub fn account() -> impl Parser<char, Account, Error = Simple<char>> {
    account_type()
        .then_ignore(
            // TODO many1 of these
            just(':'),
        )
        .then(sub_account())
        .try_map(|(acc_type, sub), span| {
            Account::new(
                acc_type,
                once(sub)
                    // .chain(colon_sub_pairs.into_iter().map(|(_colon, sub)| sub))
                    .collect(),
            )
            .map_err(|e| Simple::custom(span, format!("{}", e)))
        })
}

/// Matches `AccountType`.
pub fn account_type() -> impl Parser<char, AccountType, Error = Simple<char>> {
    choice((
        keyword("Assets").to(AccountType::Assets),
        keyword("Liabilities").to(AccountType::Liabilities),
        keyword("Equity").to(AccountType::Equity),
        keyword("Income").to(AccountType::Income),
        keyword("Expenses").to(AccountType::Expenses),
    ))
}

/// Matches `SubAccount`.
pub fn sub_account() -> impl Parser<char, SubAccount, Error = Simple<char>> {
    filter(|c| SubAccount::is_valid_initial(&c))
        .then(filter(|c| SubAccount::is_valid_subsequent(&c)))
        .try_map(|(initial, subsequent), span| {
            (vec![initial, subsequent])
                .iter()
                .collect::<String>()
                .parse::<SubAccount>()
                .map_err(|e| Simple::custom(span, format!("{}", e)))
        })
}

mod tests;
