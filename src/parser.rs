use super::*;
use chumsky::{prelude::*, text::keyword};

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
