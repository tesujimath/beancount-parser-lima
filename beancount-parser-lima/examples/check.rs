use rust_decimal::Decimal;
use std::io::Write;
use std::path::PathBuf;
use std::{collections::HashMap, io};

use beancount_parser_lima::{
    BeancountParser, BeancountSources, Directive, DirectiveVariant, Error, ParseError,
    ParseSuccess, Posting, Spanned,
};

/// This is a non-comprehensive example of reporting semantic errors in context.
/// These are not parse errors per se, so not the business of the core parser to deletct and report.
/// Yet reporting with specific source location remains essential.
///
/// Only these checks are made:
///
/// 1. A transaction with all postings specifying an amount must sum to zero. (It is good practice to let the last posting amount be inferred.)
///
/// 2. No posting must refer to an unknown or closed account.
fn main() {
    let flags = xflags::parse_or_exit! {
        /// File to parse
        required path: PathBuf
    };

    let stderr = &io::stderr();
    let sources = BeancountSources::new(flags.path);
    let parser = BeancountParser::new(&sources);

    parse(&sources, &parser, stderr);
}

fn parse<W>(sources: &BeancountSources, parser: &BeancountParser, error_w: W)
where
    W: Write + Copy,
{
    match parser.parse() {
        Ok(ParseSuccess {
            directives,
            options: _,
            mut warnings,
        }) => {
            let mut accounts = HashMap::new();
            let mut errors = Vec::new();

            for d in directives {
                use DirectiveVariant::*;

                match d.variant() {
                    Transaction(x) => {
                        check_postings_amounts(&d, x.postings(), &mut errors);
                        check_postings_accounts(&d, x.postings(), &accounts, &mut errors);
                    }

                    Open(x) => {
                        // using strings as keys is a bit crass, but that's not the point of this example
                        let account_key = x.account().to_string();
                        match accounts.get(&account_key) {
                            None => {
                                accounts.insert(account_key, AccountStatus::opened(d));
                            }
                            Some(AccountStatus {
                                opened,
                                closed: None,
                            }) => {
                                warnings.push(d.warning("duplicate open").related_to(opened));
                            }
                            Some(AccountStatus {
                                closed: Some(closed),
                                ..
                            }) => {
                                errors.push(d.error("account was closed").related_to(closed));
                            }
                        }
                    }

                    Close(x) => {
                        // using strings as keys is a bit crass, but that's not the point of this example
                        let account = x.account();
                        let account_key = account.to_string();
                        match accounts.get(&account_key) {
                            None => {
                                errors.push(account.error("no such account").in_context(&d));
                            }
                            Some(AccountStatus { closed: None, .. }) => {
                                let account_status = accounts.get_mut(&account_key).unwrap();
                                account_status.closed = Some(d);
                            }
                            Some(AccountStatus {
                                closed: Some(closed),
                                ..
                            }) => {
                                errors.push(d.error("account already closed").related_to(closed));
                            }
                        }
                    }

                    _ => (),
                }
            }

            sources.write(error_w, warnings).unwrap();

            if !errors.is_empty() {
                sources.write(error_w, errors).unwrap();
            }
        }

        Err(ParseError { errors, warnings }) => {
            sources.write(error_w, errors).unwrap();
            sources.write(error_w, warnings).unwrap();
        }
    }
}

struct AccountStatus<'a> {
    opened: Spanned<Directive<'a>>,
    closed: Option<Spanned<Directive<'a>>>,
}

impl<'a> AccountStatus<'a> {
    fn opened(d: Spanned<Directive<'a>>) -> Self {
        AccountStatus {
            opened: d,
            closed: None,
        }
    }
}

fn check_postings_amounts<'a>(
    d: &'a Spanned<Directive<'a>>,
    postings: impl ExactSizeIterator<Item = &'a Spanned<Posting<'a>>>,
    errors: &mut Vec<Error>,
) {
    let n_postings = postings.len();
    let mut amounts_with_value = postings.filter_map(|p| p.amount()).collect::<Vec<_>>();

    if n_postings > 0 && amounts_with_value.len() == n_postings {
        let total: Decimal = amounts_with_value.iter().map(|x| x.value()).sum();

        if total != Decimal::ZERO {
            let last_amount = amounts_with_value.pop().unwrap(); // can't fail as n_postings > 0

            errors.push(
                last_amount
                    .error(format!("sum is {}, expected zero", total))
                    .related_to_all(amounts_with_value)
                    .in_context(d),
            )
        }
    }
}

fn check_postings_accounts<'a>(
    d: &'a Spanned<Directive<'a>>,
    postings: impl ExactSizeIterator<Item = &'a Spanned<Posting<'a>>>,
    accounts: &HashMap<String, AccountStatus>,
    errors: &mut Vec<Error>,
) {
    for posting in postings {
        let account = posting.account();
        let account_key = account.to_string();

        match accounts.get(&account_key) {
            None => {
                errors.push(
                    account
                        .error("no such account")
                        .in_context(posting)
                        .in_context(d),
                );
            }
            Some(AccountStatus {
                closed: Some(closed),
                ..
            }) => {
                errors.push(
                    account
                        .error("account is closed")
                        .in_context(posting)
                        .in_context(d)
                        .related_to(closed),
                );
            }
            _ => (),
        }
    }
}
