use anyhow::Result;
use rust_decimal::Decimal;
use std::io;
use std::path::PathBuf;

use beancount_parser::{BeancountParser, BeancountSources, DirectiveVariant};

/// This example is really a test that there is sufficient public access to parser output types.
/// We need to avoid leaning on the Display implementations to be sure we can extract a usable value in every case.
fn main() -> Result<()> {
    let flags = xflags::parse_or_exit! {
        /// File to parse
        required path: PathBuf
    };

    let error_w = &io::stderr();
    let sources = BeancountSources::new(flags.path);
    let beancount_parser = BeancountParser::new(&sources);

    match beancount_parser.parse() {
        Ok(directives) => {
            let mut errors = Vec::new();

            for d in directives {
                use DirectiveVariant::*;

                if let Transaction(x) = d.variant() {
                    let postings = x.postings();
                    let n_postings = postings.len();
                    let mut amounts_with_value =
                        postings.filter_map(|p| p.amount()).collect::<Vec<_>>();

                    if n_postings > 0 && amounts_with_value.len() == n_postings {
                        let total: Decimal = amounts_with_value.iter().map(|x| x.value()).sum();

                        if total != Decimal::ZERO {
                            let last_amount = amounts_with_value.pop().unwrap(); // can't fail as n_postings > 0

                            errors.push(
                                last_amount
                                    .error(
                                        "invalid amount",
                                        format!("sum is {}, expected zero", total),
                                    )
                                    .with("amount", amounts_with_value.into_iter())
                                    .in_transaction(&d),
                            )
                        }
                    }
                    // let mut total = Decimal::ZERO;

                    // for p in x.postings() {
                    //     if let Some(amount) = p.amount() {
                    //         // ignoring currency complications for now, this is just a quick example
                    //         total += amount.value();
                    //         // amount_spans.push(amount.span());
                    //     } else {
                    //         // can't fail, as an amount has been left blank
                    //         continue 'directives;
                    //     }

                    //     if total != Decimal::ZERO {
                    //         error.push(Error::new("invalid amount"))
                    //     }
                    // }
                }
            }

            if errors.is_empty() {
                Ok(())
            } else {
                sources.write_errors(error_w, errors).map_err(|e| e.into())
            }
        }

        Err(errors) => sources.write_errors(error_w, errors).map_err(|e| e.into()),
    }
}
