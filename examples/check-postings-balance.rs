use rust_decimal::Decimal;
use std::io;
use std::path::PathBuf;

use beancount_parser::{
    BeancountParser, BeancountSources, DirectiveVariant, ParseError, ParseResult,
};

/// This is an example of rpeorting semantic errors by the application.
/// These are not parse errors per se, so not the business of the core parser to deletct and report.
///
/// The balancing algorithm is described here:
/// https://beancount.github.io/docs/beancount_language_syntax.html#balancing-rule-the-weight-of-postings
/// but this example does not implement all of that (because it is quite complex).
///
/// For this example, we ignore everything about a posting except its amount, and we don't care about currency.
fn main() {
    let flags = xflags::parse_or_exit! {
        /// File to parse
        required path: PathBuf
    };

    let stderr = &io::stderr();
    let sources = BeancountSources::new(flags.path);
    let beancount_parser = BeancountParser::new(&sources);

    match beancount_parser.parse() {
        Ok(ParseResult {
            directives,
            options: _,
            mut warnings,
        }) => {
            let mut errors = Vec::new();

            for directive in directives {
                if let DirectiveVariant::Transaction(transaction) = directive.variant() {
                    let mut postings = transaction.postings().collect::<Vec<_>>();
                    let n_postings = postings.len();
                    let n_amounts = itertools::partition(&mut postings, |p| p.amount().is_some());

                    if postings.is_empty() {
                        warnings.push(directive.warning("no postings"));
                    } else if n_amounts + 1 < n_postings {
                        errors.push(
                            directive
                                .error("multiple postings without amount specified")
                                .related_to_all(postings[n_amounts..].iter().copied()),
                        );
                    } else if n_amounts == n_postings {
                        let total: Decimal =
                            postings.iter().map(|p| p.amount().unwrap().value()).sum();

                        if total != Decimal::ZERO {
                            let last_posting = postings.pop().unwrap(); // can't fail as n_postings > 0

                            errors.push(
                                last_posting
                                    .amount()
                                    .unwrap()
                                    .error(format!("sum is {}, expected zero", total))
                                    .related_to_all(postings.iter().map(|p| p.amount().unwrap()))
                                    .in_context(&directive),
                            )
                        }
                    }
                }
            }

            sources.write(stderr, errors).unwrap();
            sources.write(stderr, warnings).unwrap();
        }

        Err(ParseError { errors, warnings }) => {
            sources.write(stderr, errors).unwrap();
            sources.write(stderr, warnings).unwrap();
        }
    }
}
