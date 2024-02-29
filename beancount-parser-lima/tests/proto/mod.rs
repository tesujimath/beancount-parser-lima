use ::beancount_parser_lima as lima;
use lima::{BeancountParser, BeancountSources, ParseError, ParseSuccess, Spanned};

fn check_proto(sources: &BeancountSources, parser: &BeancountParser, expected: &str) {
    let stderr = &std::io::stderr();

    let expected: Ledger = protobuf::text_format::parse_from_str(expected).unwrap();

    println!(
        "Expected ledger contains {} directives",
        expected.directives.len()
    );

    match parser.parse() {
        Ok(ParseSuccess { directives, .. }) => {
            assert_eq!(
                directives.len(),
                expected.directives.len(),
                "directives.len()"
            );
            for (actual, expected) in directives.iter().zip(expected.directives.iter()) {
                actual.expect_eq(expected);
            }
        }
        Err(ParseError { errors, .. }) => {
            let n_errors = errors.len();
            sources.write(stderr, errors).unwrap();
            panic!("parse failed with {} errors", n_errors);
        }
    }
}

pub fn check_proto_parse(input: &str, expected: &str) {
    let sources = BeancountSources::from(input);
    let parser = BeancountParser::new(&sources);

    check_proto(&sources, &parser, expected);
}

pub trait ExpectEq<Rhs>
where
    Rhs: ?Sized,
{
    fn expect_eq(&self, expected: &Rhs);
}

impl<'a> ExpectEq<Directive> for Spanned<lima::Directive<'a>> {
    fn expect_eq(&self, expected: &Directive) {
        self.date().expect_eq(&expected.date);

        // match (self.variant(), &expected.variant) {
        //     (lima::DirectiveVariant::Transaction(variant), Transaction(ref other)) => {
        //         variant.expect_eq(other);
        //     }
        //     (lima::DirectiveVariant::Balance(variant), Balance(ref other)) => {
        //         variant.expect_eq(other);
        //     }
        //     _ => panic!(
        //         "mismatched directive variant: got {}, expected {:?}",
        //         self, &expected
        //     ),
        // }
    }
}

impl<'a> ExpectEq<Date> for Spanned<time::Date> {
    fn expect_eq(&self, expected: &Date) {
        assert_eq!(self.year(), expected.year.unwrap(), "year of date");
        assert_eq!(
            self.month() as i32,
            expected.month.unwrap(),
            "month of date"
        );
        assert_eq!(self.day() as i32, expected.day.unwrap(), "day of date");

        // match (self.variant(), &expected.variant) {
        //     (lima::DirectiveVariant::Transaction(variant), Transaction(ref other)) => {
        //         variant.expect_eq(other);
        //     }
        //     (lima::DirectiveVariant::Balance(variant), Balance(ref other)) => {
        //         variant.expect_eq(other);
        //     }
        //     _ => panic!(
        //         "mismatched directive variant: got {}, expected {:?}",
        //         self, &expected
        //     ),
        // }
    }
}

use beancount::data::Directive;
use beancount::date::Date;
use beancount::ledger::Ledger;

mod beancount;
