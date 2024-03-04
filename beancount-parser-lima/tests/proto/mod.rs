use ::beancount_parser_lima as lima;
use beancount::{
    data::{Amount, Balance, Directive, Posting},
    date::Date,
    ledger::Ledger,
    number::Number,
};
use derive_more::Display;
use lima::{BeancountParser, BeancountSources, OptionalItem, ParseError, ParseSuccess};
use rust_decimal::Decimal;
use std::{
    borrow::ToOwned, env, fmt::Display, fs::read_to_string, path::PathBuf, rc::Rc, str::FromStr,
};

fn check(
    sources: &BeancountSources,
    parser: &BeancountParser,
    expected_directives: Vec<Directive>,
) {
    let stderr = &std::io::stderr();

    println!(
        "Expected ledger contains {} directives",
        expected_directives.len()
    );

    match parser.parse() {
        Ok(ParseSuccess { directives, .. }) => {
            assert_eq!(
                directives.len(),
                expected_directives.len(),
                "directives.len()"
            );
            for (i, (actual, expected)) in directives
                .iter()
                .zip(expected_directives.iter())
                .enumerate()
            {
                actual.expect_eq(expected, context(format!("directive {}", i + 1)));
            }
        }
        Err(ParseError { errors, .. }) => {
            let n_errors = errors.len();
            sources.write(stderr, errors).unwrap();
            panic!("parse failed with {} errors", n_errors);
        }
    }
}

fn create_sources_and_check(input: &str, expected_directives: Vec<Directive>) {
    let sources = BeancountSources::from(input);
    let parser = BeancountParser::new(&sources);

    check(&sources, &parser, expected_directives);
}

pub fn check_parse(test_name: &str) {
    let cargo_manifest_dir: PathBuf = env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let testcase_dir =
        cargo_manifest_dir.join(["..", "beancount-parser-tests"].iter().collect::<PathBuf>());
    let input_file: PathBuf = Into::<PathBuf>::into(format!("{}.beancount", test_name));
    let input_path = testcase_dir.join(input_file);
    let expected_output_file: PathBuf = Into::<PathBuf>::into(format!("{}.txtpb", test_name));
    let expected_output_path = testcase_dir.join(expected_output_file);

    let input = read_to_string(&input_path)
        .unwrap_or_else(|_| panic!("failed to read input from {:?}", &input_path));

    let expected_output = read_to_string(&expected_output_path).unwrap_or_else(|_| {
        panic!(
            "failed to read expected output from {:?}",
            &expected_output_path
        )
    });
    let expected_output_ledger: Ledger =
        protobuf::text_format::parse_from_str(&expected_output).unwrap();

    create_sources_and_check(&input, expected_output_ledger.directives);
}

struct Context {
    label: String,
    parent: Option<Rc<Context>>,
}

fn context<S>(label: S) -> Rc<Context>
where
    S: AsRef<str>,
{
    Rc::new(Context {
        label: label.as_ref().to_owned(),
        parent: None,
    })
}

fn with_context<S>(parent: Rc<Context>, label: S) -> Rc<Context>
where
    S: AsRef<str>,
{
    Rc::new(Context {
        label: label.as_ref().to_owned(),
        parent: Some(parent),
    })
}

impl Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(parent) = &self.parent {
            write!(f, "{}.{}", parent, self.label)
        } else {
            write!(f, "{}", self.label)
        }
    }
}

trait ExpectEq<Rhs> {
    fn expect_eq(&self, expected: Rhs, ctx: Rc<Context>);
}

impl<E, T> ExpectEq<Option<E>> for Option<T>
where
    T: ExpectEq<Option<E>>,
{
    fn expect_eq(&self, expected: Option<E>, ctx: Rc<Context>) {
        match (self, expected) {
            (Some(actual), Some(expected)) => actual.expect_eq(Some(expected), ctx),
            (Some(_), None) => panic!("expected nothing found value{}", &ctx),
            (None, Some(_)) => panic!("expected value found nothing {}", &ctx),
            (None, None) => (),
        }
    }
}

impl<'a> ExpectEq<&Directive> for lima::Directive<'a> {
    fn expect_eq(&self, expected: &Directive, ctx: Rc<Context>) {
        self.date()
            .expect_eq(&expected.date, with_context(ctx.clone(), "date"));

        match self.variant() {
            //     (lima::DirectiveVariant::Transaction(variant), Transaction(ref other)) => {
            //         variant.expect_eq(other);
            //     }
            lima::DirectiveVariant::Balance(variant) if expected.has_balance() => {
                variant.expect_eq(expected.balance(), ctx);
            }

            _ => panic!(
                "mismatched directive variant: got {}, expected {:?} {}",
                self, &expected, &ctx
            ),
        }
    }
}

impl<'a> ExpectEq<&Balance> for lima::Balance<'a> {
    fn expect_eq(&self, expected: &Balance, ctx: Rc<Context>) {
        self.account().expect_eq(
            &expected.account,
            with_context(ctx.clone(), "balance.account"),
        );
        self.atol().amount().expect_eq(
            &expected.amount,
            with_context(ctx.clone(), "balance.amount"),
        );
        self.atol().tolerance().item().copied().expect_eq(
            expected.tolerance.as_ref(),
            with_context(ctx.clone(), "balance.tolerance"),
        );
    }
}

impl<'a> ExpectEq<&Posting> for lima::Posting<'a> {
    fn expect_eq(&self, expected: &Posting, ctx: Rc<Context>) {
        self.flag()
            .item()
            .copied()
            .expect_eq(expected.flag.as_ref(), ctx.clone());
        self.account().expect_eq(&expected.account, ctx.clone());
        // TODO
        // self.amount()
        //     .item()
        //     .expect_eq(&expected.amount, ctx.clone());
        // self.currency().expect_eq(&expected.currency, ctx.clone());
        // self.cost_spec().is(cost_spec);
        // self.price_annotation().is(price_annotation);
        // self.metadata().is(metadata);
    }
}

impl<'a> ExpectEq<&Vec<Posting>> for Vec<&'a lima::Posting<'a>> {
    fn expect_eq(&self, expected: &Vec<Posting>, ctx: Rc<Context>) {
        assert_eq!(self.len(), expected.len(), "postings.len");
        for (i, (actual, expected)) in self.iter().zip(expected.iter()).enumerate() {
            actual.expect_eq(
                expected,
                with_context(ctx.clone(), format!("posting {}", i + 1)),
            )
        }
    }
}

// TODO not actually needed I think
// impl<'a> ExpectEq<&str> for Account<'a> {
//     fn expect_eq(&self, expected: &str) {
//         assert_eq!(self, &account(expected));
//     }
// }

impl<'a> ExpectEq<&Option<String>> for lima::Account<'a> {
    fn expect_eq(&self, expected: &Option<String>, ctx: Rc<Context>) {
        match expected {
            Some(expected) => assert_eq!(self, &account(expected.as_str()), "{}", &ctx),
            None => panic!("missing account from expected {}", &ctx),
        }
    }
}

fn account(s: &str) -> lima::Account {
    let mut account = s.split(':');
    let account_type_name = account.by_ref().next().unwrap();
    let subaccount = account
        .map(lima::AccountName::try_from)
        .collect::<Result<lima::Subaccount, _>>()
        .unwrap();

    lima::Account::new(
        lima::AccountType::from_str(account_type_name).unwrap(),
        subaccount,
    )
}

impl<'a> ExpectEq<&Amount> for lima::Amount<'a> {
    fn expect_eq(&self, expected: &Amount, ctx: Rc<Context>) {
        self.number().value().expect_eq(
            expected.number.as_ref(),
            with_context(ctx.clone(), "number"),
        );
        // self.number().expr().expect_eq(expected.expr);
        self.currency().item().expect_eq(
            expected.currency.as_ref(),
            with_context(ctx.clone(), "currency"),
        );
    }
}

// TODO may not need this
// impl<'a> ExpectEq<Option<&Amount>> for Option<&lima::Amount<'a>> {
//     fn expect_eq(&self, expected: Option<&Amount>, ctx: Rc<Context>) {
//         match (self, expected) {
//             (Some(amount), Some(expected)) => amount.expect_eq(expected, ctx.clone()),
//             (Some(_), None) => panic!("expected nothing found amount {}", &ctx),
//             (None, Some(_)) => panic!("expected amount found nothing {}", &ctx),
//             (None, None) => (),
//         }
//     }
// }

impl<'a> ExpectEq<Option<&String>> for &lima::Currency<'a> {
    fn expect_eq(&self, expected: Option<&String>, ctx: Rc<Context>) {
        match expected {
            Some(expected) => assert_eq!(self.as_ref(), expected, "{}", &ctx),
            None => panic!("missing currency from expected {}", &ctx),
        }
    }
}

impl ExpectEq<Option<&Vec<u8>>> for lima::Flag {
    fn expect_eq(&self, expected: Option<&Vec<u8>>, ctx: Rc<Context>) {
        match expected {
            Some(expected) => match bytes_to_flag(expected) {
                Ok(expected_flag) => assert_eq!(*self, expected_flag, "{}", &ctx),
                Err(e) => panic!("{}", e),
            },
            None => panic!("expected nothing found flag {}", &ctx),
        }
    }
}

// TODO remove
// impl ExpectEq<Option<&Vec<u8>>> for Option<&lima::Flag> {
//     fn expect_eq(&self, expected: Option<&Vec<u8>>, ctx: Rc<Context>) {
//         match (self, expected) {
//             (Some(flag), Some(expected)) => {
//                 assert!(
//                     expected.len() == 1,
//                     "expected flag value must be one character"
//                 );
//                 match bytes_to_flag(expected) {
//                     Ok(expected_flag) => assert_eq!(**flag, expected_flag, "{}", &ctx),
//                     Err(e) => panic!("{}", e),
//                 }
//             }
//             (Some(_), None) => panic!("expected nothing found flag {}", &ctx),
//             (None, Some(_)) => panic!("expected flag found nothing {}", &ctx),
//             (None, None) => (),
//         }
//     }
// }

#[derive(Display, Debug)]
struct BytesToFlagError(&'static str);

impl std::error::Error for BytesToFlagError {}

fn bytes_to_flag(bytes: &[u8]) -> Result<lima::Flag, BytesToFlagError> {
    if bytes.len() != 1 {
        Err(BytesToFlagError(
            "expected flag value must be one character",
        ))
    } else {
        // TODO
        Ok(lima::Flag::Exclamation)
    }
}

impl ExpectEq<&Date> for time::Date {
    fn expect_eq(&self, expected: &Date, ctx: Rc<Context>) {
        assert_eq!(
            self.year(),
            expected.year.unwrap(),
            "{}",
            with_context(ctx.clone(), "year")
        );
        assert_eq!(
            self.month() as i32,
            expected.month.unwrap(),
            "{}",
            with_context(ctx.clone(), "month")
        );
        assert_eq!(
            self.day() as i32,
            expected.day.unwrap(),
            "{}",
            with_context(ctx.clone(), "day")
        );
    }
}

impl ExpectEq<Option<&Number>> for Decimal {
    fn expect_eq(&self, expected: Option<&Number>, ctx: Rc<Context>) {
        match expected {
            Some(expected) => assert_eq!(self, &number_to_decimal(expected), "{}", &ctx),
            None => panic!("expected nothing found value {}", &ctx),
        }
    }
}

fn number_to_decimal(number: &Number) -> Decimal {
    Decimal::from_str_exact(number.exact.as_ref().unwrap().as_str()).unwrap()
}

mod beancount;
