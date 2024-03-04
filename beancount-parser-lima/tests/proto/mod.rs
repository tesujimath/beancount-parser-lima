use self::beancount::{
    data::{Amount, Balance, Directive, Posting, Transaction},
    date::Date,
    ledger::Ledger,
    number::Number,
};
use ::beancount_parser_lima as lima;
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
                actual.expect_eq(expected, context(format!("directives[{}]", i)));
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

pub fn check_parse<S>(test_name: S)
where
    S: AsRef<str>,
{
    let cargo_manifest_dir: PathBuf = env::var("CARGO_MANIFEST_DIR").unwrap().into();
    let testcase_dir = cargo_manifest_dir.join(["..", "test-cases"].iter().collect::<PathBuf>());
    let input_file: PathBuf = Into::<PathBuf>::into(format!("{}.beancount", test_name.as_ref()));
    let input_path = testcase_dir.join(input_file);
    let expected_output_file: PathBuf =
        Into::<PathBuf>::into(format!("{}.txtpb", test_name.as_ref()));
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

#[derive(Clone, Debug)]
struct ContextChain {
    label: String,
    parent: Option<Context>,
}

#[derive(Clone, Debug)]
struct Context(Rc<ContextChain>);

fn context<S>(label: S) -> Context
where
    S: AsRef<str>,
{
    Context(Rc::new(ContextChain {
        label: label.as_ref().to_owned(),
        parent: None,
    }))
}

impl Context {
    fn with<S>(&self, label: S) -> Context
    where
        S: AsRef<str>,
    {
        Context(Rc::new(ContextChain {
            label: label.as_ref().to_owned(),
            parent: Some(Context(self.0.clone())),
        }))
    }
}

impl Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(parent) = &self.0.parent {
            write!(f, "{}.{}", parent, self.0.label)
        } else {
            write!(f, "{}", self.0.label)
        }
    }
}

trait ExpectEq<Rhs> {
    fn expect_eq(&self, expected: Rhs, ctx: Context);
}

impl<E, T> ExpectEq<Option<E>> for Option<T>
where
    T: ExpectEq<Option<E>>,
{
    fn expect_eq(&self, expected: Option<E>, ctx: Context) {
        match (self, expected) {
            (Some(actual), Some(expected)) => actual.expect_eq(Some(expected), ctx),
            (Some(_), None) => panic!("expected nothing found value at {}", &ctx),
            (None, Some(_)) => panic!("expected value found nothing at {}", &ctx),
            (None, None) => (),
        }
    }
}

impl<'a> ExpectEq<&Directive> for lima::Directive<'a> {
    fn expect_eq(&self, expected: &Directive, ctx: Context) {
        self.date().expect_eq(&expected.date, ctx.with("date"));

        match self.variant() {
            lima::DirectiveVariant::Transaction(variant) if expected.has_transaction() => {
                variant.expect_eq(expected.transaction(), ctx.with("transaction"));
            }

            lima::DirectiveVariant::Balance(variant) if expected.has_balance() => {
                variant.expect_eq(expected.balance(), ctx.with("balance"));
            }

            _ => panic!(
                "mismatched directive variant: got {}, expected {:?} {}",
                self, &expected, &ctx
            ),
        }
    }
}

impl<'a> ExpectEq<&Transaction> for lima::Transaction<'a> {
    fn expect_eq(&self, expected: &Transaction, ctx: Context) {
        self.flag()
            .item()
            .expect_eq(expected.flag.as_ref(), ctx.with("flag"));
        self.payee()
            .item()
            .copied()
            .expect_eq(expected.payee.as_ref(), ctx.with("payee"));
        self.narration()
            .item()
            .copied()
            .expect_eq(expected.narration.as_ref(), ctx.with("narration"));
        self.postings()
            .map(|posting| posting.item())
            .collect::<Vec<_>>()
            .expect_eq(&expected.postings, ctx.clone());
    }
}

impl<'a> ExpectEq<&Balance> for lima::Balance<'a> {
    fn expect_eq(&self, expected: &Balance, ctx: Context) {
        self.account()
            .expect_eq(&expected.account, ctx.with("account"));
        self.atol()
            .amount()
            .expect_eq(&expected.amount, ctx.with("amount"));
        self.atol()
            .tolerance()
            .item()
            .copied()
            .expect_eq(expected.tolerance.as_ref(), ctx.with("tolerance"));
    }
}

impl<'a> ExpectEq<&Posting> for lima::Posting<'a> {
    fn expect_eq(&self, expected: &Posting, ctx: Context) {
        self.flag()
            .item()
            .copied()
            .expect_eq(expected.flag.as_ref(), ctx.with("flag"));
        self.account()
            .expect_eq(&expected.account, ctx.with("account"));
        self.amount()
            .item()
            .map(|expr| expr.value())
            .expect_eq(expected.spec.units.number.as_ref(), ctx.with("amount"));
        self.currency()
            .item()
            .expect_eq(expected.spec.units.currency.as_ref(), ctx.with("currency"));
        // TODO
        // self.cost_spec().is(cost_spec);
        // self.price_annotation().is(price_annotation);
        // self.metadata().is(metadata);
    }
}

impl<'a> ExpectEq<&Vec<Posting>> for Vec<&'a lima::Posting<'a>> {
    fn expect_eq(&self, expected: &Vec<Posting>, ctx: Context) {
        assert_eq!(self.len(), expected.len(), "postings.len");
        for (i, (actual, expected)) in self.iter().zip(expected.iter()).enumerate() {
            actual.expect_eq(expected, ctx.with(format!("postings[{}]", i)))
        }
    }
}

impl<'a> ExpectEq<&Option<String>> for lima::Account<'a> {
    fn expect_eq(&self, expected: &Option<String>, ctx: Context) {
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
    fn expect_eq(&self, expected: &Amount, ctx: Context) {
        self.number()
            .value()
            .expect_eq(expected.number.as_ref(), ctx.with("number"));
        // self.number().expr().expect_eq(expected.expr);
        self.currency()
            .item()
            .expect_eq(expected.currency.as_ref(), ctx.with("currency"));
    }
}

impl<'a> ExpectEq<Option<&String>> for &lima::Currency<'a> {
    fn expect_eq(&self, expected: Option<&String>, ctx: Context) {
        match expected {
            Some(expected) => assert_eq!(self.as_ref(), expected, "{}", &ctx),
            None => panic!("missing currency from expected {}", &ctx),
        }
    }
}

impl ExpectEq<Option<&String>> for &str {
    fn expect_eq(&self, expected: Option<&String>, ctx: Context) {
        match expected {
            Some(expected) => assert_eq!(*self, expected.as_str(), "{}", &ctx),
            None => panic!("missing currency from expected {}", &ctx),
        }
    }
}
impl ExpectEq<Option<&Vec<u8>>> for lima::Flag {
    fn expect_eq(&self, expected: Option<&Vec<u8>>, ctx: Context) {
        match expected {
            Some(expected) => match bytes_to_flag(expected) {
                Ok(expected_flag) => assert_eq!(*self, expected_flag, "{}", &ctx),
                Err(e) => panic!("{}", e),
            },
            None => panic!("expected nothing found value at {}", &ctx),
        }
    }
}

#[derive(Display, Debug)]
struct BytesToFlagError(String);

impl std::error::Error for BytesToFlagError {}

fn bytes_to_flag(bytes: &[u8]) -> Result<lima::Flag, BytesToFlagError> {
    if bytes.len() != 1 {
        Err(BytesToFlagError(
            "expected flag value must be one character".to_string(),
        ))
    } else {
        match bytes[0] as char {
            '*' => Ok(lima::Flag::Asterisk),
            '!' => Ok(lima::Flag::Exclamation),
            '&' => Ok(lima::Flag::Ampersand),
            '#' => Ok(lima::Flag::Hash),
            '?' => Ok(lima::Flag::Question),
            '%' => Ok(lima::Flag::Percent),
            c => lima::FlagLetter::try_from(c)
                .map(lima::Flag::Letter)
                .map_err(|e| BytesToFlagError(e.to_string())),
        }
    }
}

impl ExpectEq<&Date> for time::Date {
    fn expect_eq(&self, expected: &Date, ctx: Context) {
        assert_eq!(self.year(), expected.year.unwrap(), "{}", ctx.with("year"));
        assert_eq!(
            self.month() as i32,
            expected.month.unwrap(),
            "{}",
            ctx.with("month")
        );
        assert_eq!(
            self.day() as i32,
            expected.day.unwrap(),
            "{}",
            ctx.with("day")
        );
    }
}

impl ExpectEq<Option<&Number>> for Decimal {
    fn expect_eq(&self, expected: Option<&Number>, ctx: Context) {
        match expected {
            Some(expected) => assert_eq!(self, &number_to_decimal(expected), "{}", &ctx),
            None => panic!("expected nothing found value at {}", &ctx),
        }
    }
}

fn number_to_decimal(number: &Number) -> Decimal {
    Decimal::from_str_exact(number.exact.as_ref().unwrap().as_str()).unwrap()
}

mod beancount;
