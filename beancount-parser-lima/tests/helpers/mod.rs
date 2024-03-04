use self::beancount::{
    data::{Amount, Balance, Directive, Error, Posting, Transaction},
    date::Date,
    inter::CostSpec,
    ledger::Ledger,
    number::Number,
};
use ::beancount_parser_lima as lima;
use derive_more::Display;
use lima::{BeancountParser, BeancountSources, OptionalItem, ParseError, ParseSuccess};
use rust_decimal::Decimal;
use std::{
    env,
    fmt::Display,
    fs::read_to_string,
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

fn check(
    sources: &BeancountSources,
    parser: &BeancountParser,
    expected_directives: Vec<Directive>,
    expected_errors: Vec<Error>,
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
            if n_errors == expected_errors.len() {
                for (i, (actual, expected)) in errors.iter().zip(expected_errors.iter()).enumerate()
                {
                    actual.expect_eq(expected, context(format!("errors[{}]", i)));
                }
            } else {
                sources.write(stderr, errors).unwrap();
                panic!("parse failed with {} errors", n_errors);
            }
        }
    }
}

fn create_sources_and_check<P>(
    input_path: P,
    expected_directives: Vec<Directive>,
    expected_errors: Vec<Error>,
) where
    P: AsRef<Path>,
{
    let sources = BeancountSources::from(input_path.as_ref());
    let parser = BeancountParser::new(&sources);

    check(&sources, &parser, expected_directives, expected_errors);
}

pub fn check_parse<S>(test_name: S)
where
    S: AsRef<str>,
{
    let cargo_manifest_dir: PathBuf = env::var("CARGO_MANIFEST_DIR").unwrap().into();
    // unwrap here is safe because we know the repo structure, so there definitely is a parent
    let testcase_dir = cargo_manifest_dir.parent().unwrap().join("test-cases");
    let input_file: PathBuf = Into::<PathBuf>::into(format!("{}.beancount", test_name.as_ref()));
    let input_path = testcase_dir.join(input_file);
    let expected_output_file: PathBuf =
        Into::<PathBuf>::into(format!("{}.txtpb", test_name.as_ref()));
    let expected_output_path = testcase_dir.join(expected_output_file);

    let expected_output = read_to_string(&expected_output_path).unwrap_or_else(|_| {
        panic!(
            "failed to read expected output from {:?}",
            &expected_output_path
        )
    });
    let expected_output_ledger: Ledger =
        protobuf::text_format::parse_from_str(&expected_output).unwrap();

    create_sources_and_check(
        input_path,
        expected_output_ledger.directives,
        expected_output_ledger.errors,
    );
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
    fn expect_eq(&self, expected: &Rhs, ctx: Context);
}

impl<'r, 't, Rhs, T> ExpectEq<Option<&'r Rhs>> for Option<&'t T>
where
    T: ExpectEq<Rhs>,
{
    fn expect_eq(&self, expected: &Option<&Rhs>, ctx: Context) {
        match (self, expected) {
            (Some(actual), Some(expected)) => (*actual).expect_eq(*expected, ctx),
            (Some(_), None) => panic!("expected nothing found value at {}", &ctx),
            (None, Some(_)) => panic!("expected value found nothing at {}", &ctx),
            (None, None) => (),
        }
    }
}

trait ExpectEqUnwrapped<Rhs> {
    fn expect_eq_unwrapped(&self, expected: Option<&Rhs>, ctx: Context);
}

impl<Rhs, T> ExpectEqUnwrapped<Rhs> for T
where
    T: ExpectEq<Rhs>,
{
    fn expect_eq_unwrapped(&self, expected: Option<&Rhs>, ctx: Context) {
        match expected {
            Some(expected) => self.expect_eq(expected, ctx),
            None => panic!("expected nothing found value at {}", &ctx),
        }
    }
}

impl<'a> ExpectEq<Directive> for lima::Directive<'a> {
    fn expect_eq(&self, expected: &Directive, ctx: Context) {
        self.date()
            .expect_eq_unwrapped(expected.date.as_ref(), ctx.with("date"));

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

impl ExpectEq<Error> for lima::Error {
    fn expect_eq(&self, expected: &Error, ctx: Context) {
        self.message()
            .expect_eq_unwrapped(expected.message.as_ref(), ctx.with("message"));
    }
}

impl<'a> ExpectEq<Transaction> for lima::Transaction<'a> {
    fn expect_eq(&self, expected: &Transaction, ctx: Context) {
        self.flag()
            .item()
            .expect_eq_unwrapped(expected.flag.as_ref(), ctx.with("flag"));
        self.payee()
            .item()
            .expect_eq(&expected.payee.as_ref(), ctx.with("payee"));
        self.narration()
            .item()
            .expect_eq(&expected.narration.as_ref(), ctx.with("narration"));
        self.postings()
            .map(|posting| posting.item())
            .collect::<Vec<_>>()
            .expect_eq(&expected.postings, ctx.clone());
    }
}

impl<'a> ExpectEq<Balance> for lima::Balance<'a> {
    fn expect_eq(&self, expected: &Balance, ctx: Context) {
        self.account()
            .expect_eq_unwrapped(expected.account.as_ref(), ctx.with("account"));
        self.atol()
            .amount()
            .expect_eq(&expected.amount, ctx.with("amount"));
        self.atol()
            .tolerance()
            .item()
            .expect_eq(&expected.tolerance.as_ref(), ctx.with("tolerance"));
    }
}

impl<'a> ExpectEq<Posting> for lima::Posting<'a> {
    fn expect_eq(&self, expected: &Posting, ctx: Context) {
        self.flag()
            .item()
            .expect_eq(&expected.flag.as_ref(), ctx.with("flag"));
        self.account()
            .expect_eq_unwrapped(expected.account.as_ref(), ctx.with("account"));
        self.amount()
            .item()
            .map(|expr| expr.value())
            .as_ref()
            .expect_eq(&expected.spec.units.number.as_ref(), ctx.with("amount"));
        self.currency()
            .item()
            .as_ref()
            .expect_eq(&expected.spec.units.currency.as_ref(), ctx.with("currency"));
        self.cost_spec()
            .item()
            .expect_eq(&expected.spec.cost.as_ref(), ctx.with("cost"));
        // TODO
        // self.price_annotation().is(price_annotation);
        // self.metadata().is(metadata);
    }
}

impl<'a> ExpectEq<Vec<Posting>> for Vec<&'a lima::Posting<'a>> {
    fn expect_eq(&self, expected: &Vec<Posting>, ctx: Context) {
        assert_eq!(self.len(), expected.len(), "postings.len");
        for (i, (actual, expected)) in self.iter().zip(expected.iter()).enumerate() {
            actual.expect_eq(expected, ctx.with(format!("postings[{}]", i)))
        }
    }
}

impl<'a> ExpectEq<CostSpec> for lima::CostSpec<'a> {
    fn expect_eq(&self, expected: &CostSpec, ctx: Context) {
        // TODO
        // per_unit: Option<Spanned<ExprValue>>,
        // total: Option<Spanned<ExprValue>>,
        // currency: Option<Spanned<Currency<'a>>>,
        // date: Option<Spanned<Date>>,
        // label: Option<Spanned<&'a str>>,
        // merge: bool,
    }
}

impl<'a, S> ExpectEq<S> for lima::Account<'a>
where
    S: AsRef<str>,
{
    fn expect_eq(&self, expected: &S, ctx: Context) {
        assert_eq!(self, &account(expected.as_ref()), "{}", &ctx);
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

impl<'a> ExpectEq<Amount> for lima::Amount<'a> {
    fn expect_eq(&self, expected: &Amount, ctx: Context) {
        self.number()
            .value()
            .expect_eq_unwrapped(expected.number.as_ref(), ctx.with("number"));
        self.currency()
            .item()
            .expect_eq_unwrapped(expected.currency.as_ref(), ctx.with("currency"));
    }
}

impl<'a, S> ExpectEq<S> for &lima::Currency<'a>
where
    S: AsRef<str>,
{
    fn expect_eq(&self, expected: &S, ctx: Context) {
        assert_eq!(self.as_ref(), expected.as_ref(), "{}", &ctx);
    }
}

impl<S> ExpectEq<S> for &str
where
    S: AsRef<str>,
{
    fn expect_eq(&self, expected: &S, ctx: Context) {
        assert_eq!(*self, expected.as_ref(), "{}", &ctx);
    }
}

impl ExpectEq<Vec<u8>> for lima::Flag {
    fn expect_eq(&self, expected: &Vec<u8>, ctx: Context) {
        match bytes_to_flag(expected) {
            Ok(expected_flag) => assert_eq!(*self, expected_flag, "{}", &ctx),
            Err(e) => panic!("{}", e),
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

impl ExpectEq<Date> for time::Date {
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

impl ExpectEq<Number> for Decimal {
    fn expect_eq(&self, expected: &Number, ctx: Context) {
        assert_eq!(self, &number_to_decimal(expected, ctx.clone()), "{}", &ctx);
    }
}

fn number_to_decimal(number: &Number, ctx: Context) -> Decimal {
    Decimal::from_str_exact(
        number
            .exact
            .as_ref()
            .unwrap_or_else(|| {
                panic!(
                    "expected values for numbers must be exact at {}",
                    &ctx.with("exact")
                )
            })
            .as_str(),
    )
    .unwrap()
}

mod beancount;
