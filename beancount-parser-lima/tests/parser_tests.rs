use std::{
    io::{stderr, Write},
    str::FromStr,
};

use ::beancount_parser_lima as lima;
use lima::{
    Account, AccountName, AccountType, BeancountParser, BeancountSources, DirectiveVariant, Flag,
    ParseError, ParseSuccess, Spanned, Subaccount,
};
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::fmt::Debug;
use time::{format_description::well_known::Iso8601, Date};

#[test]
fn parser_basic_testing() {
    const SOURCE: &str = r#"

2014-01-27 * "UNION MARKET"
  Liabilities:US:Amex:BlueCash    -22.02 USD
  Expenses:Food:Grocery            22.02 USD

"#;

    let sources = BeancountSources::from(SOURCE);
    let parser = BeancountParser::new(&sources);

    fn check<W>(sources: &BeancountSources, parser: &BeancountParser, w: W)
    where
        W: Write + Copy,
    {
        match parser.parse() {
            Ok(ParseSuccess { directives, .. }) => {
                use DirectiveVariant::*;

                assert_eq!(directives.len(), 1);
                let d0 = &directives[0];

                // assert_eq!(
                //     *d0.date().item(),
                //     Date::from_calendar_date(2014, Month::January, 27).unwrap()
                // );

                eq_date(d0.date(), date("2014-01-27"));

                if let Transaction(t0) = d0.variant() {
                    eq_flag(t0.flag(), Flag::Asterisk);
                    eq(t0.flag(), Flag::Asterisk);

                    eq_option(t0.payee(), None);

                    // assert_eq!(t0.narration().map(|n| *n.item()), Some("UNION MARKET"));
                    eq_option(t0.narration(), Some("UNION MARKET"));

                    assert_eq!(t0.postings().len(), 2);
                    t0.postings().collect::<Vec<_>>().is(vec![
                        Posting::new("Liabilities:US:Amex:BlueCash")
                            .amount(dec!(-22.02))
                            .currency("USD"),
                        Posting::new("Expenses:Food:Grocery")
                            .amount(dec!(22.02))
                            .currency("USD"),
                    ]);
                }
            }
            Err(ParseError { errors, .. }) => {
                sources.write(w, errors).unwrap();
            }
        }
    }

    check(&sources, &parser, &stderr());
}

trait Checker<E> {
    fn is(self, expected: E);
}

pub struct Transaction {
    flag: Flag,
    payee: Option<&'static str>,
    narration: Option<&'static str>,
    postings: Vec<Posting>,
}

impl Transaction {
    fn new(flag: Flag, postings: Vec<Posting>) -> Self {
        Transaction {
            flag,
            payee: None,
            narration: None,
            postings,
        }
    }

    fn payee(self, x: &'static str) -> Self {
        Transaction {
            payee: Some(x),
            ..self
        }
    }

    fn narration(self, x: &'static str) -> Self {
        Transaction {
            narration: Some(x),
            ..self
        }
    }
}

pub struct Price {
    currency: &'static str,
    amount: Amount,
}

impl Price {
    fn new(currency: &'static str, amount: Amount) -> Self {
        Price { currency, amount }
    }
}

pub struct Posting {
    flag: Option<Flag>,
    account: &'static str,
    amount: Option<Decimal>,
    currency: Option<&'static str>,
    cost_spec: Option<CostSpec>,
    price_annotation: Option<ScopedAmount>,
    metadata: Metadata,
}

impl Posting {
    fn new(account: &'static str) -> Self {
        Posting {
            flag: None,
            account,
            amount: None,
            currency: None,
            cost_spec: None,
            price_annotation: None,
            metadata: Metadata::default(),
        }
    }

    fn flag(self, x: Flag) -> Self {
        Posting {
            flag: Some(x),
            ..self
        }
    }

    fn amount(self, x: Decimal) -> Self {
        Posting {
            amount: Some(x),
            ..self
        }
    }

    fn currency(self, x: &'static str) -> Self {
        Posting {
            currency: Some(x),
            ..self
        }
    }
}

pub struct Amount {
    number: Decimal,
    currency: &'static str,
}

impl Amount {
    fn new(number: Decimal, currency: &'static str) -> Self {
        Amount { number, currency }
    }
}

impl<'a> Checker<Transaction> for &'a lima::Transaction<'a> {
    fn is(self, expected: Transaction) {
        let Transaction {
            flag,
            payee,
            narration,
            postings,
        } = expected;

        self.flag().is(flag);
    }
}

impl<'a> Checker<Posting> for &'a Spanned<lima::Posting<'a>> {
    fn is(self, expected: Posting) {
        let Posting {
            flag,
            account,
            amount,
            currency: cur,
            cost_spec,
            price_annotation,
            metadata,
        } = expected;
        self.flag().is(flag);
        // self.account().is(account);
        self.amount().is(amount);
        self.currency().is(cur.map(|c| currency(c)));
        // TODO
        // self.cost_spec().is(cost_spec);
        // self.price_annotation().is(price_annotation);
        // self.metadata().is(metadata);
    }
}

impl<'a> Checker<Vec<Posting>> for Vec<&'a Spanned<lima::Posting<'a>>> {
    fn is(self, expected: Vec<Posting>) {
        assert_eq!(self.len(), expected.len());

        for (actual, expected) in self.into_iter().zip(expected.into_iter()) {
            actual.is(expected);
        }
    }
}

impl<'a> Checker<&'static str> for &'a Account<'a> {
    fn is(self, expected: &'static str) {
        assert_eq!(self, &account(expected));
    }
}

fn account(s: &str) -> Account {
    let mut account = s.split(':');
    let account_type_name = account.by_ref().next().unwrap();
    let subaccount = account
        .map(AccountName::try_from)
        .collect::<Result<Subaccount, _>>()
        .unwrap();

    Account::new(
        AccountType::from_str(account_type_name).unwrap(),
        subaccount,
    )
}

fn currency(s: &str) -> lima::Currency {
    lima::Currency::try_from(s).unwrap()
}

// TODO
pub struct CostSpec();
pub struct ScopedAmount();

#[derive(Default)]
pub struct Metadata();

fn eq_date(actual: &Date, expected: Date) {
    assert_eq!(*actual, expected);
}

fn eq_flag(actual: &Flag, expected: Flag) {
    assert_eq!(*actual, expected);
}

// TODO this probably isn't what I want to do
fn eq<T>(actual: &Spanned<T>, expected: T)
where
    T: Eq + Debug,
{
    assert_eq!(actual.item(), &expected);
}

impl<T, E> Checker<E> for &Spanned<T>
where
    T: PartialEq<E> + Debug,
    E: Debug,
{
    fn is(self, expected: E) {
        assert_eq!(self.item(), &expected);
    }
}

impl<T, E> Checker<Option<E>> for Option<&Spanned<T>>
where
    T: PartialEq<E> + Debug,
    E: Debug,
{
    fn is(self, expected: Option<E>) {
        match (self, expected) {
            (None, None) => (),
            (Some(actual), Some(ref expected)) => {
                assert_eq!(actual.item(), expected);
            }
            (Some(actual), None) => panic!("expected None got {:?}", actual.item()),
            (None, Some(ref expected)) => panic!("expected {:?} got None", expected),
        }
    }
}

// impl<T> Checker for Option<T>
// where
//     T: Eq + Debug,
// {
//     type Expected = Option<T>;

//     fn is(self, expected: Self::Expected) {
//         assert_eq!(self, expected);
//         // match (self, expected) {
//         //     (None, None) => (),
//         //     (Some(actual), Some(expected)) => {
//         //         assert_eq!(actual, expected);
//         //     }
//         //     (Some(actual), None) => panic!("expected None got {:?}", actual.item()),
//         //     (None, Some(ref expected)) => panic!("expected {:?} got None", expected),
//         // }
//     }
// }

fn eq_option<T>(actual: Option<&Spanned<T>>, expected: Option<T>)
where
    T: Eq + Debug,
{
    match (actual, expected) {
        (None, None) => (),
        (Some(actual), Some(ref expected)) => {
            assert_eq!(actual.item(), expected);
        }
        (Some(actual), None) => panic!("expected None got {:?}", actual.item()),
        (None, Some(ref expected)) => panic!("expected {:?} got None", expected),
    }
}

fn date(s: &str) -> Date {
    time::Date::parse(s, &Iso8601::DEFAULT).unwrap()
}
