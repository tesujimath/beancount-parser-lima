use std::{
    io::{stderr, Write},
    str::FromStr,
};

use ::beancount_parser_lima as lima;
use lima::{
    Account, AccountName, AccountType, BeancountParser, BeancountSources, Flag, ParseError,
    ParseSuccess, Spanned, Subaccount,
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
                let expected = vec![Directive::new(
                    "2014-01-27",
                    DirectiveVariant::Transaction(
                        Transaction::new(
                            Flag::Asterisk,
                            vec![
                                Posting::new("Liabilities:US:Amex:BlueCash")
                                    .amount(dec!(-22.02))
                                    .currency("USD"),
                                Posting::new("Expenses:Food:Grocery")
                                    .amount(dec!(22.02))
                                    .currency("USD"),
                            ],
                        )
                        .narration("UNION MARKET"),
                    ),
                )];

                assert_eq!(directives.len(), expected.len(), "directives.len()");
                for (actual, expected) in directives.iter().zip(expected.iter()) {
                    actual.expect_eq(expected);
                }
            }
            Err(ParseError { errors, .. }) => {
                sources.write(w, errors).unwrap();
            }
        }
    }

    check(&sources, &parser, &stderr());
}

trait ExpectEq<Rhs>
where
    Rhs: ?Sized,
{
    fn expect_eq(&self, expected: &Rhs);
}

#[derive(Debug)]
struct Directive {
    pub(crate) date: Date,
    pub(crate) metadata: Metadata,
    pub(crate) variant: DirectiveVariant,
}

impl Directive {
    fn new(date: &str, variant: DirectiveVariant) -> Self {
        Directive {
            date: date_from_string(date),
            metadata: Metadata::default(),
            variant,
        }
    }
}

impl<'a> ExpectEq<Directive> for Spanned<lima::Directive<'a>> {
    fn expect_eq(&self, expected: &Directive) {
        use DirectiveVariant::*;

        assert_eq!(self.date().item(), &expected.date);
        match (self.variant(), &expected.variant) {
            (lima::DirectiveVariant::Transaction(variant), Transaction(ref other)) => {
                variant.expect_eq(other);
            }
            _ => panic!("mismatched directive variant"),
        }
    }
}

#[derive(Debug)]
enum DirectiveVariant {
    Transaction(Transaction),
    // Price(Price),
    // Balance(Balance),
    // Open(Open),
    // Close(Close),
    // Commodity(Commodity),
    // Pad(Pad),
    // Document(Document),
    // Note(Note),
    // Event(Event),
    // Query(Query),
}

#[derive(Debug)]
struct Transaction {
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

impl<'a> ExpectEq<Transaction> for lima::Transaction<'a> {
    fn expect_eq(&self, expected: &Transaction) {
        self.flag().expect_eq(&expected.flag);
        self.payee().expect_eq(&expected.payee);
        self.narration().expect_eq(&expected.narration);
        self.postings()
            .collect::<Vec<_>>()
            .expect_eq(&expected.postings);
    }
}

#[derive(Debug)]
struct Price {
    currency: &'static str,
    amount: Amount,
}

impl Price {
    fn new(currency: &'static str, amount: Amount) -> Self {
        Price { currency, amount }
    }
}

#[derive(Debug)]
struct Posting {
    flag: Option<Flag>,
    account: Account<'static>,
    amount: Option<Decimal>,
    currency: Option<&'static str>,
    cost_spec: Option<CostSpec>,
    price_annotation: Option<ScopedAmount>,
    metadata: Metadata,
}

impl Posting {
    fn new(account_str: &'static str) -> Self {
        Posting {
            flag: None,
            account: account(account_str),
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

impl<'a> ExpectEq<Posting> for Spanned<lima::Posting<'a>> {
    fn expect_eq(&self, expected: &Posting) {
        self.flag().expect_eq(&expected.flag);
        self.account().expect_eq(&expected.account);
        self.amount().expect_eq(&expected.amount);
        self.currency().expect_eq(&expected.currency);
        // TODO
        // self.cost_spec().is(cost_spec);
        // self.price_annotation().is(price_annotation);
        // self.metadata().is(metadata);
    }
}

impl<'a> ExpectEq<Vec<Posting>> for Vec<&'a Spanned<lima::Posting<'a>>> {
    fn expect_eq(&self, expected: &Vec<Posting>) {
        assert_eq!(self.len(), expected.len(), "postings.len");
        for (actual, expected) in self.iter().zip(expected.iter()) {
            actual.expect_eq(expected)
        }
    }
}

#[derive(Debug)]
struct Amount {
    number: Decimal,
    currency: &'static str,
}

impl Amount {
    fn new(number: Decimal, currency: &'static str) -> Self {
        Amount { number, currency }
    }
}

impl<'a> ExpectEq<str> for Account<'a> {
    fn expect_eq(&self, expected: &str) {
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
#[derive(Debug)]
struct CostSpec();
#[derive(Debug)]
struct ScopedAmount();

#[derive(Default, Debug)]
struct Metadata();

impl<T, E> ExpectEq<E> for Spanned<T>
where
    T: PartialEq<E> + Debug,
    E: Debug,
{
    fn expect_eq(&self, expected: &E) {
        assert_eq!(self.item(), expected);
    }
}

impl<T, E> ExpectEq<Option<E>> for Option<&Spanned<T>>
where
    T: PartialEq<E> + Debug,
    E: Debug,
{
    fn expect_eq(&self, expected: &Option<E>) {
        match (self, expected) {
            (None, None) => (),
            (Some(actual), Some(ref expected)) => assert_eq!(actual.item(), expected),
            (Some(actual), None) => panic!("expected None got {:?}", actual.item()),
            (None, Some(ref expected)) => panic!("expected {:?} got None", expected),
        }
    }
}

fn date_from_string(s: &str) -> Date {
    time::Date::parse(s, &Iso8601::DEFAULT).unwrap()
}
