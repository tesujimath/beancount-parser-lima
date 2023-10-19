use super::format::{format, plain};
use super::types::*;
use nonempty::NonEmpty;
use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct BeancountOption<'a> {
    source: Source,
    variant: BeancountOptionVariant<'a>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum BeancountOptionVariant<'a> {
    Title(&'a str),
    AccountTypeName(AccountType, AccountTypeName<'a>),
    AccountPreviousBalances(NonEmpty<AccountName<'a>>),
    AccountPreviousEarnings(NonEmpty<AccountName<'a>>),
    AccountPreviousConversions(NonEmpty<AccountName<'a>>),
    AccountCurrentEarnings(NonEmpty<AccountName<'a>>),
    AccountCurrentConversions(NonEmpty<AccountName<'a>>),
    AccountUnrealizedGains(NonEmpty<AccountName<'a>>),
    AccountRounding(NonEmpty<AccountName<'a>>),
    ConversionCurrency(Currency<'a>),
    Assimilated,
}

impl<'a> BeancountOption<'a> {
    pub(crate) fn parse(
        name: Spanned<&'a str>,
        value: Spanned<&'a str>,
    ) -> Result<BeancountOption<'a>, BeancountOptionError> {
        use BeancountOptionError::*;
        use BeancountOptionVariant::*;

        match name.item {
            "title" => Ok(Title(value.item)),

            "name_assets" => {
                parse_account_type_name(value.item).map(|n| AccountTypeName(AccountType::Assets, n))
            }

            "name_liabilities" => parse_account_type_name(value.item)
                .map(|n| AccountTypeName(AccountType::Liabilities, n)),

            "name_equity" => {
                parse_account_type_name(value.item).map(|n| AccountTypeName(AccountType::Equity, n))
            }

            "name_income" => {
                parse_account_type_name(value.item).map(|n| AccountTypeName(AccountType::Income, n))
            }

            "name_expenses" => parse_account_type_name(value.item)
                .map(|n| AccountTypeName(AccountType::Expenses, n)),

            "account_previous_balances" => {
                parse_account_name(value.item).map(AccountPreviousBalances)
            }

            "account_previous_earnings" => {
                parse_account_name(value.item).map(AccountPreviousEarnings)
            }

            "account_previous_conversions" => {
                parse_account_name(value.item).map(AccountPreviousConversions)
            }

            "account_current_earnings" => {
                parse_account_name(value.item).map(AccountCurrentEarnings)
            }

            "account_current_conversions" => {
                parse_account_name(value.item).map(AccountCurrentConversions)
            }

            "account_unrealized_gains" => {
                parse_account_name(value.item).map(AccountUnrealizedGains)
            }

            "account_rounding" => parse_account_name(value.item).map(AccountRounding),

            "conversion_currency" => parse_currency(value.item).map(ConversionCurrency),

            _ => Err(UnknownOption),
        }
        .map(|variant| BeancountOption {
            source: Source {
                name: name.span,
                value: value.span,
            },
            variant,
        })
    }
}

fn parse_account_name(
    colon_separated: &str,
) -> Result<NonEmpty<AccountName>, BeancountOptionError> {
    let mut account = colon_separated.split(':');
    let account_name_components = account
        .by_ref()
        .map(AccountName::try_from)
        .collect::<Result<Vec<AccountName>, _>>()
        .map_err(|e| BadValueErrorKind::AccountName(e).wrap())?;

    Ok(NonEmpty::collect(account_name_components).unwrap())
}

fn parse_account_type_name(value: &str) -> Result<AccountTypeName, BeancountOptionError> {
    AccountTypeName::try_from(value).map_err(|e| BadValueErrorKind::AccountTypeName(e).wrap())
}

fn parse_currency(value: &str) -> Result<Currency, BeancountOptionError> {
    Currency::try_from(value).map_err(|e| BadValueErrorKind::Currency(e).wrap())
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum BeancountOptionError {
    UnknownOption,
    BadValue(BadValueError),
}

impl Display for BeancountOptionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use BeancountOptionError::*;

        match &self {
            UnknownOption => f.write_str("unknown option"),
            BadValue(BadValueError(e)) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for BeancountOptionError {}

#[derive(PartialEq, Eq, Debug)]
pub struct BadValueError(BadValueErrorKind);

#[derive(PartialEq, Eq, Debug)]
enum BadValueErrorKind {
    AccountTypeName(AccountTypeNameError),
    AccountTypeNames(AccountTypeNamesError),
    AccountName(AccountNameError),
    Currency(CurrencyError),
}

impl Display for BadValueErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use BadValueErrorKind::*;

        match &self {
            AccountTypeName(e) => write!(f, "{}", e),
            AccountTypeNames(e) => write!(f, "{}", e),
            AccountName(e) => write!(f, "{}", e),
            Currency(e) => write!(f, "{}", e),
        }
    }
}

impl BadValueErrorKind {
    fn wrap(self) -> BeancountOptionError {
        BeancountOptionError::BadValue(BadValueError(self))
    }
}

#[derive(Default, Debug)]
/// ParserOptions are only those which affect the core parsing.
pub(crate) struct ParserOptions<'a> {
    pub(crate) account_type_names: AccountTypeNames<'a>,
}

impl<'a> ParserOptions<'a> {
    pub(crate) fn assimilate(
        &mut self,
        opt: BeancountOption<'a>,
    ) -> Result<BeancountOption<'a>, ParserOptionsError> {
        use BeancountOptionVariant::*;

        // let mut variant = Assimilated;
        // swap(&mut opt.variant, &mut variant);

        match opt {
            BeancountOption {
                source,
                variant: AccountTypeName(account_type, account_type_name),
            } => self
                .account_type_names
                .update(account_type, account_type_name)
                .map_err(ParserOptionsError)
                .map(|_| BeancountOption {
                    source,
                    variant: Assimilated,
                }),
            _ => Ok(opt),
        }
    }

    pub fn account_type_name(&self, account_type: AccountType) -> AccountTypeName {
        self.account_type_names.name_by_type[account_type as usize]
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct ParserOptionsError(AccountTypeNamesError);

impl Display for ParserOptionsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for ParserOptionsError {}

#[derive(Debug)]
pub struct Options<'a> {
    title: Sourced<&'a str>,
    account_previous_balances: Sourced<NonEmpty<AccountName<'a>>>,
    account_previous_earnings: Sourced<NonEmpty<AccountName<'a>>>,
    account_previous_conversions: Sourced<NonEmpty<AccountName<'a>>>,
    account_current_earnings: Sourced<NonEmpty<AccountName<'a>>>,
    account_current_conversions: Sourced<NonEmpty<AccountName<'a>>>,
    account_unrealized_gains: Sourced<NonEmpty<AccountName<'a>>>,
    account_rounding: Option<Sourced<NonEmpty<AccountName<'a>>>>,
    conversion_currency: Sourced<Currency<'a>>,
    parser_options: ParserOptions<'a>,
}

impl<'a> Options<'a> {
    pub(crate) fn new(parser_options: ParserOptions<'a>) -> Self {
        Options {
            title: unsourced("Beancount"),
            account_previous_balances: unsourced(parse_account_name("Opening-Balances").unwrap()),
            account_previous_earnings: unsourced(parse_account_name("Earnings:Previous").unwrap()),
            account_previous_conversions: unsourced(
                parse_account_name("Conversions:Previous").unwrap(),
            ),
            account_current_earnings: unsourced(parse_account_name("Earnings:Current").unwrap()),
            account_current_conversions: unsourced(
                parse_account_name("Conversions:Current").unwrap(),
            ),
            account_unrealized_gains: unsourced(parse_account_name("Earnings:Unrealized").unwrap()),
            account_rounding: None,
            conversion_currency: unsourced(Currency::try_from("NOTHING").unwrap()),
            parser_options,
        }
    }

    pub(crate) fn assimilate(&mut self, opt: BeancountOption<'a>) -> Result<(), Error> {
        use BeancountOptionVariant::*;
        use OptionError::*;

        let BeancountOption { source, variant } = opt;
        match variant {
            Title(value) => Self::update_string(&mut self.title, value, source),

            // this one was already assimilated into ParserOptions
            AccountTypeName(_, _) => Ok(()),

            AccountPreviousBalances(value) => {
                Self::update_account_name(&mut self.account_previous_balances, value, source)
            }
            AccountPreviousEarnings(value) => {
                Self::update_account_name(&mut self.account_previous_earnings, value, source)
            }
            AccountPreviousConversions(value) => {
                Self::update_account_name(&mut self.account_previous_conversions, value, source)
            }

            AccountCurrentEarnings(value) => {
                Self::update_account_name(&mut self.account_current_earnings, value, source)
            }

            AccountCurrentConversions(value) => {
                Self::update_account_name(&mut self.account_current_conversions, value, source)
            }

            AccountUnrealizedGains(value) => {
                Self::update_account_name(&mut self.account_unrealized_gains, value, source)
            }

            AccountRounding(value) => {
                Self::update_optional_account_name(&mut self.account_rounding, value, source)
            }

            ConversionCurrency(value) => {
                Self::update_currency(&mut self.conversion_currency, value, source)
            }

            // this value contains nothing
            Assimilated => Ok(()),
        }
        .map_err(|ref e| match e {
            DuplicateOption(span) => Error::new("invalid option", "duplicate", source.name)
                .related_to_named_span("option", *span),
            DuplicateValue(span) => Error::new("invalid option", "duplicate value", source.value)
                .related_to_named_span("option", *span),
        })
    }

    fn update_string(
        field: &mut Sourced<&'a str>,
        value: &'a str,
        source: Source,
    ) -> Result<(), OptionError> {
        *field = sourced(value, source);
        Ok(())
    }

    // a colon-separated account name not including the account type
    fn update_account_name(
        field: &mut Sourced<NonEmpty<AccountName<'a>>>,
        value: NonEmpty<AccountName<'a>>,
        source: Source,
    ) -> Result<(), OptionError> {
        use OptionError::*;

        match &field.source {
            None => {
                *field = sourced(value, source);
                Ok(())
            }
            Some(source) => Err(DuplicateOption(source.name)),
        }
    }

    // a colon-separated account name not including the account type
    fn update_optional_account_name(
        field: &mut Option<Sourced<NonEmpty<AccountName<'a>>>>,
        value: NonEmpty<AccountName<'a>>,
        source: Source,
    ) -> Result<(), OptionError> {
        use OptionError::*;

        match field {
            None => {
                *field = Some(sourced(value, source));
                Ok(())
            }
            Some(Sourced {
                source: Some(source),
                ..
            }) => Err(DuplicateOption(source.name)),
            Some(Sourced {
                source: None, // TODO this one is not possible, bah!
                ..
            }) => Err(DuplicateOption(source.name)),
        }
    }

    fn update_currency(
        field: &mut Sourced<Currency<'a>>,
        value: Currency<'a>,
        source: Source,
    ) -> Result<(), OptionError> {
        use OptionError::*;

        match &field.source {
            None => {
                *field = sourced(value, source);
                Ok(())
            }
            Some(source) => Err(DuplicateOption(source.name)),
        }
    }

    // pub fn title(&self) -> &str {
    //     self.title.value
    // }

    pub fn account_type_name(&self, account_type: AccountType) -> AccountTypeName {
        self.parser_options.account_type_name(account_type)
    }
}

#[derive(Debug)]
struct Sourced<T> {
    value: T,
    source: Option<Source>,
}

fn unsourced<T>(value: T) -> Sourced<T> {
    Sourced {
        value,
        source: None,
    }
}

fn sourced<T>(value: T, source: Source) -> Sourced<T> {
    Sourced {
        value,
        source: Some(source),
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
struct Source {
    name: Span,
    value: Span,
}

#[derive(PartialEq, Eq, Debug)]
pub enum OptionError {
    DuplicateOption(Span),
    DuplicateValue(Span),
}

impl OptionError {
    pub(crate) fn span(&self) -> Span {
        use OptionError::*;

        match self {
            DuplicateOption(span) => *span,
            DuplicateValue(span) => *span,
        }
    }
}

impl Display for OptionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use OptionError::*;

        match &self {
            DuplicateOption(_) => f.write_str("duplicate option"),
            DuplicateValue(_) => f.write_str("duplicate value"),
        }
    }
}

impl std::error::Error for OptionError {}

#[derive(Debug)]
pub(crate) struct AccountTypeNames<'a> {
    // relies on AccountType discriminants being contiguous and from zero, which they are
    name_by_type: Vec<AccountTypeName<'a>>,
    type_by_name: HashMap<AccountTypeName<'a>, AccountType>,
}

impl<'a> AccountTypeNames<'a> {
    pub(crate) fn name(&self, account_type: AccountType) -> AccountTypeName<'a> {
        self.name_by_type[account_type as usize]
    }

    pub(crate) fn get(&self, name: &AccountTypeName) -> Option<AccountType> {
        self.type_by_name.get(name).copied()
    }

    pub(crate) fn update(
        &mut self,
        account_type: AccountType,
        name: AccountTypeName<'a>,
    ) -> Result<(), AccountTypeNamesError> {
        use std::collections::hash_map::Entry::*;

        match self.type_by_name.entry(name) {
            Vacant(e) => {
                e.insert(account_type);
                let old_name = self.name_by_type[account_type as usize];
                self.name_by_type[account_type as usize] = name;
                self.type_by_name.remove(&old_name);
                Ok(())
            }
            Occupied(o) => {
                let existing_account_type = *o.get();
                if existing_account_type == account_type {
                    // updating as same, harmless
                    Ok(())
                } else {
                    Err(AccountTypeNamesError(AccountTypeNamesErrorKind::NameInUse(
                        existing_account_type,
                    )))
                }
            }
        }
    }
}

impl<'a> Default for AccountTypeNames<'a> {
    fn default() -> Self {
        use AccountType::*;

        let names_types = vec![
            ("Assets", Assets),
            ("Liabilities", Liabilities),
            ("Equity", Equity),
            ("Income", Income),
            ("Expenses", Expenses),
        ];

        let mut names_type_indices = names_types
            .iter()
            .map(|(n, t)| (*n, *t as usize))
            .collect::<Vec<_>>();
        names_type_indices.sort_by_key(|(_n, t)| *t);

        let name_by_type = names_type_indices
            .into_iter()
            .map(|(n, _t)| AccountTypeName::try_from(n).unwrap())
            .collect::<Vec<_>>();

        let type_by_name: HashMap<AccountTypeName<'a>, AccountType> = HashMap::from_iter(
            names_types
                .into_iter()
                .map(|(n, t)| (AccountTypeName::try_from(n).unwrap(), t)),
        );

        AccountTypeNames {
            name_by_type,
            type_by_name,
        }
    }
}

impl<'a> Display for AccountTypeNames<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format(f, &self.name_by_type, plain, ", ", None)
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct AccountTypeNamesError(AccountTypeNamesErrorKind);

#[derive(PartialEq, Eq, Debug)]
enum AccountTypeNamesErrorKind {
    NameInUse(AccountType),
}

impl Display for AccountTypeNamesError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AccountTypeNamesErrorKind::*;
        match &self.0 {
            NameInUse(t) => write!(f, "account type name in use for {}", t),
        }
    }
}

impl std::error::Error for AccountTypeNamesError {}
