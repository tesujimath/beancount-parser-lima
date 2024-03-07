use super::format::{format, plain};
use super::types::*;
use path_clean::PathClean;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;
use std::path::{Path, PathBuf};
use std::{
    collections::{hash_map, HashMap},
    fmt::{self, Display, Formatter},
    hash::Hash,
};
use strum::IntoEnumIterator;

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) struct BeancountOption<'a> {
    source: Source,
    variant: BeancountOptionVariant<'a>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) enum BeancountOptionVariant<'a> {
    Title(&'a str),
    AccountTypeName(AccountType, AccountTypeName<'a>),
    AccountPreviousBalances(Subaccount<'a>),
    AccountPreviousEarnings(Subaccount<'a>),
    AccountPreviousConversions(Subaccount<'a>),
    AccountCurrentEarnings(Subaccount<'a>),
    AccountCurrentConversions(Subaccount<'a>),
    AccountUnrealizedGains(Subaccount<'a>),
    AccountRounding(Subaccount<'a>),
    ConversionCurrency(Currency<'a>),
    InferredToleranceDefault(CurrencyOrAny<'a>, Decimal),
    InferredToleranceMultiplier(Decimal),
    InferToleranceFromCost(bool),
    Documents(PathBuf),
    OperatingCurrency(Currency<'a>),
    RenderCommas(bool),
    LongStringMaxlines(usize),
    BookingMethod(Booking),
    PluginProcessingMode(PluginProcessingMode),
    Assimilated,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub(crate) enum CurrencyOrAny<'a> {
    Currency(Currency<'a>),
    Any,
}

impl<'a> TryFrom<&'a str> for CurrencyOrAny<'a> {
    type Error = CurrencyError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        if s == "*" {
            Ok(CurrencyOrAny::Any)
        } else {
            Currency::try_from(s).map(CurrencyOrAny::Currency)
        }
    }
}

impl<'a> BeancountOption<'a> {
    pub(crate) fn parse(
        name: Spanned<&'a str>,
        value: Spanned<&'a str>,
        source_path: Option<&Path>,
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
                parse_subaccount(value.item).map(AccountPreviousBalances)
            }

            "account_previous_earnings" => {
                parse_subaccount(value.item).map(AccountPreviousEarnings)
            }

            "account_previous_conversions" => {
                parse_subaccount(value.item).map(AccountPreviousConversions)
            }

            "account_current_earnings" => parse_subaccount(value.item).map(AccountCurrentEarnings),

            "account_current_conversions" => {
                parse_subaccount(value.item).map(AccountCurrentConversions)
            }

            "account_unrealized_gains" => parse_subaccount(value.item).map(AccountUnrealizedGains),

            "account_rounding" => parse_subaccount(value.item).map(AccountRounding),

            "conversion_currency" => parse_currency(value.item).map(ConversionCurrency),

            "inferred_tolerance_default" => {
                parse_inferred_tolerance_default(value.item).map(|(currency_or_any, tolerance)| {
                    InferredToleranceDefault(currency_or_any, tolerance)
                })
            }

            "inferred_tolerance_multiplier" => Decimal::try_from(value.item)
                .map(InferredToleranceMultiplier)
                .map_err(|e| BadValueErrorKind::Decimal(e).wrap()),

            "infer_tolerance_from_cost" => parse_bool(value.item).map(InferToleranceFromCost),

            "documents" => Ok(Documents(
                source_path
                    .and_then(|path| path.parent())
                    .map_or(PathBuf::from(value.item), |parent| {
                        parent.join(value.item).clean()
                    }),
            )),

            "operating_currency" => parse_currency(value.item).map(OperatingCurrency),

            // any of case-insensitive true, false, 0, 1
            "render_commas" => parse_bool(value.item)
                .or(parse_zero_or_one_as_bool(value.item))
                .map(RenderCommas),

            "long_string_maxlines" => value
                .item
                .parse::<usize>()
                .map(LongStringMaxlines)
                .map_err(|e| BadValueErrorKind::ParseIntError(e).wrap()),

            "booking_method" => parse_booking(value.item).map(BookingMethod),

            "plugin_processing_mode" => {
                parse_plugin_processing_mode(value.item).map(PluginProcessingMode)
            }

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

fn parse_subaccount(colon_separated: &str) -> Result<Subaccount, BeancountOptionError> {
    colon_separated
        .split(':')
        .map(AccountName::try_from)
        .collect::<Result<Subaccount, _>>()
        .map_err(|e| BadValueErrorKind::AccountName(e).wrap())
}

fn parse_account_type_name(value: &str) -> Result<AccountTypeName, BeancountOptionError> {
    AccountTypeName::try_from(value).map_err(|e| BadValueErrorKind::AccountTypeName(e).wrap())
}

fn parse_currency(value: &str) -> Result<Currency, BeancountOptionError> {
    Currency::try_from(value).map_err(|e| BadValueErrorKind::Currency(e).wrap())
}

fn parse_inferred_tolerance_default(
    value: &str,
) -> Result<(CurrencyOrAny, Decimal), BeancountOptionError> {
    use BadValueErrorKind as Bad;

    let mut fields = value.split(':');
    let currency_or_any = CurrencyOrAny::try_from(fields.by_ref().next().unwrap())
        .map_err(|e| Bad::Currency(e).wrap())?;
    let tolerance = fields
        .by_ref()
        .next()
        .ok_or(Bad::MissingColon)
        .and_then(|s| Decimal::try_from(s).map_err(Bad::Decimal))
        .map_err(Bad::wrap)?;

    if fields.next().is_none() {
        Ok((currency_or_any, tolerance))
    } else {
        Err(Bad::TooManyColons.wrap())
    }
}

fn parse_booking(value: &str) -> Result<Booking, BeancountOptionError> {
    Booking::try_from(value).map_err(|e| BadValueErrorKind::Booking(e).wrap())
}

fn parse_plugin_processing_mode(value: &str) -> Result<PluginProcessingMode, BeancountOptionError> {
    PluginProcessingMode::try_from(value)
        .map_err(|e| BadValueErrorKind::PluginProcessingMode(e).wrap())
}

// case insenstive parsing
fn parse_bool(value: &str) -> Result<bool, BeancountOptionError> {
    if value.eq_ignore_ascii_case("true") {
        Ok(true)
    } else if value.eq_ignore_ascii_case("false") {
        Ok(false)
    } else {
        Err(BadValueErrorKind::Bool.wrap())
    }
}

fn parse_zero_or_one_as_bool(value: &str) -> Result<bool, BeancountOptionError> {
    if value == "1" {
        Ok(true)
    } else if value == "0" {
        Ok(false)
    } else {
        Err(BadValueErrorKind::Bool.wrap())
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub(crate) struct BadValueError(BadValueErrorKind);

#[derive(Debug)]
enum BadValueErrorKind {
    AccountTypeName(AccountTypeNameError),
    AccountTypeNames(AccountTypeNamesError),
    AccountName(AccountNameError),
    Currency(CurrencyError),
    Booking(strum::ParseError),
    PluginProcessingMode(strum::ParseError),
    Decimal(rust_decimal::Error),
    Bool,
    MissingColon,
    TooManyColons,
    ParseIntError(std::num::ParseIntError),
}

impl Display for BadValueErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use BadValueErrorKind::*;

        match &self {
            AccountTypeName(e) => write!(f, "{}", e),
            AccountTypeNames(e) => write!(f, "{}", e),
            AccountName(e) => write!(f, "{}", e),
            Currency(e) => write!(f, "{}", e),
            Booking(_e) => format(
                f,
                crate::types::Booking::iter(),
                plain,
                ", ",
                Some("Expected one of "),
            ),
            PluginProcessingMode(_e) => format(
                f,
                self::PluginProcessingMode::iter(),
                plain,
                ", ",
                Some("Expected one of "),
            ),
            Decimal(e) => write!(f, "{}", e),
            Bool => f.write_str("must be true or false or case-insensitive equivalent"),
            MissingColon => f.write_str("missing colon"),
            TooManyColons => f.write_str("too many colons"),
            ParseIntError(e) => write!(f, "{}", e),
        }
    }
}

impl BadValueErrorKind {
    fn wrap(self) -> BeancountOptionError {
        BeancountOptionError::BadValue(BadValueError(self))
    }
}

#[derive(Debug)]
/// ParserOptions are only those which affect the core parsing.
pub(crate) struct ParserOptions<'a> {
    pub(crate) account_type_names: AccountTypeNames<'a>,
    pub(crate) long_string_maxlines: OptionallySourced<usize>,
}

impl<'a> Default for ParserOptions<'a> {
    fn default() -> Self {
        ParserOptions {
            account_type_names: AccountTypeNames::default(),
            long_string_maxlines: unsourced(64),
        }
    }
}

impl<'a> ParserOptions<'a> {
    pub(crate) fn assimilate(
        &mut self,
        opt: BeancountOption<'a>,
    ) -> Result<BeancountOption<'a>, ParserOptionsError> {
        use BeancountOptionVariant::*;

        let BeancountOption { source, variant } = opt;

        match variant {
            AccountTypeName(account_type, account_type_name) => self
                .account_type_names
                .update(account_type, account_type_name)
                .map_err(ParserOptionsError)
                .map(|_| Assimilated),

            LongStringMaxlines(n) => {
                self.long_string_maxlines = optionally_sourced(n, source);
                Ok(Assimilated)
            }

            _ => Ok(variant),
        }
        .map(|variant| BeancountOption { source, variant })
    }

    pub(crate) fn account_type_name(&self, account_type: AccountType) -> &AccountTypeName {
        &self.account_type_names.name_by_type[account_type as usize]
    }

    pub(crate) fn long_string_maxlines(&self) -> usize {
        self.long_string_maxlines.item
    }
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) struct ParserOptionsError(AccountTypeNamesError);

impl Display for ParserOptionsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for ParserOptionsError {}

/// All options read in from `option` pragmas, excluding those for internal processing only.
#[derive(Debug)]
pub struct Options<'a> {
    title: OptionallySourced<&'a str>,
    account_previous_balances: OptionallySourced<Subaccount<'a>>,
    account_previous_earnings: OptionallySourced<Subaccount<'a>>,
    account_previous_conversions: OptionallySourced<Subaccount<'a>>,
    account_current_earnings: OptionallySourced<Subaccount<'a>>,
    account_current_conversions: OptionallySourced<Subaccount<'a>>,
    account_unrealized_gains: OptionallySourced<Subaccount<'a>>,
    account_rounding: Option<Sourced<Subaccount<'a>>>,
    conversion_currency: OptionallySourced<Currency<'a>>,
    inferred_tolerance_default: HashMap<CurrencyOrAny<'a>, (Decimal, Source)>,
    inferred_tolerance_multiplier: OptionallySourced<Decimal>,
    infer_tolerance_from_cost: OptionallySourced<bool>,
    documents: HashMap<PathBuf, Source>,
    operating_currency: HashMap<Currency<'a>, Source>,
    render_commas: OptionallySourced<bool>,
    booking_method: OptionallySourced<Booking>,
    plugin_processing_mode: OptionallySourced<PluginProcessingMode>,
    parser_options: ParserOptions<'a>,
}

impl<'a> Options<'a> {
    pub(crate) fn new(parser_options: ParserOptions<'a>) -> Self {
        Options {
            title: unsourced("Beancount"),
            account_previous_balances: unsourced(parse_subaccount("Opening-Balances").unwrap()),
            account_previous_earnings: unsourced(parse_subaccount("Earnings:Previous").unwrap()),
            account_previous_conversions: unsourced(
                parse_subaccount("Conversions:Previous").unwrap(),
            ),
            account_current_earnings: unsourced(parse_subaccount("Earnings:Current").unwrap()),
            account_current_conversions: unsourced(
                parse_subaccount("Conversions:Current").unwrap(),
            ),
            account_unrealized_gains: unsourced(parse_subaccount("Earnings:Unrealized").unwrap()),
            account_rounding: None,
            conversion_currency: unsourced(Currency::try_from("NOTHING").unwrap()),
            inferred_tolerance_default: HashMap::new(),
            inferred_tolerance_multiplier: unsourced(dec!(0.5)),
            infer_tolerance_from_cost: unsourced(false),
            documents: HashMap::new(),
            operating_currency: HashMap::new(),
            render_commas: unsourced(false),
            booking_method: unsourced(Booking::Strict),
            plugin_processing_mode: unsourced(PluginProcessingMode::Default),
            parser_options,
        }
    }

    pub(crate) fn assimilate(&mut self, opt: BeancountOption<'a>) -> Result<(), Error> {
        use BeancountOptionVariant::*;
        use OptionError::*;

        let BeancountOption { source, variant } = opt;
        match variant {
            Title(value) => Self::update(&mut self.title, value, source),

            // already assimilated into ParserOptions
            AccountTypeName(_, _) => Ok(()),

            AccountPreviousBalances(value) => {
                Self::update(&mut self.account_previous_balances, value, source)
            }
            AccountPreviousEarnings(value) => {
                Self::update(&mut self.account_previous_earnings, value, source)
            }
            AccountPreviousConversions(value) => {
                Self::update(&mut self.account_previous_conversions, value, source)
            }

            AccountCurrentEarnings(value) => {
                Self::update(&mut self.account_current_earnings, value, source)
            }

            AccountCurrentConversions(value) => {
                Self::update(&mut self.account_current_conversions, value, source)
            }

            AccountUnrealizedGains(value) => {
                Self::update(&mut self.account_unrealized_gains, value, source)
            }

            AccountRounding(value) => {
                Self::update_optional(&mut self.account_rounding, value, source)
            }

            ConversionCurrency(value) => Self::update(&mut self.conversion_currency, value, source),

            InferredToleranceDefault(currency_or_any, tolerance) => Self::update_hashmap(
                &mut self.inferred_tolerance_default,
                currency_or_any,
                tolerance,
                source,
            ),

            InferredToleranceMultiplier(value) => {
                Self::update(&mut self.inferred_tolerance_multiplier, value, source)
            }

            InferToleranceFromCost(value) => {
                Self::update(&mut self.infer_tolerance_from_cost, value, source)
            }

            Documents(path) => Self::update_unit_hashmap(&mut self.documents, path, source),

            OperatingCurrency(value) => {
                Self::update_unit_hashmap(&mut self.operating_currency, value, source)
            }

            RenderCommas(value) => Self::update(&mut self.render_commas, value, source),

            // already assimilated into ParserOptions
            LongStringMaxlines(_) => Ok(()),

            BookingMethod(value) => Self::update(&mut self.booking_method, value, source),

            PluginProcessingMode(value) => {
                Self::update(&mut self.plugin_processing_mode, value, source)
            }

            // this value contains nothing
            Assimilated => Ok(()),
        }
        .map_err(|ref e| match e {
            DuplicateOption(span) => Error::new("invalid option", "duplicate", source.name)
                .related_to_named_span("option", *span),
            DuplicateValue(span) => Error::new("invalid option", "duplicate value", source.value)
                .related_to_named_span("option value", *span),
        })
    }

    fn update<T>(
        field: &mut OptionallySourced<T>,
        value: T,
        source: Source,
    ) -> Result<(), OptionError> {
        use OptionError::*;

        match &field.source {
            None => {
                *field = optionally_sourced(value, source);
                Ok(())
            }
            Some(source) => Err(DuplicateOption(source.name)),
        }
    }

    fn update_optional<T>(
        field: &mut Option<Sourced<T>>,
        value: T,
        source: Source,
    ) -> Result<(), OptionError> {
        use OptionError::*;

        match field {
            None => {
                *field = Some(sourced(value, source));
                Ok(())
            }
            Some(Sourced { source, .. }) => Err(DuplicateOption(source.name)),
        }
    }

    fn update_hashmap<K, V>(
        field: &mut HashMap<K, (V, Source)>,
        key: K,
        value: V,
        source: Source,
    ) -> Result<(), OptionError>
    where
        K: Eq + Hash,
    {
        use hash_map::Entry::*;
        use OptionError::*;

        match field.entry(key) {
            Vacant(entry) => {
                entry.insert((value, source));

                Ok(())
            }
            Occupied(entry) => Err(DuplicateValue(entry.get().1.value)),
        }
    }

    // equivalent to a HashSet where the key has a source
    fn update_unit_hashmap<K>(
        field: &mut HashMap<K, Source>,
        key: K,
        source: Source,
    ) -> Result<(), OptionError>
    where
        K: Eq + Hash,
    {
        use hash_map::Entry::*;
        use OptionError::*;

        match field.entry(key) {
            Vacant(entry) => {
                entry.insert(source);

                Ok(())
            }
            Occupied(entry) => Err(DuplicateValue(entry.get().value)),
        }
    }

    pub fn account_type_name(&self, account_type: AccountType) -> &AccountTypeName {
        self.parser_options.account_type_name(account_type)
    }

    pub fn title(&self) -> &str {
        self.title.item
    }

    pub fn account_previous_balances(&self) -> &Subaccount {
        &self.account_previous_balances.item
    }

    pub fn account_previous_earnings(&self) -> &Subaccount {
        &self.account_previous_earnings.item
    }

    pub fn account_previous_conversions(&self) -> &Subaccount {
        &self.account_previous_conversions.item
    }

    pub fn account_current_earnings(&self) -> &Subaccount {
        &self.account_current_earnings.item
    }

    pub fn account_current_conversions(&self) -> &Subaccount {
        &self.account_current_conversions.item
    }

    pub fn account_unrealized_gains(&self) -> &Subaccount {
        &self.account_unrealized_gains.item
    }

    pub fn account_rounding(&self) -> Option<&Subaccount> {
        self.account_rounding.as_ref().map(|x| &x.item)
    }

    pub fn conversion_currency(&self) -> &Currency {
        &self.conversion_currency.item
    }

    pub fn inferred_tolerance_default(&self, currency: &Currency) -> Option<Decimal> {
        self.inferred_tolerance_default
            .get(&CurrencyOrAny::Currency(*currency))
            .map(|d| d.0)
            .or(self
                .inferred_tolerance_default
                .get(&CurrencyOrAny::Any)
                .map(|d| d.0))
    }

    /// return the tolerance defaults for all currencies, with None as the 'any' currency
    pub fn inferred_tolerance_defaults(&self) -> impl Iterator<Item = (Option<Currency>, Decimal)> {
        self.inferred_tolerance_default
            .iter()
            .map(|(c, d)| match c {
                CurrencyOrAny::Currency(c) => (Some(*c), d.0),
                CurrencyOrAny::Any => (None, d.0),
            })
    }

    pub fn inferred_tolerance_multiplier(&self) -> Decimal {
        self.inferred_tolerance_multiplier.item
    }

    pub fn infer_tolerance_from_cost(&self) -> bool {
        self.infer_tolerance_from_cost.item
    }

    pub fn documents(&self) -> impl Iterator<Item = &PathBuf> {
        self.documents.iter().map(|document| document.0)
    }

    pub fn operating_currency(&self) -> impl Iterator<Item = &Currency> {
        self.operating_currency.iter().map(|document| document.0)
    }

    pub fn render_commas(&self) -> bool {
        self.render_commas.item
    }

    pub fn booking_method(&self) -> Booking {
        self.booking_method.item
    }

    pub fn plugin_processing_mode(&self) -> PluginProcessingMode {
        self.plugin_processing_mode.item
    }

    pub fn long_string_maxlines(&self) -> usize {
        self.parser_options.long_string_maxlines.item
    }
}

#[derive(Debug)]
struct Sourced<T> {
    item: T,
    source: Source,
}

impl<T> PartialEq for Sourced<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.item.eq(&other.item)
    }
}

impl<T> Eq for Sourced<T> where T: Eq {}

fn sourced<T>(item: T, source: Source) -> Sourced<T> {
    Sourced { item, source }
}

#[derive(Debug)]
pub(crate) struct OptionallySourced<T> {
    pub(crate) item: T,
    pub(crate) source: Option<Source>,
}

fn unsourced<T>(item: T) -> OptionallySourced<T> {
    OptionallySourced { item, source: None }
}

fn optionally_sourced<T>(item: T, source: Source) -> OptionallySourced<T> {
    OptionallySourced {
        item,
        source: Some(source),
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) struct Source {
    pub(crate) name: Span,
    pub(crate) value: Span,
}

#[derive(PartialEq, Eq, Debug)]
pub(crate) enum OptionError {
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
        use hash_map::Entry::*;

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
pub(crate) struct AccountTypeNamesError(AccountTypeNamesErrorKind);

#[derive(PartialEq, Eq, Debug)]
enum AccountTypeNamesErrorKind {
    NameInUse(AccountType),
}

impl Display for AccountTypeNamesError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AccountTypeNamesErrorKind::*;
        match &self.0 {
            NameInUse(t) => write!(f, "account type name in use for {}", t.as_ref()),
        }
    }
}

impl std::error::Error for AccountTypeNamesError {}
