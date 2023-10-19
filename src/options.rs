use super::format::{format, plain};
use super::types::*;
use nonempty::NonEmpty;
use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
pub struct Options<'a> {
    pub(crate) account_type_names: AccountTypeNames<'a>,
    title: &'a str,
    account_previous_balances: NonEmpty<AccountName<'a>>,
    account_previous_earnings: NonEmpty<AccountName<'a>>,
    account_previous_conversions: NonEmpty<AccountName<'a>>,
    account_current_earnings: NonEmpty<AccountName<'a>>,
    account_current_conversions: NonEmpty<AccountName<'a>>,
    account_unrealized_gains: NonEmpty<AccountName<'a>>,
}

impl<'a> Default for Options<'a> {
    fn default() -> Self {
        Options {
            account_type_names: AccountTypeNames::default(),
            title: "Beancount",
            account_previous_balances: Self::parse_account_name("Opening-Balances").unwrap(),
            account_previous_earnings: Self::parse_account_name("Earnings:Previous").unwrap(),
            account_previous_conversions: Self::parse_account_name("Conversions:Previous").unwrap(),
            account_current_earnings: Self::parse_account_name("Earnings:Current").unwrap(),
            account_current_conversions: Self::parse_account_name("Conversions:Current").unwrap(),
            account_unrealized_gains: Self::parse_account_name("Earnings:Unrealized").unwrap(),
        }
    }
}

impl<'a> Options<'a> {
    pub(crate) fn set(&mut self, name: &'a str, value: &'a str) -> Result<(), OptionsError> {
        use AccountType::*;
        use OptionsErrorKind::*;

        match name {
            "title" => Self::update_string(&mut self.title, value),

            "name_assets" => self.update_account_name_type(Assets, value),
            "name_liabilities" => self.update_account_name_type(Liabilities, value),
            "name_equity" => self.update_account_name_type(Equity, value),
            "name_income" => self.update_account_name_type(Income, value),
            "name_expenses" => self.update_account_name_type(Expenses, value),

            "account_previous_balances" => {
                Self::update_account_name(&mut self.account_previous_balances, value)
            }

            "account_previous_earnings" => {
                Self::update_account_name(&mut self.account_previous_earnings, value)
            }

            "account_previous_conversions" => {
                Self::update_account_name(&mut self.account_previous_conversions, value)
            }

            "account_current_earnings" => {
                Self::update_account_name(&mut self.account_current_earnings, value)
            }

            "account_current_conversions" => {
                Self::update_account_name(&mut self.account_current_conversions, value)
            }

            "account_unrealized_gains" => {
                Self::update_account_name(&mut self.account_unrealized_gains, value)
            }

            _ => Err(OptionsError(UnknownOption)),
        }
    }

    fn update_string(field: &mut &'a str, value: &'a str) -> Result<(), OptionsError> {
        *field = value;
        Ok(())
    }

    // a colon-separated account name not including the account type
    fn update_account_name(
        field: &mut NonEmpty<AccountName<'a>>,
        value: &'a str,
    ) -> Result<(), OptionsError> {
        let account_name = Self::parse_account_name(value)
            .map_err(|e| OptionsError(OptionsErrorKind::AccountName(e)))?;

        *field = account_name;

        Ok(())
    }

    fn parse_account_name(
        colon_separated: &str,
    ) -> Result<NonEmpty<AccountName>, AccountNameError> {
        let mut account = colon_separated.split(':');
        let account_name_components = account
            .by_ref()
            .map(AccountName::try_from)
            .collect::<Result<Vec<AccountName>, _>>()?;

        Ok(NonEmpty::collect(account_name_components).unwrap())
    }

    fn update_account_name_type(
        &mut self,
        account_type: AccountType,
        account_type_name: &'a str,
    ) -> Result<(), OptionsError> {
        use OptionsErrorKind as Kind;

        AccountTypeName::try_from(account_type_name)
            .map_err(|e| OptionsError(Kind::AccountTypeName(e)))
            .and_then(|account_type_name| {
                self.account_type_names
                    .update(account_type, account_type_name)
                    .map_err(|e| OptionsError(Kind::AccountTypeNames(e)))
            })
    }

    pub fn title(&self) -> &str {
        self.title
    }

    pub fn account_type_name(&self, account_type: AccountType) -> AccountTypeName {
        self.account_type_names.name_by_type[account_type as usize]
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct OptionsError(OptionsErrorKind);

#[derive(PartialEq, Eq, Debug)]
enum OptionsErrorKind {
    UnknownOption,
    AccountTypeName(AccountTypeNameError),
    AccountTypeNames(AccountTypeNamesError),
    AccountName(AccountNameError),
}

impl Display for OptionsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use OptionsErrorKind::*;

        match &self.0 {
            UnknownOption => f.write_str("unknown option"),
            AccountTypeName(e) => write!(f, "{}", e),
            AccountTypeNames(e) => write!(f, "{}", e),
            AccountName(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for OptionsError {}

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
