use super::format::{format, plain};
use super::types::*;
use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

#[derive(Default, Debug)]
pub struct Options<'a> {
    pub(crate) account_type_names: AccountTypeNames<'a>,
}

impl<'a> Options<'a> {
    pub(crate) fn set(&mut self, opt: &BeancountOption<'a>) -> Result<(), OptionsError> {
        use AccountType::*;
        use OptionsErrorKind::*;

        match opt.name {
            "name_assets" => self.update_account_name_type(Assets, opt.value),
            "name_liabilities" => self.update_account_name_type(Liabilities, opt.value),
            "name_equity" => self.update_account_name_type(Equity, opt.value),
            "name_income" => self.update_account_name_type(Income, opt.value),
            "name_expenses" => self.update_account_name_type(Expenses, opt.value),

            _ => Err(OptionsError(UnknownOption)),
        }
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
}

impl Display for OptionsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use OptionsErrorKind::*;

        match &self.0 {
            UnknownOption => f.write_str("unknown option"),
            AccountTypeName(e) => write!(f, "{}", e),
            AccountTypeNames(e) => write!(f, "{}", e),
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
                if o.get() == &account_type {
                    // updating as same, harmless
                    Ok(())
                } else {
                    Err(AccountTypeNamesError(AccountTypeNamesErrorKind::NameInUse))
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
    NameInUse,
}

impl Display for AccountTypeNamesError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AccountTypeNamesErrorKind::*;
        match &self.0 {
            NameInUse => write!(f, "account type name in use"),
        }
    }
}

impl std::error::Error for AccountTypeNamesError {}
