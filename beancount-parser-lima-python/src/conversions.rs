use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{hash_map, HashMap},
    hash::Hash,
    iter::{once, FromIterator},
};

use crate::types::*;
use beancount_parser_lima as lima;
use pyo3::{
    types::{PyDate, PyDateAccess, PyList, PyString},
    IntoPy, Py, PyAny, PyResult, Python,
};
use smallvec::SmallVec;
use string_interner::{symbol::SymbolU32, StringInterner, Symbol};
use time::Date;

/// Convert from Rust to Python while interning strings, dates, Subaccount lists.
///
/// While PyO3 does seem to support string interning, there's a comment there that
/// every string interning request results in creation of a temporary Python String
/// object, which we choose to avoid.
pub(crate) struct Converter {
    string: StringFactory,
    account: AccountFactory,
    date: DateFactory,
}

impl Converter {
    pub(crate) fn new() -> Self {
        Converter {
            string: StringFactory::new(),
            account: AccountFactory::new(),
            date: DateFactory::new(),
        }
    }

    pub(crate) fn directive(&mut self, py: Python<'_>, date: &Date) -> Directive {
        Directive {
            date: self.date.create_or_reuse(py, date),
        }
    }

    pub(crate) fn transaction(
        &mut self,
        py: Python<'_>,
        date: &Date,
        x: &lima::Transaction<'_>,
    ) -> PyResult<Py<PyAny>> {
        let narration = x
            .narration()
            .map(|narration| self.string.create_or_reuse(py, narration.item()));
        let postings = x
            .postings()
            .map(|p| self.posting(py, p))
            .collect::<Vec<_>>();

        Ok(Py::new(
            py,
            (
                Transaction {
                    narration,
                    postings,
                },
                self.directive(py, date),
            ),
        )?
        .into_py(py))
    }

    pub(crate) fn posting(&mut self, py: Python<'_>, x: &lima::Posting<'_>) -> Posting {
        let account = self
            .account
            .create_or_reuse(py, x.account(), &mut self.string);
        let currency = x
            .currency()
            .map(|currency| self.string.create_or_reuse(py, currency.item().as_ref()));

        Posting { account, currency }
    }
}

struct StringFactory {
    string_interner: StringInterner,
    py_strings: Vec<Py<PyString>>,
}

impl StringFactory {
    fn new() -> Self {
        StringFactory {
            string_interner: StringInterner::new(),
            py_strings: Vec::new(),
        }
    }

    // either create a new string or reuse an existing one if we've already seen this string before
    fn create_or_reuse(&mut self, py: Python<'_>, x: &str) -> Py<PyString> {
        let sym = self.create_or_lookup_symbol(py, x);
        self.reuse(py, sym)
    }

    // reuse a string we have in our symbol table
    fn reuse(&mut self, py: Python<'_>, sym: SymbolU32) -> Py<PyString> {
        self.py_strings[sym.to_usize()].clone_ref(py)
    }

    fn create_or_lookup_symbol(&mut self, py: Python<'_>, x: &str) -> SymbolU32 {
        use Ordering::*;

        let sym = self.string_interner.get_or_intern(x);
        let i_sym = sym.to_usize();
        println!("interned {} as {:?} with index {}", x, sym, i_sym);

        // allocate a new PyString if we don't already have it
        match i_sym.cmp(&self.py_strings.len()) {
            Less => (),
            Equal => {
                self.py_strings.push(PyString::new(py, x).into());
            }
            Greater => {
                // impossible
                panic!(
                    "unexpected symbol {:?} with index {} from string_interner with vec len {}",
                    sym,
                    i_sym,
                    self.py_strings.len()
                );
            }
        }

        sym
    }
}

struct AccountFactory {
    accounts: HashMap<AccountKey, Py<PyList>>,
}

impl AccountFactory {
    pub(crate) fn new() -> Self {
        AccountFactory {
            accounts: HashMap::new(),
        }
    }

    fn account_key(
        &mut self,
        py: Python<'_>,
        x: &lima::Account,
        string: &mut StringFactory,
    ) -> AccountKey {
        let string = RefCell::new(string);
        let account_type = string
            .borrow_mut()
            .create_or_lookup_symbol(py, x.account_type().as_ref());
        let subaccount = x.names().map(|name| {
            string
                .borrow_mut()
                .create_or_lookup_symbol(py, name.as_ref())
        });

        once(account_type).chain(subaccount).collect::<AccountKey>()
    }

    fn create_or_reuse(
        &mut self,
        py: Python<'_>,
        x: &lima::Account,
        string: &mut StringFactory,
    ) -> Py<PyList> {
        use hash_map::Entry::*;

        let key = self.account_key(py, x, string);
        // if we've got this account, then just clone a new ref, otherwise create a PyList
        match self.accounts.entry(key) {
            Occupied(account) => account.get().clone_ref(py),
            Vacant(entry) => {
                let string = RefCell::new(string);
                let account_type = string
                    .borrow_mut()
                    .create_or_lookup_symbol(py, x.account_type().as_ref());
                let subaccount = x.names().map(|name| {
                    string
                        .borrow_mut()
                        .create_or_lookup_symbol(py, name.as_ref())
                });

                entry
                    .insert(
                        PyList::new(
                            py,
                            once(account_type)
                                .chain(subaccount)
                                .map(|sym| string.borrow_mut().reuse(py, sym))
                                .collect::<Vec<_>>(),
                        )
                        .into(),
                    )
                    .clone_ref(py)
            }
        }
    }
}

/// Uniquely identifies an account.
struct AccountKey(SmallVec<SymbolU32, 5>);

impl PartialEq for AccountKey {
    fn eq(&self, other: &Self) -> bool {
        self.0.len() == other.0.len() && self.0.iter().zip(other.0.iter()).all(|(a, b)| a == b)
    }
}

impl Eq for AccountKey {}

impl Hash for AccountKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for sym in self.0.iter() {
            sym.hash(state);
        }
    }
}

impl FromIterator<SymbolU32> for AccountKey {
    fn from_iter<T: IntoIterator<Item = SymbolU32>>(iter: T) -> Self {
        AccountKey(iter.into_iter().collect())
    }
}

struct DateFactory {
    cached_date: Option<Py<PyDate>>,
}

impl DateFactory {
    pub(crate) fn new() -> Self {
        DateFactory { cached_date: None }
    }

    // reuse the cached date if it matches, otherwise discard and create a new one.
    fn create_or_reuse(&mut self, py: Python<'_>, x: &Date) -> Py<PyDate> {
        let cached_date = self.cached_date.take();

        self.cached_date = match cached_date {
            Some(cached_date) => {
                let py_date = cached_date.as_ref(py);
                if py_date.get_year() == x.year()
                    && py_date.get_month() == x.month() as u8
                    && py_date.get_day() == x.day()
                {
                    Some(cached_date)
                } else {
                    Some(create_date(py, x))
                }
            }
            None => Some(create_date(py, x)),
        };

        self.cached_date.as_ref().unwrap().clone_ref(py)
    }
}

fn create_date(py: Python<'_>, x: &Date) -> Py<PyDate> {
    PyDate::new(py, x.year(), x.month() as u8, x.day())
        .unwrap()
        .into()
}
