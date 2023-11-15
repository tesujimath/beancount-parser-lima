use std::{cell::RefCell, cmp::Ordering, iter::once};

use crate::types::*;
use beancount_parser_lima as lima;
use pyo3::{
    types::{PyDate, PyList, PyString},
    IntoPy, Py, PyAny, PyResult, Python,
};
use string_interner::{symbol::SymbolU32, StringInterner, Symbol};
use time::Date;

/// Convert from Rust to Python while interning strings, dates, Subaccount lists.
///
/// While PyO3 does seem to support string interning, there's a comment there that
/// every string interning request results in creation of a temporary Python String
/// object, which we must avoid.
pub(crate) struct Converter {
    string_interner: StringInterner,
    py_strings: Vec<Py<PyString>>,
}

impl Converter {
    pub(crate) fn new() -> Self {
        Converter {
            string_interner: StringInterner::new(),
            // the StringInterner never produces symbol zero, so:
            py_strings: Vec::new(),
        }
    }

    pub(crate) fn directive(&mut self, py: Python<'_>, date: &Date) -> Directive {
        Directive {
            date: PyDate::new(py, date.year(), date.month() as u8, date.day())
                .unwrap()
                .into(),
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
            .map(|narration| PyString::new(py, narration.item()).into());
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
        let account = self.account(py, x.account());
        let currency = x
            .currency()
            .map(|currency| self.string(py, currency.item().as_ref()));

        Posting { account, currency }
    }

    pub(crate) fn account(&mut self, py: Python<'_>, x: &lima::Account) -> Py<PyList> {
        let rc = RefCell::new(self);
        let account_type = rc.borrow_mut().str2sym(py, x.account_type().as_ref());
        let subaccount = x.names().map(|name| {
            // let mut c = rc.borrow_mut();

            rc.borrow_mut().str2sym(py, name.as_ref())
        });

        PyList::new(
            py,
            once(account_type)
                .chain(subaccount)
                .map(|sym| rc.borrow_mut().sym2string(py, sym))
                .collect::<Vec<_>>(),
        )
        .into()
    }

    pub(crate) fn string(&mut self, py: Python<'_>, x: &str) -> Py<PyString> {
        let sym = self.str2sym(py, x);
        self.sym2string(py, sym)
    }

    fn sym2string(&mut self, py: Python<'_>, sym: SymbolU32) -> Py<PyString> {
        self.py_strings[sym.to_usize()].clone_ref(py)
    }

    fn str2sym(&mut self, py: Python<'_>, x: &str) -> SymbolU32 {
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
