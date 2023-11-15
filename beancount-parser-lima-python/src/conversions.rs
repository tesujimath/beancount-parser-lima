use crate::types::*;
use beancount_parser_lima as lima;
use pyo3::{
    types::{PyDate, PyString},
    IntoPy, Py, PyAny, PyResult, Python,
};
use string_interner::StringInterner;
use time::Date;

/// Convert from Rust to Python while interning strings, dates, Subaccount lists.
///
/// While PyO3 does seem to support string interning, there's a comment there that
/// every string interning request results in creation of a temporary Python String
/// object, which we must avoid.
pub(crate) struct Converter {
    string_interner: StringInterner,
}

impl Converter {
    pub(crate) fn new() -> Self {
        Converter {
            string_interner: StringInterner::new(),
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
        let currency = x
            .currency()
            .map(|currency| self.string(py, currency.item().as_ref()));

        Posting { currency }
    }

    pub(crate) fn string(&mut self, py: Python<'_>, s: &str) -> Py<PyString> {
        PyString::new(py, s).into()
    }
}
