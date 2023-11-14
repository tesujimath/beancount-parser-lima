// TODO remove suppression for dead code warning
#![allow(dead_code)]

use beancount_parser_lima as lima;
use pyo3::{
    pyclass,
    types::{PyDate, PyString},
    IntoPy, Py, PyAny, PyResult, Python,
};
use time::Date;

/// Beancount directive base class.
#[derive(Debug)]
#[pyclass(subclass)]
pub(crate) struct Directive {
    #[pyo3(get)]
    date: Py<PyDate>,
    // metadata: Metadata<'a>,
}

impl Directive {
    fn new(py: Python<'_>, date: &Date) -> Self {
        Directive {
            date: PyDate::new(py, date.year(), date.month() as u8, date.day())
                .unwrap()
                .into(),
        }
    }
}

/// Beancount transaction directive.
#[derive(Debug)]
#[pyclass(extends=Directive)]
pub(crate) struct Transaction {
    // flag: Py<PyString>,
    // payee: Option<Py<PyString>>,
    #[pyo3(get)]
    narration: Option<Py<PyString>>,
    // postings: Vec<Posting>,
}

pub(crate) fn transaction<'a>(
    py: Python<'_>,
    date: &'a Date,
    x: &lima::Transaction<'a>,
) -> PyResult<Py<PyAny>> {
    let narration = x
        .narration()
        .map(|narration| PyString::new(py, narration.item()).into());

    Ok(Py::new(py, (Transaction { narration }, Directive::new(py, date)))?.into_py(py))
}

// A single posting within a [Transaction].
// #[derive(Debug)]
// #[pyclass]
// struct Posting {
// flag: Option<Spanned<Flag>>,
// account: Py<PyList>,
// amount: Option<Decimal>,
// currency: Option<Py<PyString>>,
// cost_spec: Option<Spanned<CostSpec<'a>>>,
// price_annotation: Option<Spanned<ScopedAmount<'a>>>,
// metadata: Metadata<'a>,
// }
