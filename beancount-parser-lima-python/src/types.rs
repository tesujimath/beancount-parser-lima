// TODO remove suppression for dead code warning
#![allow(dead_code)]

use beancount_parser_lima as lima;
use pyo3::{
    pyclass,
    types::{PyDate, PyString},
    Py, Python,
};
use time::Date;

/// Beancount directive base class.
#[derive(Debug)]
#[pyclass]
struct Directive {
    date: Py<PyDate>,
    // metadata: Metadata<'a>,
}

/// Beancount transaction directive.
#[derive(Debug)]
#[pyclass]
pub(crate) struct Transaction {
    // flag: Py<PyString>,
    // payee: Option<Py<PyString>>,
    #[pyo3(get)]
    narration: Option<Py<PyString>>,
    // postings: Vec<Posting>,
}

pub(crate) fn transaction<'a>(
    py: Python<'_>,
    _date: &'a Date,
    x: &lima::Transaction<'a>,
) -> Transaction {
    let narration = x
        .narration()
        .map(|narration| PyString::new(py, narration.item()).into());

    Transaction { narration }
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
