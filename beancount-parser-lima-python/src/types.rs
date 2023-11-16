// TODO remove suppression for dead code warning
#![allow(dead_code)]

use pyo3::{
    pyclass,
    types::{PyDate, PyList, PyString},
    Py,
};
use rust_decimal::Decimal;

/// Beancount directive base class.
#[derive(Debug)]
#[pyclass(subclass)]
pub(crate) struct Directive {
    #[pyo3(get)]
    pub(crate) date: Py<PyDate>,
    // metadata: Metadata<'a>,
}

/// Beancount transaction directive.
#[derive(Debug)]
#[pyclass(extends=Directive)]
pub(crate) struct Transaction {
    #[pyo3(get)]
    pub(crate) flag: Py<PyString>,
    #[pyo3(get)]
    pub(crate) payee: Option<Py<PyString>>,
    #[pyo3(get)]
    pub(crate) narration: Option<Py<PyString>>,
    #[pyo3(get)]
    pub(crate) postings: Vec<Posting>,
}

// A single posting within a [Transaction].
#[derive(Clone, Debug)]
#[pyclass]
pub(crate) struct Posting {
    #[pyo3(get)]
    pub(crate) flag: Option<Py<PyString>>,
    #[pyo3(get)]
    pub(crate) account: Py<PyList>,
    #[pyo3(get)]
    pub(crate) amount: Option<Decimal>,
    #[pyo3(get)]
    pub(crate) currency: Option<Py<PyString>>,
    // cost_spec: Option<Spanned<CostSpec<'a>>>,
    // price_annotation: Option<Spanned<ScopedAmount<'a>>>,
    // metadata: Metadata<'a>,
}

mod format;
