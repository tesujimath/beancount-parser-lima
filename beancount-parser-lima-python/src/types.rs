// TODO remove suppression for dead code warning
#![allow(dead_code)]

use pyo3::{
    pyclass,
    types::{PyDate, PyString},
    Py,
};

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
    // flag: Py<PyString>,
    // payee: Option<Py<PyString>>,
    #[pyo3(get)]
    pub(crate) narration: Option<Py<PyString>>,
    #[pyo3(get)]
    pub(crate) postings: Vec<Posting>,
}

// A single posting within a [Transaction].
#[derive(Clone, Debug)]
#[pyclass]
pub(crate) struct Posting {
    // flag: Option<Spanned<Flag>>,
    // account: Py<PyList>,
    // amount: Option<Decimal>,
    #[pyo3(get)]
    pub(crate) currency: Option<Py<PyString>>,
    // cost_spec: Option<Spanned<CostSpec<'a>>>,
    // price_annotation: Option<Spanned<ScopedAmount<'a>>>,
    // metadata: Metadata<'a>,
}
