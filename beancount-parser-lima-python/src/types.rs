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
#[pyclass]
struct Directive {
    date: Py<PyDate>,
    // metadata: Metadata<'a>,
}

/// Beancount transaction directive.
#[derive(Debug)]
#[pyclass]
struct Transaction {
    // flag: &PyString,
    // payee: Option<&PyString>,
    // narration: Option<&PyString>,
    postings: Vec<Posting>,
}

/// A single posting within a [Transaction].
#[derive(Debug)]
#[pyclass]
struct Posting {
    // flag: Option<Spanned<Flag>>,
    account: Py<PyList>,
    amount: Option<Decimal>,
    currency: Option<Py<PyString>>,
    // cost_spec: Option<Spanned<CostSpec<'a>>>,
    // price_annotation: Option<Spanned<ScopedAmount<'a>>>,
    // metadata: Metadata<'a>,
}
