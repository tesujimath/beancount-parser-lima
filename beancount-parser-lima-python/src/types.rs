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
    #[pyo3(get)]
    postings: Vec<Posting>,
}

pub(crate) fn transaction(
    py: Python<'_>,
    date: &Date,
    x: &lima::Transaction<'_>,
) -> PyResult<Py<PyAny>> {
    let narration = x
        .narration()
        .map(|narration| PyString::new(py, narration.item()).into());
    let postings = x.postings().map(|p| posting(py, p)).collect::<Vec<_>>();

    Ok(Py::new(
        py,
        (
            Transaction {
                narration,
                postings,
            },
            Directive::new(py, date),
        ),
    )?
    .into_py(py))
}

// A single posting within a [Transaction].
#[derive(Clone, Debug)]
#[pyclass]
struct Posting {
    // flag: Option<Spanned<Flag>>,
    // account: Py<PyList>,
    // amount: Option<Decimal>,
    #[pyo3(get)]
    currency: Option<Py<PyString>>,
    // cost_spec: Option<Spanned<CostSpec<'a>>>,
    // price_annotation: Option<Spanned<ScopedAmount<'a>>>,
    // metadata: Metadata<'a>,
}

fn posting(py: Python<'_>, x: &lima::Posting<'_>) -> Posting {
    let currency = x
        .currency()
        .map(|currency| PyString::new(py, currency.item().as_ref()).into());

    Posting { currency }
}
