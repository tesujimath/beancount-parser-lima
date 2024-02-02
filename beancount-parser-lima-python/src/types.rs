// TODO remove suppression for dead code warning
#![allow(dead_code)]

use pyo3::{
    pyclass,
    types::{PyDate, PyDict, PyList, PySet, PyString},
    Py,
};
use rust_decimal::Decimal;

/// Beancount directive base class.
#[derive(Debug)]
#[pyclass(frozen, subclass)]
pub(crate) struct Directive {
    #[pyo3(get)]
    pub(crate) date: Py<PyDate>,
    #[pyo3(get)]
    pub(crate) metadata: Option<Metadata>,
}

/// Beancount transaction directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
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
#[pyclass(frozen)]
pub(crate) struct Posting {
    #[pyo3(get)]
    pub(crate) flag: Option<Py<PyString>>,
    #[pyo3(get)]
    pub(crate) account: Py<PyList>,
    #[pyo3(get)]
    pub(crate) amount: Option<Decimal>,
    #[pyo3(get)]
    pub(crate) currency: Option<Py<PyString>>,
    #[pyo3(get)]
    pub(crate) cost_spec: Option<CostSpec>,
    #[pyo3(get)]
    pub(crate) price_annotation: Option<ScopedAmount>,
    #[pyo3(get)]
    pub(crate) metadata: Option<Metadata>,
}

/// Beancount price directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
pub(crate) struct Price {
    #[pyo3(get)]
    pub(crate) currency: Py<PyString>,
    #[pyo3(get)]
    pub(crate) amount: Amount,
}

/// Beancount balance directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
pub(crate) struct Balance {
    #[pyo3(get)]
    pub(crate) account: Py<PyList>,
    #[pyo3(get)]
    pub(crate) atol: AmountWithTolerance,
}

/// Beancount open directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
pub(crate) struct Open {
    #[pyo3(get)]
    pub(crate) account: Py<PyList>,
    #[pyo3(get)]
    pub(crate) currencies: Py<PyList>,
    #[pyo3(get)]
    pub(crate) booking: Option<Py<PyString>>,
}

/// Beancount close directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
pub(crate) struct Close {
    #[pyo3(get)]
    pub(crate) account: Py<PyList>,
}

/// Beancount commodity directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
pub(crate) struct Commodity {
    #[pyo3(get)]
    pub(crate) currency: Py<PyString>,
}

/// Beancount pad directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
pub(crate) struct Pad {
    #[pyo3(get)]
    pub(crate) account: Py<PyList>,
    #[pyo3(get)]
    pub(crate) source: Py<PyList>,
}

/// Beancount document directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
pub(crate) struct Document {
    #[pyo3(get)]
    pub(crate) account: Py<PyList>,
    #[pyo3(get)]
    pub(crate) path: Py<PyString>,
}

/// Beancount note directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
pub(crate) struct Note {
    #[pyo3(get)]
    pub(crate) account: Py<PyList>,
    #[pyo3(get)]
    pub(crate) comment: Py<PyString>,
}

/// Beancount event directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
pub(crate) struct Event {
    #[pyo3(get)]
    pub(crate) event_type: Py<PyString>,
    #[pyo3(get)]
    pub(crate) description: Py<PyString>,
}

/// Beancount query directive.
#[derive(Debug)]
#[pyclass(frozen, extends=Directive)]
pub(crate) struct Query {
    #[pyo3(get)]
    pub(crate) name: Py<PyString>,
    #[pyo3(get)]
    pub(crate) content: Py<PyString>,
}

// TODO Custom directive, not yet supported by core parser

#[derive(Clone, Debug)]
#[pyclass(frozen)]
pub struct Amount {
    #[pyo3(get)]
    pub(crate) number: Decimal,
    #[pyo3(get)]
    pub(crate) currency: Py<PyString>,
}

/// An `Amount` with optional tolerance.
#[derive(Clone, Debug)]
#[pyclass(frozen)]
pub struct AmountWithTolerance {
    #[pyo3(get)]
    pub(crate) amount: Amount,
    #[pyo3(get)]
    pub(crate) tolerance: Option<Decimal>,
}

/// A cost specification.
#[derive(Clone, Debug)]
#[pyclass(frozen)]
pub(crate) struct CostSpec {
    #[pyo3(get)]
    pub(crate) per_unit: Option<Decimal>,
    #[pyo3(get)]
    pub(crate) total: Option<Decimal>,
    #[pyo3(get)]
    pub(crate) currency: Option<Py<PyString>>,
    #[pyo3(get)]
    pub(crate) date: Option<Py<PyDate>>,
    #[pyo3(get)]
    pub(crate) label: Option<Py<PyString>>,
    #[pyo3(get)]
    pub(crate) merge: bool,
}

/// An amount which specifies a total or per-unit and/or a currency.
#[derive(Clone, Debug)]
#[pyclass(frozen)]
pub(crate) struct ScopedAmount {
    #[pyo3(get)]
    pub(crate) per_unit: Option<Decimal>,
    #[pyo3(get)]
    pub(crate) total: Option<Decimal>,
    #[pyo3(get)]
    pub(crate) currency: Option<Py<PyString>>,
}

/// Metadata associated with a [Directive] or a [Posting].
#[derive(Clone, Debug)]
#[pyclass(frozen)]
pub struct Metadata {
    #[pyo3(get)]
    pub(crate) key_values: Option<Py<PyDict>>,
    #[pyo3(get)]
    pub(crate) tags: Option<Py<PyList>>,
    #[pyo3(get)]
    pub(crate) links: Option<Py<PyList>>,
}

/// A value of metadata key/value.
/// A Rust enum mapped onto Python baseclass and subclasses.
#[derive(Clone, Debug)]
#[pyclass(frozen, subclass)]
pub struct MetaValue {}

#[derive(Clone, Debug)]
#[pyclass(frozen, extends=MetaValue)]
pub struct MetaValueAmount {
    #[pyo3(get)]
    pub(crate) value: Amount,
}

#[derive(Clone, Debug)]
#[pyclass(frozen, extends=MetaValue)]
pub struct MetaValueString {
    #[pyo3(get)]
    pub(crate) value: Py<PyString>,
}

#[derive(Clone, Debug)]
#[pyclass(frozen, extends=MetaValue)]
pub struct MetaValueCurrency {
    #[pyo3(get)]
    pub(crate) value: Py<PyString>,
}

#[derive(Clone, Debug)]
#[pyclass(frozen, extends=MetaValue)]
pub struct MetaValueAccount {
    #[pyo3(get)]
    pub(crate) value: Py<PyList>,
}

#[derive(Clone, Debug)]
#[pyclass(frozen, extends=MetaValue)]
pub struct MetaValueTag {
    #[pyo3(get)]
    pub(crate) value: Py<PyString>,
}

#[derive(Clone, Debug)]
#[pyclass(frozen, extends=MetaValue)]
pub struct MetaValueLink {
    #[pyo3(get)]
    pub(crate) value: Py<PyString>,
}

#[derive(Clone, Debug)]
#[pyclass(frozen, extends=MetaValue)]
pub struct MetaValueDate {
    #[pyo3(get)]
    pub(crate) value: Py<PyDate>,
}

#[derive(Clone, Debug)]
#[pyclass(frozen, extends=MetaValue)]
pub struct MetaValueBool {
    #[pyo3(get)]
    pub(crate) value: bool,
}

#[derive(Clone, Debug)]
#[pyclass(frozen, extends=MetaValue)]
pub struct MetaValueNone;

#[derive(Clone, Debug)]
#[pyclass(frozen, extends=MetaValue)]
pub struct MetaValueExpr {
    #[pyo3(get)]
    pub(crate) value: Decimal,
}

/// All options read in from `option` pragmas, including those for internal processing only.
#[derive(Clone, Debug)]
#[pyclass(frozen)]
pub(crate) struct Options {
    #[pyo3(get)]
    pub(crate) title: Py<PyString>,
    #[pyo3(get)]
    pub(crate) account_previous_balances: Py<PyList>,
    #[pyo3(get)]
    pub(crate) account_previous_earnings: Py<PyList>,
    #[pyo3(get)]
    pub(crate) account_previous_conversions: Py<PyList>,
    #[pyo3(get)]
    pub(crate) account_current_earnings: Py<PyList>,
    #[pyo3(get)]
    pub(crate) account_current_conversions: Py<PyList>,
    #[pyo3(get)]
    pub(crate) account_unrealized_gains: Py<PyList>,
    #[pyo3(get)]
    pub(crate) account_rounding: Option<Py<PyList>>,
    #[pyo3(get)]
    pub(crate) conversion_currency: Py<PyString>,
    #[pyo3(get)]
    pub(crate) inferred_tolerance_default: Py<PyDict>,
    #[pyo3(get)]
    pub(crate) inferred_tolerance_multiplier: Decimal,
    #[pyo3(get)]
    pub(crate) infer_tolerance_from_cost: bool,
    #[pyo3(get)]
    pub(crate) documents: Py<PySet>,
    #[pyo3(get)]
    pub(crate) operating_currency: Py<PySet>,
    #[pyo3(get)]
    pub(crate) render_commas: bool,
    #[pyo3(get)]
    pub(crate) booking_method: Py<PyString>,
    #[pyo3(get)]
    pub(crate) plugin_processing_mode: Py<PyString>,
    // from ParserOptions:
    #[pyo3(get)]
    pub(crate) account_name_by_type: Py<PyDict>,
    #[pyo3(get)]
    pub(crate) account_type_by_name: Py<PyDict>,
    #[pyo3(get)]
    pub(crate) long_string_maxlines: usize,
}

mod format;
