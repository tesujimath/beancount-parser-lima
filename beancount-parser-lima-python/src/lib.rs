use ::beancount_parser_lima as lima;
use pyo3::prelude::*;
use pyo3::types::PyList;
use std::io::{self, prelude::*};
use std::path::PathBuf;
use types::Options;

/// Python wrapper for BeancountSources
#[derive(Debug)]
#[pyclass(frozen)]
pub struct BeancountSources(lima::BeancountSources);

#[pymethods]
impl BeancountSources {
    #[new]
    fn new(path: &str) -> BeancountSources {
        let sources = lima::BeancountSources::new(PathBuf::from(path));
        BeancountSources(sources)
    }

    fn parse(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let mut stderr = &io::stderr();

        let parser = lima::BeancountParser::new(&self.0);
        // TODO remove
        writeln!(stderr, "{:?}", &self.0).unwrap();

        parse(py, &parser)
    }

    // Would be nice to be able to pass in a File object from Python, but this:
    // https://github.com/PyO3/pyo3/issues/933
    fn write(&self, py: Python<'_>, errors_and_warnings: Py<PyList>) -> PyResult<()> {
        let errors_and_warnings = errors_and_warnings.as_ref(py);
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        for x in errors_and_warnings.iter() {
            match Error::extract(x) {
                Ok(e) => errors.push(e.0),
                Err(_) => match Warning::extract(x) {
                    Ok(w) => warnings.push(w.0),
                    Err(_) => {
                        eprintln!("Failed to extract error or warning");
                    }
                },
            }
        }

        let stderr = &io::stderr();
        self.0.write(stderr, errors).map_err(PyErr::from)?;
        self.0.write(stderr, warnings).map_err(PyErr::from)
    }
}

/// The Python module in Rust.
#[pymodule]
fn beancount_parser_lima(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<BeancountSources>()?;
    m.add_class::<ParseSuccess>()?;
    m.add_class::<ParseError>()?;

    Ok(())
}

fn parse(py: Python<'_>, parser: &lima::BeancountParser) -> PyResult<Py<PyAny>> {
    match parser.parse() {
        Ok(lima::ParseSuccess {
            directives,
            options,
            plugins: _,
            warnings,
        }) => {
            use lima::DirectiveVariant as V;

            let mut c = Converter::new();

            let directives = directives
                .into_iter()
                .map(|d| match d.variant() {
                    V::Transaction(x) => c.transaction(py, d.date(), d.metadata(), x),
                    V::Price(x) => c.price(py, d.date(), d.metadata(), x),
                    V::Balance(x) => c.balance(py, d.date(), d.metadata(), x),
                    V::Open(x) => c.open(py, d.date(), d.metadata(), x),
                    V::Close(x) => c.close(py, d.date(), d.metadata(), x),
                    V::Commodity(x) => c.commodity(py, d.date(), d.metadata(), x),
                    V::Pad(x) => c.pad(py, d.date(), d.metadata(), x),
                    V::Document(x) => c.document(py, d.date(), d.metadata(), x),
                    V::Note(x) => c.note(py, d.date(), d.metadata(), x),
                    V::Event(x) => c.event(py, d.date(), d.metadata(), x),
                    V::Query(x) => c.query(py, d.date(), d.metadata(), x),
                })
                .collect::<PyResult<Vec<Py<PyAny>>>>()?;

            let options = c.options(py, &options)?;

            let warnings = warnings.into_iter().map(Warning::new).collect::<Vec<_>>();

            Ok(Py::new(
                py,
                (
                    ParseSuccess {
                        directives,
                        options,
                        warnings,
                    },
                    ParseResult {},
                ),
            )?
            .into_py(py))
        }

        Err(lima::ParseError { errors, warnings }) => {
            let errors = errors.into_iter().map(Error::new).collect::<Vec<_>>();
            let warnings = warnings.into_iter().map(Warning::new).collect::<Vec<_>>();

            Ok(Py::new(py, (ParseError { errors, warnings }, ParseResult {}))?.into_py(py))
        }
    }
}

/// Success or fail base class
#[derive(Debug)]
#[pyclass(frozen, subclass)]
pub struct ParseResult {}

/// Successful parse result
#[derive(Debug)]
#[pyclass(frozen, extends=ParseResult)]
pub struct ParseSuccess {
    #[pyo3(get)]
    pub(crate) directives: Vec<Py<PyAny>>,
    #[pyo3(get)]
    pub(crate) options: Options,
    // TODO plugins
    #[pyo3(get)]
    pub(crate) warnings: Vec<Warning>,
}

/// Failed parse result
#[derive(Debug)]
#[pyclass(frozen, extends=ParseResult)]
pub struct ParseError {
    #[pyo3(get)]
    pub(crate) errors: Vec<Error>,
    #[pyo3(get)]
    pub(crate) warnings: Vec<Warning>,
}

/// Wrapper for lima::Error
#[derive(Clone, Debug)]
#[pyclass(frozen)]
pub struct Error(lima::Error);

impl Error {
    fn new(e: lima::Error) -> Self {
        Error(e)
    }
}

/// Wrapper for lima::Warning
#[derive(Clone, Debug)]
#[pyclass(frozen)]
pub struct Warning(lima::Warning);

impl Warning {
    fn new(w: lima::Warning) -> Self {
        Warning(w)
    }
}

mod conversions;
use conversions::Converter;
mod types;
