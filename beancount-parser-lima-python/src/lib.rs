use beancount_parser_lima::{BeancountParser, BeancountSources, DirectiveVariant, ParseResult};
use pyo3::{create_exception, exceptions::PyException, prelude::*};
use std::io::{self, prelude::*};
use std::path::PathBuf;

/// Integrated all-in-one parse.
#[pyfunction]
fn parse(py: Python<'_>, path: &str) -> PyResult<Vec<Py<PyAny>>> {
    let mut stderr = &io::stderr();

    let sources = BeancountSources::new(PathBuf::from(path));
    let parser = BeancountParser::new(&sources);
    writeln!(stderr, "{:?}", &sources).unwrap();

    parse_sources(py, &sources, &parser, stderr)
}

/// The Python module in Rust.
#[pymodule]
fn beancount_parser_lima(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(parse, m)?)?;
    Ok(())
}

fn parse_sources<W>(
    py: Python<'_>,
    sources: &BeancountSources,
    parser: &BeancountParser,
    error_w: W,
) -> PyResult<Vec<Py<PyAny>>>
where
    W: Write + Copy,
{
    match parser.parse() {
        Ok(ParseResult {
            directives,
            options: _,
            warnings,
        }) => {
            use DirectiveVariant as V;

            let mut c = Converter::new();

            sources.write(error_w, warnings).unwrap();

            directives
                .into_iter()
                .filter_map(|d| match d.variant() {
                    V::Transaction(x) => Some(c.transaction(py, d.date(), x)),
                    _ => None,
                })
                .collect::<PyResult<Vec<Py<PyAny>>>>()
        }
        Err(beancount_parser_lima::ParseError { errors, warnings }) => {
            let n_errors = errors.len();

            sources.write(error_w, errors).unwrap();
            sources.write(error_w, warnings).unwrap();
            Err(ParseError::new_err(format!("{} errors", n_errors)))
        }
    }
}

create_exception!(
    beancount_parser_lima,
    ParseError,
    PyException,
    "Parse error"
);

mod conversions;
use conversions::Converter;
mod types;
