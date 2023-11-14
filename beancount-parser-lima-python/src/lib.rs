use beancount_parser_lima::{BeancountParser, BeancountSources, ParseResult};
use pyo3::exceptions::PyException;
use pyo3::{create_exception, prelude::*};
use std::io::{self, prelude::*};
use std::path::PathBuf;

/// Integrated all-in-one parse.
#[pyfunction]
fn parse(path: &str) -> PyResult<String> {
    let mut stderr = &io::stderr();

    let sources = BeancountSources::new(PathBuf::from(path));
    let parser = BeancountParser::new(&sources);
    writeln!(stderr, "{:?}", &sources).unwrap();

    parse_sources(&sources, &parser, stderr)
        .map_err(|n_errors| ParseError::new_err(format!("{} errors", n_errors)))
}

/// The Python module in Rust.
#[pymodule]
fn beancount_parser_lima(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(parse, m)?)?;
    Ok(())
}

fn parse_sources<W>(
    sources: &BeancountSources,
    parser: &BeancountParser,
    error_w: W,
) -> Result<String, usize>
where
    W: Write + Copy,
{
    match parser.parse() {
        Ok(ParseResult {
            directives,
            options: _,
            warnings,
        }) => {
            for directive in directives {
                println!("{}\n", &directive);
            }

            sources.write(error_w, warnings).unwrap();

            Ok("Yay!".to_owned())
        }
        Err(beancount_parser_lima::ParseError { errors, warnings }) => {
            let n_errors = errors.len();

            sources.write(error_w, errors).unwrap();
            sources.write(error_w, warnings).unwrap();
            Err(n_errors)
        }
    }
}

create_exception!(
    beancount_parser_lima,
    ParseError,
    PyException,
    "Parse error"
);
