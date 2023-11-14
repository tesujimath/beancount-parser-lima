
use pyo3::prelude::*;

/// Formats the sum of two numbers as string.
#[pyfunction]
fn parse(a: usize, b: usize) -> PyResult<String> {
    Ok((a + b).to_string())
}

/// A Python module implemented in Rust.
#[pymodule]
fn beancount_parser_lima(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(parse, m)?)?;
    Ok(())
}
