use super::*;
use lazy_format::lazy_format;
use pyo3::{pymethods, PyRef, Python};
use std::{
    borrow::Borrow,
    fmt::{self, Display, Formatter},
};

#[pymethods]
impl Transaction {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_transaction(self_.borrow(), py, &d.date, f))
        )
    }
}

fn fmt_transaction(
    x: &Transaction,
    py: Python<'_>,
    date: &Py<PyDate>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} {}", date, x.flag)?;
    format(f, &x.payee, double_quoted, " ", Some(" "))?;
    format(f, &x.narration, double_quoted, " ", Some(" "))?;
    // we prefer to show tags and links inline rather then line by line in metadata
    // metadata.fmt_tags_links_inline(f)?;
    // metadata.fmt_keys_values(f)?;
    for posting in &x.postings {
        f.write_str(NEWLINE_INDENT)?;
        fmt_posting(posting, py, f)?;
    }
    f.write_str(NEWLINE)
}

#[pymethods]
impl Posting {
    fn __str__(&self, py: Python<'_>) -> String {
        format!("{}", Fmt(|f| fmt_posting(self, py, f)))
    }
}

fn fmt_posting(x: &Posting, py: Python<'_>, f: &mut Formatter<'_>) -> fmt::Result {
    simple_format(f, x.flag.as_ref(), None)?;

    format(
        f,
        x.account.as_ref(py),
        plain,
        ":",
        x.flag.as_ref().and(Some(" ")),
    )?;
    // write!(
    //     f,
    //     "{}{}",
    //     "", // TODO if self.flag.is_some() { " " } else { "" },
    //     &self.account
    // )?;

    simple_format(f, x.amount.as_ref(), Some(" "))?;
    simple_format(f, x.currency.as_ref(), Some(" "))
    // simple_format(f, &self.cost_spec, Some(" "))?;
    // simple_format(f, &self.price_annotation, Some(" "))?;

    // self.metadata.fmt(f)
}

struct Fmt<F>(pub F)
where
    F: Fn(&mut fmt::Formatter) -> fmt::Result;

impl<F> Display for Fmt<F>
where
    F: Fn(&mut Formatter) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.0)(f)
    }
}

/// Format the given container, with optional prefix, applying `mapper` to each element,
/// and with the given `separator`.
fn format<C, T, M, D>(
    f: &mut Formatter<'_>,
    container: C,
    mapper: M,
    separator: &'static str,
    prefix: Option<&'static str>,
) -> fmt::Result
where
    C: IntoIterator<Item = T>,
    M: Fn(T) -> D,
    D: Display,
{
    let mut container = container.into_iter();
    if let Some(item) = container.by_ref().next() {
        if let Some(prefix) = prefix {
            f.write_str(prefix)?;
        }

        mapper(item).fmt(f)?;
    }

    for item in container {
        f.write_str(separator)?;
        mapper(item).fmt(f)?;
    }

    Ok(())
}

/// Simple format with no mapper or separator.
fn simple_format<C, T>(
    f: &mut Formatter<'_>,
    container: C,
    prefix: Option<&'static str>,
) -> fmt::Result
where
    C: IntoIterator<Item = T>,
    T: Display,
{
    format(f, container, plain, "", prefix)
}

/// Format plain.
fn plain<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("{s}")
}

/// Format in single quotes.
fn single_quoted<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("'{s}'")
}

/// Format in double quotes.
fn double_quoted<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("\"{s}\"")
}

/// Format key/value.
fn key_value<K, V>(kv: (K, V)) -> impl Display
where
    K: Display,
    V: Display,
{
    lazy_format!("{}: {}", kv.0, kv.1)
}

fn pad_if(condition: bool) -> &'static str {
    if condition {
        " "
    } else {
        ""
    }
}

/// A single space.
const SPACE: &str = " ";

/// A single newline.
const NEWLINE: &str = "\n";

/// Standard indent.
const INDENT: &str = "  ";

/// Newline followed by indent.
const NEWLINE_INDENT: &str = "\n  ";
