use super::*;
use ::beancount_parser_lima::format::{
    double_quoted, format, plain, simple_format, NEWLINE_INDENT,
};
use beancount_parser_lima::format::NEWLINE;
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
    // write!(f, "{} {}", date, self.flag)?;
    write!(f, "{}", date)?;
    // write!(f, "Transaction")?;

    // format(f, &self.payee, double_quoted, " ", Some(" "))?;
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
    // simple_format(f, self.flag, None)?;

    format(
        f,
        x.account.as_ref(py),
        plain,
        ":",  // TODO if self.flag.is_some() { " " } else { "" },
        None, // TODO if self.flag.is_some() { " " } else { "" },
    )?;
    // write!(
    //     f,
    //     "{}{}",
    //     "", // TODO if self.flag.is_some() { " " } else { "" },
    //     &self.account
    // )?;

    // simple_format(f, &self.amount, Some(" "))?;
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
