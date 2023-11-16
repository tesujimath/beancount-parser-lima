use super::*;
use ::beancount_parser_lima::format::{
    double_quoted, format, plain, simple_format, NEWLINE_INDENT,
};
use pyo3::{pymethods, PyRef};
use std::{
    borrow::Borrow,
    fmt::{self, Display, Formatter},
};

#[pymethods]
impl Transaction {
    fn __str__(self_: PyRef<'_, Self>) -> String {
        let d = self_.as_ref();

        format!("{}", Fmt(|f| fmt_transaction(self_.borrow(), &d.date, f)))
    }
}

fn fmt_transaction(x: &Transaction, date: &Py<PyDate>, f: &mut Formatter<'_>) -> fmt::Result {
    // write!(f, "{} {}", date, self.flag)?;
    write!(f, "{}", date)?;
    // write!(f, "Transaction")?;

    // format(f, &self.payee, double_quoted, " ", Some(" "))?;
    format(f, &x.narration, double_quoted, " ", Some(" "))?;
    // we prefer to show tags and links inline rather then line by line in metadata
    // metadata.fmt_tags_links_inline(f)?;
    // metadata.fmt_keys_values(f)?;
    format(f, &x.postings, plain, NEWLINE_INDENT, Some(NEWLINE_INDENT))
}

#[pymethods]
impl Posting {
    fn __str__(&self) -> String {
        self.to_string()
    }
}

impl Display for Posting {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // simple_format(f, self.flag, None)?;

        // write!(
        //     f,
        //     "{}{}",
        //     if self.flag.is_some() { " " } else { "" },
        //     &self.account
        // )?;

        // simple_format(f, &self.amount, Some(" "))?;
        simple_format(f, self.currency.as_ref(), Some(" "))
        // simple_format(f, &self.cost_spec, Some(" "))?;
        // simple_format(f, &self.price_annotation, Some(" "))?;

        // self.metadata.fmt(f)
    }
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
