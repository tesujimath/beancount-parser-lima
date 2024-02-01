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
            Fmt(|f| fmt_transaction(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_transaction(
    x: &Transaction,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} {}", date, x.flag)?;
    format(f, &x.payee, double_quoted, " ", Some(" "))?;
    format(f, &x.narration, double_quoted, " ", Some(" "))?;

    fmt_optional_metadata_inline(metadata, py, f)?;

    for posting in &x.postings {
        f.write_str(NEWLINE_INDENT)?;
        fmt_posting(posting, py, f)?;
    }

    Ok(())
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

    simple_format(f, x.amount.as_ref(), Some(" "))?;
    simple_format(f, x.currency.as_ref(), Some(" "))?;

    format(f, &x.cost_spec, plain, " ", Some(" "))?;
    format(f, &x.price_annotation, plain, " ", Some(" @ "))?;

    fmt_optional_metadata(&x.metadata, py, f)?;

    Ok(())
}

#[pymethods]
impl Price {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_price(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_price(
    x: &Price,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} price {} {}", date, x.currency, &x.amount)?;

    fmt_optional_metadata_inline(metadata, py, f)
}

#[pymethods]
impl Balance {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_balance(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_balance(
    x: &Balance,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} balance ", date,)?;
    fmt_account(&x.account, py, f)?;
    write!(f, " {}", x.atol)?;

    fmt_optional_metadata_inline(metadata, py, f)
}

#[pymethods]
impl Open {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_open(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_open(
    x: &Open,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} open ", date)?;
    fmt_account(&x.account, py, f)?;
    format(f, x.currencies.as_ref(py), plain, ",", Some(" "))?;
    format(f, &x.booking, double_quoted, " ", Some(" "))?;

    fmt_optional_metadata_inline(metadata, py, f)
}

#[pymethods]
impl Close {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_close(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_close(
    x: &Close,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} close ", date)?;
    fmt_account(&x.account, py, f)?;

    fmt_optional_metadata_inline(metadata, py, f)
}

#[pymethods]
impl Commodity {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_commodity(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_commodity(
    x: &Commodity,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} commodity {}", date, x.currency)?;

    fmt_optional_metadata_inline(metadata, py, f)
}

#[pymethods]
impl Pad {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_pad(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_pad(
    x: &Pad,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} pad ", date)?;
    fmt_account(&x.account, py, f)?;
    f.write_str(" ")?;
    fmt_account(&x.source, py, f)?;

    fmt_optional_metadata_inline(metadata, py, f)
}

#[pymethods]
impl Document {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_document(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_document(
    x: &Document,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} document ", date)?;
    fmt_account(&x.account, py, f)?;
    write!(f, " \"{}\"", x.path)?;

    fmt_optional_metadata_inline(metadata, py, f)
}

#[pymethods]
impl Note {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_note(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_note(
    x: &Note,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} note ", date)?;
    fmt_account(&x.account, py, f)?;
    write!(f, " \"{}\"", x.comment)?;

    fmt_optional_metadata_inline(metadata, py, f)
}

#[pymethods]
impl Event {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_event(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_event(
    x: &Event,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(
        f,
        "{} event \"{}\" \"{}\"",
        date, &x.event_type, &x.description
    )?;

    fmt_optional_metadata_inline(metadata, py, f)
}

#[pymethods]
impl Query {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        let d = self_.as_ref();

        format!(
            "{}",
            Fmt(|f| fmt_query(self_.borrow(), py, &d.date, &d.metadata, f))
        )
    }
}

fn fmt_query(
    x: &Query,
    py: Python<'_>,
    date: &Py<PyDate>,
    metadata: &Option<Metadata>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    write!(f, "{} query \"{}\" \"{}\"", date, &x.name, &x.content)?;

    fmt_optional_metadata_inline(metadata, py, f)
}

#[pymethods]
impl Metadata {
    fn __str__(&self, py: Python<'_>) -> String {
        format!("{}", Fmt(|f| fmt_metadata(self, py, f)))
    }
}

fn fmt_optional_metadata_inline(
    x: &Option<Metadata>,
    py: Python<'_>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    if let Some(metadata) = x.as_ref() {
        if let Some(tags) = metadata.tags.as_ref().map(|tags| tags.as_ref(py)) {
            fmt_tags_inline(tags, f)?;
        }

        if let Some(links) = metadata.links.as_ref().map(|links| links.as_ref(py)) {
            fmt_links_inline(links, f)?;
        }

        if let Some(kv) = metadata.key_values.as_ref().map(|kv| kv.as_ref(py)) {
            fmt_metadata_keys_values(kv, f)?;
        }
    }

    Ok(())
}

fn fmt_optional_metadata(
    x: &Option<Metadata>,
    py: Python<'_>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    if let Some(metadata) = x.as_ref() {
        fmt_metadata(metadata, py, f)
    } else {
        Ok(())
    }
}

fn fmt_metadata(x: &Metadata, py: Python<'_>, f: &mut Formatter<'_>) -> fmt::Result {
    if let Some(tags) = x.tags.as_ref().map(|tags| tags.as_ref(py)) {
        format(f, tags.iter(), tag, NEWLINE_INDENT, Some(NEWLINE_INDENT))?;
    }

    if let Some(links) = x.links.as_ref().map(|links| links.as_ref(py)) {
        format(f, links.iter(), link, NEWLINE_INDENT, Some(NEWLINE_INDENT))?;
    }

    if let Some(key_values) = x.key_values.as_ref().map(|kv| kv.as_ref(py)) {
        format(
            f,
            key_values.iter(),
            key_value,
            NEWLINE_INDENT,
            Some(NEWLINE_INDENT),
        )?;
    }

    Ok(())
}

fn fmt_metadata_keys_values(x: &PyDict, f: &mut Formatter<'_>) -> fmt::Result {
    format(f, x.iter(), key_value, NEWLINE_INDENT, Some(NEWLINE_INDENT))
}

fn fmt_tags_inline(tags: &PyList, f: &mut Formatter<'_>) -> fmt::Result {
    format(f, tags.iter(), tag, SPACE, Some(SPACE))
}

fn fmt_links_inline(links: &PyList, f: &mut Formatter<'_>) -> fmt::Result {
    format(f, links.iter(), link, SPACE, Some(SPACE))
}

#[pymethods]
impl MetaValueAmount {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for MetaValueAmount {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.value)
    }
}

#[pymethods]
impl MetaValueString {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for MetaValueString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

#[pymethods]
impl MetaValueCurrency {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for MetaValueCurrency {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[pymethods]
impl MetaValueAccount {
    fn __str__(&self, py: Python<'_>) -> String {
        format!("{}", Fmt(|f| fmt_meta_value_account(self, py, f)))
    }
}

fn fmt_meta_value_account(
    x: &MetaValueAccount,
    py: Python<'_>,
    f: &mut Formatter<'_>,
) -> fmt::Result {
    fmt_account(&x.value, py, f)
}

#[pymethods]
impl MetaValueTag {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for MetaValueTag {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.value)
    }
}

#[pymethods]
impl MetaValueLink {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for MetaValueLink {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "^{}", self.value)
    }
}

#[pymethods]
impl MetaValueDate {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for MetaValueDate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[pymethods]
impl MetaValueBool {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for MetaValueBool {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(if self.value { "TRUE" } else { "FALSE" })
    }
}

#[pymethods]
impl MetaValueNone {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for MetaValueNone {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

#[pymethods]
impl MetaValueExpr {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for MetaValueExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[pymethods]
impl Amount {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for Amount {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", &self.number, &self.currency)
    }
}

#[pymethods]
impl AmountWithTolerance {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for AmountWithTolerance {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(tolerance) = self.tolerance {
            write!(f, "{} ~ {}", &self.amount, tolerance)
        } else {
            write!(f, "{}", &self.amount)
        }
    }
}

#[pymethods]
impl CostSpec {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for CostSpec {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut prefix = "";
        let space = " ";

        f.write_str("{")?;

        if let Some(per_unit) = &self.per_unit {
            write!(f, "{}{}", prefix, per_unit)?;
            prefix = space;
        }

        if let Some(total) = &self.total {
            write!(f, "{}# {}", prefix, total)?;
            prefix = space;
        }

        if let Some(currency) = &self.currency {
            write!(f, "{}{}", prefix, currency)?;
            prefix = space;
        }

        if let Some(date) = &self.date {
            write!(f, "{}{}", prefix, date)?;
            prefix = space;
        }

        if let Some(label) = &self.label {
            write!(f, "{}\"{}\"", prefix, label)?;
            prefix = space;
        }

        if self.merge {
            write!(f, "{}*", prefix)?;
        }

        f.write_str("}")
    }
}

#[pymethods]
impl ScopedAmount {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for ScopedAmount {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut prefix = "";
        let space = " ";

        if let Some(per_unit) = &self.per_unit {
            write!(f, "{}{}", prefix, per_unit)?;
            prefix = space;
        }

        if let Some(total) = &self.total {
            write!(f, "{}# {}", prefix, total)?;
            prefix = space;
        }

        if let Some(currency) = &self.currency {
            write!(f, "{}{}", prefix, currency)?;
        }

        Ok(())
    }
}

fn fmt_account(x: &Py<PyList>, py: Python<'_>, f: &mut Formatter<'_>) -> fmt::Result {
    format(f, x.as_ref(py).iter(), plain, ":", None)
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

/// Format as tag.
fn tag<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("#{s}")
}

/// Format as link.
fn link<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("^{s}")
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
