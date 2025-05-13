use super::*;
use lazy_format::lazy_format;
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
    simple_format(f, x.amount.as_ref(), x.flag.as_ref().and(Some(" ")))?;
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
    write!(f, "{} balance {} {}", date, &x.account, x.atol)?;

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
    write!(f, "{} open {} ", date, &x.account)?;
    format(f, x.currencies.bind(py), plain, ",", Some(" "))?;
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
    write!(f, "{} close {}", date, &x.account)?;

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
    write!(f, "{} pad {} {}", date, &x.account, &x.source)?;

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
    write!(f, "{} document {} \"{}\"", date, &x.account, x.path)?;

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
    write!(f, "{} note {} \"{}\"", date, &x.account, x.comment)?;

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
        if let Some(tags) = metadata.tags.as_ref().map(|tags| tags.bind(py)) {
            fmt_tags_inline(tags, f)?;
        }

        if let Some(links) = metadata.links.as_ref().map(|links| links.bind(py)) {
            fmt_links_inline(links, f)?;
        }

        if let Some(kv) = metadata.key_values.as_ref().map(|kv| kv.bind(py)) {
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
    if let Some(tags) = x.tags.as_ref().map(|tags| tags.bind(py)) {
        format(f, tags.iter(), tag, NEWLINE_INDENT, Some(NEWLINE_INDENT))?;
    }

    if let Some(links) = x.links.as_ref().map(|links| links.bind(py)) {
        format(f, links.iter(), link, NEWLINE_INDENT, Some(NEWLINE_INDENT))?;
    }

    if let Some(key_values) = x.key_values.as_ref().map(|kv| kv.bind(py)) {
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

fn fmt_metadata_keys_values(x: &Bound<'_, PyDict>, f: &mut Formatter<'_>) -> fmt::Result {
    format(f, x.iter(), key_value, NEWLINE_INDENT, Some(NEWLINE_INDENT))
}

fn fmt_tags_inline(tags: &Bound<'_, PyList>, f: &mut Formatter<'_>) -> fmt::Result {
    format(f, tags.iter(), tag, SPACE, Some(SPACE))
}

fn fmt_links_inline(links: &Bound<'_, PyList>, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn __str__(&self) -> String {
        format!("{}", Fmt(|f| fmt_meta_value_account(self, f)))
    }
}

fn fmt_meta_value_account(x: &MetaValueAccount, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", &x.value)
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
impl PriceSpec {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for PriceSpec {
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

#[pymethods]
impl Options {
    fn __str__(self_: PyRef<'_, Self>, py: Python<'_>) -> String {
        format!("{}", Fmt(|f| fmt_options(self_.borrow(), py, f)))
    }
}

fn fmt_options(x: &Options, py: Python<'_>, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "title: {}", x.title)?;

    fmt_account_option("account_previous_balances", &x.account_previous_balances, f)?;

    fmt_account_option("account_previous_earnings", &x.account_previous_earnings, f)?;

    fmt_account_option(
        "account_previous_conversions",
        &x.account_previous_conversions,
        f,
    )?;

    fmt_account_option("account_current_earnings", &x.account_current_earnings, f)?;

    fmt_account_option(
        "account_current_conversions",
        &x.account_current_conversions,
        f,
    )?;

    fmt_account_option("account_unrealized_gains", &x.account_unrealized_gains, f)?;

    if let Some(account_rounding) = x.account_rounding.as_ref() {
        fmt_account_option("account_rounding", account_rounding, f)?;
    }

    write!(f, "\nconversion_currency: {}", x.conversion_currency)?;

    format(
        f,
        x.inferred_tolerance_default.bind(py).iter(),
        |(c, d)| format!("{}={}", c, d),
        ",",
        Some("\ninferred_tolerance_default: "),
    )?;

    write!(
        f,
        "\ninferred_tolerance_multiplier: {}",
        x.inferred_tolerance_multiplier
    )?;

    write!(
        f,
        "\ninfer_tolerance_from_cost: {}",
        x.infer_tolerance_from_cost
    )?;

    format(
        f,
        x.documents.bind(py).iter(),
        plain,
        ",",
        Some("\ndocuments: "),
    )?;

    format(
        f,
        x.operating_currency.bind(py).iter(),
        plain,
        ",",
        Some("\noperating_currency: "),
    )?;

    write!(f, "\nrender_commas: {}", x.render_commas)?;
    write!(f, "\nbooking_method: {}", x.booking_method)?;
    write!(f, "\nplugin_processing_mode: {}", x.plugin_processing_mode)?;

    format(
        f,
        x.account_name_by_type.bind(py).iter(),
        |(t, n)| format!("{}={}", t, n),
        ",",
        Some("\naccount_name_by_type: "),
    )?;

    format(
        f,
        x.account_type_by_name.bind(py).iter(),
        |(n, t)| format!("{}={}", n, t),
        ",",
        Some("\naccount_type_by_name: "),
    )?;

    write!(f, "\nlong_string_maxlines: {}", x.long_string_maxlines)?;

    Ok(())
}

fn fmt_account_option(option_name: &str, x: &Py<PyString>, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "\n{}: {}", option_name, x)
}

fn fmt_account(x: &Py<PyString>, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{}", x)
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

#[pymethods]
impl Plugin {
    fn __str__(&self, _py: Python<'_>) -> String {
        self.to_string()
    }
}

impl Display for Plugin {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "plugin \"{}\"", self.module_name)?;
        if let Some(config) = self.config.as_ref() {
            writeln!(f, " \"{}\"", config)?;
        } else {
            writeln!(f)?;
        }

        Ok(())
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
