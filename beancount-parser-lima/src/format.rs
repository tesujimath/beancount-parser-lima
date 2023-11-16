//! Formatting routines for display of Beancount entities.
//! Only really public for the sake of the `beancount-parser-lima-python` crate.

use lazy_format::lazy_format;
use std::fmt::{self, Display, Formatter};

/// Format the given container, with optional prefix, applying `mapper` to each element,
/// and with the given `separator`.
pub fn format<C, T, M, D>(
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
pub fn simple_format<C, T>(
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
pub fn plain<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("{s}")
}

/// Format in single quotes.
pub fn single_quoted<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("'{s}'")
}

/// Format in double quotes.
pub fn double_quoted<S>(s: S) -> impl Display
where
    S: Display,
{
    lazy_format!("\"{s}\"")
}

/// Format key/value.
pub fn key_value<K, V>(kv: (K, V)) -> impl Display
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
pub const SPACE: &str = " ";

/// A single newline.
pub const NEWLINE: &str = "\n";

/// Standard indent.
pub const INDENT: &str = "  ";

/// Newline followed by indent.
pub const NEWLINE_INDENT: &str = "\n  ";
