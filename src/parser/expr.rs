use super::super::*;
use super::*;

#[cfg(test)]
use {rust_decimal::Decimal, rust_decimal_macros::dec};

#[cfg(test)]
use test_case::test_case;

/// Match a single digit
pub fn digit<'src>() -> impl Parser<'src, &'src str, char, extra::Err<Rich<'src, char, Span>>> {
    any().filter(char::is_ascii_digit)
}

/// Match a number with optional thousands separators and optional decimal point and fractional part
pub fn value<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char, Span>>> {
    just('-')
        .or_not()
        .then(
            digit()
                .repeated()
                .at_least(1)
                .then(
                    just(',')
                        .then(digit().then(digit()).then(digit()))
                        .repeated(),
                )
                .then((just('.').then(digit().repeated().at_least(1))).or_not()),
        )
        .slice()
        .try_map(|s, span| {
            let mut without_commas = s.to_string();
            without_commas.retain(|c| c != ',');
            FromStr::from_str(&without_commas)
                .map(Expr::Value)
                .map_err(|e| chumsky::error::Rich::custom(span, e))
        })
}

#[cfg(test)]
#[test_case("123,456,789", dec!(123456789))]
#[test_case("-123,456,789.12", dec!(-123456789.12))]
fn value_test(s: &str, expected: Decimal) {
    assert_eq!(value().parse(s).into_result(), Ok(Expr::Value(expected)));
}
