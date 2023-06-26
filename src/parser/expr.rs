use super::super::*;
use super::*;

#[cfg(test)]
use {rust_decimal::Decimal, rust_decimal_macros::dec};

#[cfg(test)]
use test_case::test_case;

/// Match an expression
pub fn expr<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char, Span>>> {
    product().foldl(
        choice((
            op('+').to(Expr::Add as fn(_, _) -> _),
            op('-').to(Expr::Sub as fn(_, _) -> _),
        ))
        .then(product())
        .repeated(),
        |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
    )
}

#[cfg(test)]
#[test_case("1 + 2 *  3", "(1 + (2 * 3))")]
#[test_case("1 + 2 *  3 / 4 - 5", "((1 + ((2 * 3) / 4)) - 5)")]
#[test_case("72 / 2 / 3", "((72 / 2) / 3)")]
#[test_case("10 - 1", "(10 - 1)")]
#[test_case("10 - -2", "(10 - -2)")]
fn expr_test(s: &str, expected: &str) {
    assert_eq!(
        expr().map(|x| format!("{:?}", x)).parse(s).into_result(),
        Ok(expected.to_owned())
    )
}

/// Match a product of factors
pub fn product<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char, Span>>> {
    factor().foldl(
        choice((
            op('*').to(Expr::Mul as fn(_, _) -> _),
            op('/').to(Expr::Div as fn(_, _) -> _),
        ))
        .then(factor())
        .repeated(),
        |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
    )
}

/// Match the specified operator
pub fn op<'src>(c: char) -> impl Parser<'src, &'src str, (), extra::Err<Rich<'src, char, Span>>> {
    just(c).ignored().padded()
}

/// Match a factor of an expression
pub fn factor<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char, Span>>> {
    value().or(parens())
}

/// Match a parenthesized expression
pub fn parens<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char, Span>>> {
    value()
        .padded()
        .delimited_by(just('('), just(')'))
        .map(|x| Expr::Paren(Box::new(x)))
}

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
