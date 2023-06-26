use super::super::*;
use super::*;

#[cfg(test)]
use {rust_decimal::Decimal, rust_decimal_macros::dec};

#[cfg(test)]
use test_case::test_case;

/// Match an expression
pub fn expr<'src>() -> impl Parser<'src, &'src str, Expr, extra::Err<Rich<'src, char, Span>>> {
    recursive(|expr| {
        // Match a number with optional thousands separators and optional decimal point and fractional part
        let digit =
            any::<&'src str, extra::Err<Rich<'src, char, Span>>>().filter(char::is_ascii_digit);

        let value = just('-')
            .or_not()
            .then(
                digit
                    .repeated()
                    .at_least(1)
                    .then(just(',').then(digit.then(digit).then(digit)).repeated())
                    .then((just('.').then(digit.repeated().at_least(1))).or_not()),
            )
            .slice()
            .try_map(|s: &'src str, span| {
                let mut without_commas = s.to_string();
                without_commas.retain(|c| c != ',');
                FromStr::from_str(&without_commas)
                    .map(Expr::Value)
                    .map_err(|e| chumsky::error::Rich::custom(span, e))
            });

        // Match a parenthesized expression
        let parens = expr
            .padded()
            .delimited_by(just('('), just(')'))
            .map(|x| Expr::Paren(Box::new(x)));

        // Match a factor of an expression
        let factor = value.or(parens.clone());

        // Match the specified operator
        let op = |c| just(c).ignored().padded();

        // Match a product of factors
        let product = factor.clone().foldl(
            choice((
                op('*').to(Expr::Mul as fn(_, _) -> _),
                op('/').to(Expr::Div as fn(_, _) -> _),
            ))
            .then(factor.clone())
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        // Match an expression
        product.clone().foldl(
            choice((
                op('+').to(Expr::Add as fn(_, _) -> _),
                op('-').to(Expr::Sub as fn(_, _) -> _),
            ))
            .then(product.clone())
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        )
    })
}

#[cfg(test)]
#[test_case("1 + 2 *  3", "(1 + (2 * 3))")]
#[test_case("1 + 2 *  3 / 4 - 5", "((1 + ((2 * 3) / 4)) - 5)")]
#[test_case("(1 + 2) *  3 / (4 - 6)", "(([(1 + 2)] * 3) / [(4 - 6)])")]
#[test_case("72 / 2 / 3", "((72 / 2) / 3)")]
#[test_case("10 - 1", "(10 - 1)")]
#[test_case("10 - -2", "(10 - -2)")]
fn expr_test(s: &str, expected: &str) {
    assert_eq!(
        expr().map(|x| format!("{:?}", x)).parse(s).into_result(),
        Ok(expected.to_owned())
    )
}

#[cfg(test)]
#[test_case("123,456,789", dec!(123456789))]
#[test_case("-123,456,789.12", dec!(-123456789.12))]
fn value_test(s: &str, expected: Decimal) {
    assert_eq!(expr().parse(s).into_result(), Ok(Expr::Value(expected)));
}
