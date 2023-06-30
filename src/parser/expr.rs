use super::super::lexer::Token;
use super::super::*;
use super::*;

#[cfg(test)]
use {rust_decimal::Decimal, rust_decimal_macros::dec};

use chumsky::input::ValueInput;
#[cfg(test)]
use test_case::test_case;

/// Match an expression
pub fn expr<'src, I>() -> impl Parser<'src, I, Expr, ParserError<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use Token::*;

    recursive(|expr| {
        // Match a parenthesized expression
        let parens = expr
            .delimited_by(just(Lparen), just(Rparen))
            .map(|x| Expr::Paren(Box::new(x)));

        // Match a bare number
        let number = select! { Number(x) => Expr::Value(x) };

        // Match a factor of an expression
        let factor = number.or(parens.clone());

        // Match a product of factors
        let product = factor.clone().foldl(
            choice((
                just(Asterisk).to(Expr::Mul as fn(_, _) -> _),
                just(Slash).to(Expr::Div as fn(_, _) -> _),
            ))
            .then(factor.clone())
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        // Match an expression
        product.clone().foldl(
            choice((
                just(Plus).to(Expr::Add as fn(_, _) -> _),
                just(Minus).to(Expr::Sub as fn(_, _) -> _),
            ))
            .then(product.clone())
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        )
    })
}

#[cfg(test)]
#[test_case("1 + 2 *  3", "(1 + (2 * 3))", "")]
#[test_case("1 + 2 *  3 / 4 - 5", "((1 + ((2 * 3) / 4)) - 5)", "")]
#[test_case("(1 + 2) *  3 / (4 - 6)", "(([(1 + 2)] * 3) / [(4 - 6)])", "")]
#[test_case("72 / 2 / 3", "((72 / 2) / 3)", "")]
#[test_case("10 - 1", "(10 - 1)", "")]
#[test_case("10 - -2", "(10 - -2)", "")]
#[test_case("4 + 2 *  3 XYZ", "(4 + (2 * 3))", " XYZ")]
#[test_case("4 + 2 *  3 # freddy", "(4 + (2 * 3))", " # freddy")]
#[test_case("2.718 #", "2.718", " #")]
#[test_case("3.141 # pi", "3.141", " # pi")]
fn expr_test(s: &str, expected: &str, expected_unparsed: &str) {
    let token_iter = lex(s).map(|(tok, span)| (tok, SimpleSpan::from(span)));
    let token_stream = Stream::from_iter(token_iter).spanned((s.len()..s.len()).into());

    match expr()
        .map(|x| format!("{:?}", x))
        .then(any().repeated().collect::<Vec<Token>>())
        .parse(token_stream)
        .into_result()
    {
        Ok((value, unparsed)) => assert_eq!(value, expected.to_owned()),
        Err(e) => panic!("oops"),
    }

    // assert_eq!(
    //     result,
    //     Ok((expected.to_owned(), expected_unparsed.to_owned()))
    // )
}

// #[cfg(test)]
// #[test_case("123,456,789", dec!(123456789))]
// #[test_case("-123,456,789.12", dec!(-123456789.12))]
// fn value_test(s: &str, expected: Decimal) {
//     assert_eq!(expr().parse(s).into_result(), Ok(Expr::Value(expected)));
// }
