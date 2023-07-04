use super::super::lexer::Token;
use super::*;

#[cfg(test)]
use super::super::lexer::bare_lex;
#[cfg(test)]
use chumsky::input::BorrowInput;
#[cfg(test)]
use test_case::test_case;

/// Match an expression
pub fn expr<'src, I>() -> impl Parser<'src, I, Expr, extra::Err<ParserError<'src>>>
where
    I: BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan>,
{
    use Token::*;

    recursive(|expr| {
        // Match a parenthesized expression
        let parens = expr
            .clone()
            .delimited_by(just(Lparen), just(Rparen))
            .map(|x| Expr::Paren(Box::new(x)));

        // Match a negated expression
        let negated = just(Minus)
            .ignore_then(expr)
            .map(|x| Expr::Neg(Box::new(x)));

        // Match a bare number
        let number = select_ref! { Number(x) => Expr::Value(*x) };

        // Match a factor of an expression
        let factor = number.or(negated).or(parens.clone());

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
#[test_case("1 + 2 *  3", "(1 + (2 * 3))")]
#[test_case("1 + 2 *  3 / 4 - 5", "((1 + ((2 * 3) / 4)) - 5)")]
#[test_case("(1 + 2) *  3 / (4 - 6)", "(([(1 + 2)] * 3) / [(4 - 6)])")]
#[test_case("72 / 2 / 3", "((72 / 2) / 3)")]
#[test_case("10 - 1", "(10 - 1)")]
#[test_case("10 - -2", "(10 - (-2))")]
#[test_case("6 - --7", "(6 - (-(-7)))")]
#[test_case("4 + 2 *  3 XYZ", "(4 + (2 * 3))")]
#[test_case("4 + 2 *  3 # freddy", "(4 + (2 * 3))")]
#[test_case("2.718 #", "2.718")]
#[test_case("3.141 # pi", "3.141")]
fn expr_test(s: &str, expected: &str) {
    let tokens = bare_lex(s);
    let spanned = tokens.spanned(end_of_input(s));

    let result = expr()
        .map(|x| format!("{:?}", x))
        .then_ignore(any().repeated().collect::<Vec<Token>>())
        .parse(spanned)
        .into_result();

    assert_eq!(result, Ok(expected.to_owned()))
}
