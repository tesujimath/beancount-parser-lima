#![cfg(test)]
use super::*;
use test_case::test_case;
use {rust_decimal::Decimal, rust_decimal_macros::dec};

#[test_case("MyAccountName", Ok("MyAccountName"))]
#[test_case("", Err(AccountNameErrorKind::Empty))]
#[test_case("dyAccountName", Err(AccountNameErrorKind::Initial('d')))]
#[test_case("My-Sub=Account?Bad", Err(AccountNameErrorKind::Subsequent(vec!['=', '?'])))]
fn test_account_name_try_from(s: &str, expected_raw: Result<&str, AccountNameErrorKind>) {
    let result = AccountName::try_from(s);
    let expected = match expected_raw {
        Ok(s) => Ok(AccountName(s)),
        Err(e) => Err(AccountNameError(e)),
    };
    // visually check the error display by making a bad test case
    if let Err(ref e) = result {
        println!("{}", e);
    }
    assert_eq!(result, expected);
}

use CurrencyErrorKind::*;
#[test_case("GBP", Ok("GBP"))]
#[test_case("AAPL", Ok("AAPL"))] // stock
#[test_case("V", Ok("V"))] // single-character stock
#[test_case("NT.TO", Ok("NT.TO"))] // stock on another market
#[test_case("TLT_040921C144", Ok("TLT_040921C144"))] // equity option
#[test_case("/6J", Ok("/6J"))] // currency futures
#[test_case("/6'J", Ok("/6'J"))] // currency futures
#[test_case("/NQH21", Ok("/NQH21"))] // commodity futures
#[test_case("/NQH21-EXT", Ok("/NQH21-EXT"))] // commodity futures
#[test_case("/NQH21_QNEG21C13100", Ok("/NQH21_QNEG21C13100"))] // futures option
#[test_case("/6.3", Err(MissingLetter))]
#[test_case("CAC_", Err(Final('_')))]
#[test_case("abc", Err(Initial('a')))]
#[test_case("A?=.-BJ", Err(Intermediate(vec!['?', '='])))]
#[test_case("", Err(Empty))]
fn test_currency_try_from(s: &str, expected: Result<&str, CurrencyErrorKind>) {
    match (Currency::try_from(s), expected) {
        (Ok(actual), Ok(expected)) => assert_eq!(actual, Currency(expected)),
        (Err(actual), Err(kind)) => assert_eq!(actual, CurrencyError(kind)),
        (Err(actual), Ok(_)) => panic!("unexpected failure: {}", actual),
        (Ok(actual), Err(_)) => panic!("unexpected success: {}", actual),
    }
}

#[test_case("a-b-c-d", Ok("a-b-c-d"))]
#[test_case("a=b?c,d-e", Err(TagOrLinkIdentifierError(vec!['=', '?', ','])))]
fn test_tag_or_link_identifier_try_from(
    s: &str,
    expected_raw: Result<&str, TagOrLinkIdentifierError>,
) {
    let result = TagOrLinkIdentifier::try_from(s);
    let expected = match expected_raw {
        Ok(s) => Ok(TagOrLinkIdentifier(s)),
        Err(e) => Err(e),
    };
    // visually check the error display by making a bad test case
    if let Err(ref e) = result {
        println!("{}", e);
    }
    assert_eq!(result, expected);
}

use Expr::*;
#[test_case(Add(Box::new(Value(dec!(12.01))), Box::new(Value(dec!(1.5)))), dec!(13.51))]
#[test_case(Div(Box::new(Value(dec!(1.00))), Box::new(Value(dec!(3)))), dec!(0.33))]
fn test_expr_value_from_expr(expr: Expr, value: Decimal) {
    let expr_value = ExprValue::from(expr);
    assert_eq!(expr_value.value, value);
}
