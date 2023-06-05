#![cfg(test)]
use super::*;
use test_case::test_case;
use {rust_decimal::Decimal, rust_decimal_macros::dec};

#[test_case("MySubAccount", Ok("MySubAccount"))]
#[test_case("", Err(SubAccountErrorKind::Empty))]
#[test_case("dySubAccount", Err(SubAccountErrorKind::Initial('d')))]
#[test_case("My-Sub=Account?Bad", Err(SubAccountErrorKind::Subsequent(vec!['=', '?'])))]
fn test_subaccount_from_str(s: &str, expected_raw: Result<&str, SubAccountErrorKind>) {
    let result = SubAccount::from_str(s);
    let expected = match expected_raw {
        Ok(s) => Ok(SubAccount(s.to_string())),
        Err(e) => Err(SubAccountError(e)),
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
fn test_currency_from_str(s: &str, expected: Result<&str, CurrencyErrorKind>) {
    match (Currency::from_str(s), expected) {
        (Ok(actual), Ok(expected)) => assert_eq!(actual, Currency(expected.to_owned())),
        (Err(actual), Err(kind)) => assert_eq!(actual, CurrencyError(kind)),
        (Err(actual), Ok(_)) => panic!("unexpected failure: {}", actual),
        (Ok(actual), Err(_)) => panic!("unexpected success: {}", actual),
    }
}

#[test_case('A', Ok(FlagLetter('A')))]
#[test_case('b', Err(FlagLetterError('b')))]
#[test_case('?', Err(FlagLetterError('?')))]
fn test_flag_letter_try_from(c: char, expected: Result<FlagLetter, FlagLetterError>) {
    let result = FlagLetter::try_from(c);
    // visually check the error display by making a bad test case
    if let Err(ref e) = result {
        println!("{}", e);
    }
    assert_eq!(result, expected);
}

#[test_case("a-b-c-d", Ok("a-b-c-d"))]
#[test_case("a=b?c,d-e", Err(TagOrLinkIdentifierError(vec!['=', '?', ','])))]
fn test_tag_or_link_identifier_from_str(
    s: &str,
    expected_raw: Result<&str, TagOrLinkIdentifierError>,
) {
    let result = TagOrLinkIdentifier::from_str(s);
    let expected = match expected_raw {
        Ok(s) => Ok(TagOrLinkIdentifier(s.to_string())),
        Err(e) => Err(e),
    };
    // visually check the error display by making a bad test case
    if let Err(ref e) = result {
        println!("{}", e);
    }
    assert_eq!(result, expected);
}

use RawDecimalExpr::*;
#[test_case(Add(Box::new(Value(dec!(12.01))), Box::new(Value(dec!(1.5)))), dec!(13.51))]
#[test_case(Div(Box::new(Value(dec!(1.00))), Box::new(Value(dec!(3)))), dec!(0.33))]
fn test_decimal_expr_new(raw: RawDecimalExpr, value: Decimal) {
    let expr = DecimalExpr::new(raw);
    assert_eq!(expr.value, value);
}
