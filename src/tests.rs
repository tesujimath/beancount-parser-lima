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
