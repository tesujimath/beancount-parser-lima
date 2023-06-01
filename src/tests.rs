#![cfg(test)]
use super::*;
use test_case::test_case;

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
fn test_tag_or_link_identifier_try_from(
    s: &str,
    expected_raw: Result<&str, TagOrLinkIdentifierError>,
) {
    let result = TagOrLinkIdentifier::try_from(s);
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
