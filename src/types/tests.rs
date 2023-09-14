#![cfg(test)]
use super::*;
use test_case::test_case;

#[test_case('A', Ok(FlagLetter::try_from('A').unwrap()))]
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
