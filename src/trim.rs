#[cfg(test)]
use test_case::test_case;

// adjust the span to exclude trailing whitespace
pub fn trim_trailing_whitespace(content: &str, start: usize, end: usize) -> usize {
    match &content[start..end].rfind(|c: char| !c.is_whitespace()) {
        Some(i) => start + *i + 1,
        None => end,
    }
}

#[cfg(test)]
#[test_case("abc", "abc"; "abc")]
#[test_case("abc ", "abc"; "abc space")]
#[test_case("abc\n", "abc"; "abc newline")]
#[test_case("abc\ndef\n\n", "abc\ndef"; "abc def")]
#[test_case("abc\n😱\ndef\n", "abc\n😱\ndef"; "abc scream def")]
fn test_trim_trailing_whitespace(source: &str, expected: &str) {
    let trimmed_end = trim_trailing_whitespace(source, 0, source.len());
    assert_eq!(&source[..trimmed_end], expected);
}
