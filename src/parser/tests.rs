#![cfg(test)]
use super::{super::lexer::bare_lex, *};
use rust_decimal_macros::dec;
use test_case::test_case;

#[test_case(r##"2023-07-03 * "New World Gardens North East Va ;"
"##, (2023, 7, 3), Flag::Asterisk, None, Some("New World Gardens North East Va ;"), vec![], vec![])]
fn test_transaction(
    s: &str,
    expected_date: (i32, u32, u32),
    expected_flag: Flag,
    expected_payee: Option<&str>,
    expected_narration: Option<&str>,
    expected_tags: Vec<&str>,
    expected_links: Vec<&str>,
) {
    let tokens = bare_lex(s);
    let spanned_tokens = tokens.spanned(end_of_input(s));

    let result = transaction().parse(spanned_tokens).into_result();
    let expected_tags = expected_tags
        .into_iter()
        .map(|s| Tag::try_from(s).unwrap())
        .collect::<Vec<_>>();
    let expected_links = expected_links
        .into_iter()
        .map(|s| Link::try_from(s).unwrap())
        .collect::<Vec<_>>();
    let expected = Transaction::new(
        NaiveDate::from_ymd_opt(expected_date.0, expected_date.1, expected_date.2).unwrap(),
        expected_flag,
        expected_payee,
        expected_narration,
        expected_tags.iter().collect(),
        expected_links.iter().collect(),
        Metadata::default(),
        Vec::new(),
    );

    assert_eq!(result, Ok(expected));
}

#[test_case("GBP", ScopedAmount::BareCurrency(&Currency::try_from("GBP").unwrap()))]
#[test_case("456.78", ScopedAmount::BareAmount(ScopedExpr::PerUnit(Expr::Value(dec!(456.78)))))]
#[test_case("# 1456.98", ScopedAmount::BareAmount(ScopedExpr::Total(Expr::Value(dec!(1456.98)))))]
#[test_case("456.78 NZD", ScopedAmount::CurrencyAmount(ScopedExpr::PerUnit(Expr::Value(dec!(456.78))), &Currency::try_from("NZD").unwrap()))]
#[test_case("# 1456.98 USD", ScopedAmount::CurrencyAmount(ScopedExpr::Total(Expr::Value(dec!(1456.98))), &Currency::try_from("USD").unwrap()))]
fn test_compound_amount(s: &str, expected: ScopedAmount) {
    let tokens = bare_lex(s);
    let spanned_tokens = tokens.spanned(end_of_input(s));

    let result = compound_amount().parse(spanned_tokens).into_result();

    assert_eq!(result, Ok(expected));
}

#[test_case("123.45", ScopedExpr::PerUnit(Expr::Value(dec!(123.45))))]
#[test_case("789.45 #", ScopedExpr::PerUnit(Expr::Value(dec!(789.45))))]
#[test_case("# 123.45", ScopedExpr::Total(Expr::Value(dec!(123.45))))]
fn test_compound_expr(s: &str, expected: ScopedExpr) {
    let tokens = bare_lex(s);
    let spanned_tokens = tokens.spanned(end_of_input(s));

    let result = compound_expr().parse(spanned_tokens).into_result();

    assert_eq!(result, Ok(expected));
}

#[test_case(r#"#a ^b #c-is-my-tag ^d.is_my/link"#, vec!["a", "c-is-my-tag"], vec!["b", "d.is_my/link"])]
fn test_tags_links(s: &str, expected_tags: Vec<&str>, expected_links: Vec<&str>) {
    let tokens = bare_lex(s);
    let spanned_tokens = tokens.spanned(end_of_input(s));

    let expected_tags = expected_tags
        .into_iter()
        .map(|s| Tag::try_from(s).unwrap())
        .collect::<Vec<_>>();
    let expected_tags = expected_tags.iter().collect::<Vec<_>>();

    let expected_links = expected_links
        .into_iter()
        .map(|s| Link::try_from(s).unwrap())
        .collect::<Vec<_>>();
    let expected_links = expected_links.iter().collect::<Vec<_>>();

    let result = tags_links().parse(spanned_tokens).into_result();

    assert_eq!(result, Ok((expected_tags, expected_links)));
}
