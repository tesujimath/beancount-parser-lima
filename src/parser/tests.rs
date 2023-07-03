#![cfg(test)]
use super::*;
use rust_decimal_macros::dec;
use test_case::test_case;

#[test_case(r##"2023-07-03 * "New World Gardens North East Va ;""##, (2023, 7, 3), Flag::Asterisk, None, Some("New World Gardens North East Va ;"), vec![], vec![])]
fn test_transaction(
    s: &str,
    expected_date: (i32, u32, u32),
    expected_flag: Flag,
    expected_payee: Option<&str>,
    expected_narration: Option<&str>,
    expected_tags: Vec<&str>,
    expected_links: Vec<&str>,
) {
    let tokens = tokenize(s);
    let spanned = tokens.spanned(end_of_input(s));

    let result = transaction().parse(spanned).into_result();
    let expected_payee = expected_payee.map(|s| Cow::Borrowed(s));
    let expected_narration = expected_narration.map(|s| Cow::Borrowed(s));
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
        expected_payee.as_ref(),
        expected_narration.as_ref(),
        expected_tags.iter().collect(),
        expected_links.iter().collect(),
    );

    assert_eq!(result, Ok(expected));
}

#[test_case("GBP", CompoundAmount::BareCurrency(&Currency("GBP")))]
#[test_case("456.78", CompoundAmount::BareAmount(CompoundExpr::PerUnit(Expr::Value(dec!(456.78)))))]
#[test_case("# 1456.98", CompoundAmount::BareAmount(CompoundExpr::Total(Expr::Value(dec!(1456.98)))))]
#[test_case("456.78 NZD", CompoundAmount::CurrencyAmount(CompoundExpr::PerUnit(Expr::Value(dec!(456.78))), &Currency("NZD")))]
#[test_case("# 1456.98 USD", CompoundAmount::CurrencyAmount(CompoundExpr::Total(Expr::Value(dec!(1456.98))), &Currency("USD")))]
fn test_compound_amount(s: &str, expected: CompoundAmount) {
    let tokens = tokenize(s);
    let spanned = tokens.spanned(end_of_input(s));

    let result = compound_amount().parse(spanned).into_result();

    assert_eq!(result, Ok(expected));
}

#[test_case("123.45", CompoundExpr::PerUnit(Expr::Value(dec!(123.45))))]
#[test_case("789.45 #", CompoundExpr::PerUnit(Expr::Value(dec!(789.45))))]
#[test_case("# 123.45", CompoundExpr::Total(Expr::Value(dec!(123.45))))]
fn test_compound_expr(s: &str, expected: CompoundExpr) {
    let tokens = tokenize(s);
    let spanned = tokens.spanned(end_of_input(s));

    let result = compound_expr().parse(spanned).into_result();

    assert_eq!(result, Ok(expected));
}

#[test_case(r#"#a ^b #c-is-my-tag ^d.is_my/link"#, vec!["a", "c-is-my-tag"], vec!["b", "d.is_my/link"])]
fn test_tags_links(s: &str, expected_tags: Vec<&str>, expected_links: Vec<&str>) {
    let tokens = tokenize(s);
    let spanned = tokens.spanned(end_of_input(s));

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

    let result = tags_links().parse(spanned).into_result();

    assert_eq!(result, Ok((expected_tags, expected_links)));
}
