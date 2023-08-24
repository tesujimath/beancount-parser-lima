#![cfg(test)]
use super::{super::lexer::bare_lex, *};
use rust_decimal_macros::dec;
use test_case::test_case;

#[test_case(r##"2023-07-03 * "New World Gardens North East Va ;"
"##, spanned((2023, 7, 3), SimpleSpan::new(0, 10)), spanned(Flag::Asterisk, SimpleSpan::new(11, 12)), None, Some(spanned("New World Gardens North East Va ;", SimpleSpan::new(13, 48))), vec![], vec![])]
fn test_transaction(
    s: &str,
    expected_date: Spanned<(i32, u32, u32)>,
    expected_flag: Spanned<Flag>,
    expected_payee: Option<Spanned<&str>>,
    expected_narration: Option<Spanned<&str>>,
    expected_tags: Vec<Spanned<&str>>,
    expected_links: Vec<Spanned<&str>>,
) {
    let tokens = bare_lex(s);
    let spanned_tokens = tokens.spanned(end_of_input(s));

    let result = transaction().parse(spanned_tokens).into_result();
    let expected_date = spanned(
        NaiveDate::from_ymd_opt(
            expected_date.value.0,
            expected_date.value.1,
            expected_date.value.2,
        )
        .unwrap(),
        expected_date.span,
    );

    let expected_tags = expected_tags
        .into_iter()
        .map(|s| spanned(Tag::try_from(s.value).unwrap(), s.span))
        .collect::<Vec<_>>();
    let expected_tags = expected_tags.iter().map(|s| s.as_ref()).collect();

    let expected_links = expected_links
        .into_iter()
        .map(|s| spanned(Link::try_from(s.value).unwrap(), s.span))
        .collect::<Vec<_>>();
    let expected_links = expected_links.iter().map(|s| s.as_ref()).collect();

    let expected = Transaction {
        date: expected_date,
        flag: expected_flag,
        payee: expected_payee,
        narration: expected_narration,
        tags: expected_tags,
        links: expected_links,
        metadata: Metadata::default(),
        postings: Vec::new(),
    };

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

#[test_case(r#"#a ^b #c-is-my-tag ^d.is_my/link"#, vec![spanned("a", SimpleSpan::new(0, 2)), spanned("c-is-my-tag", SimpleSpan::new(6, 18))], vec![spanned("b", SimpleSpan::new(3, 5)), spanned("d.is_my/link", SimpleSpan::new(19, 32))])]
fn test_tags_links(s: &str, expected_tags: Vec<Spanned<&str>>, expected_links: Vec<Spanned<&str>>) {
    let tokens = bare_lex(s);
    let spanned_tokens = tokens.spanned(end_of_input(s));

    let expected_tags = expected_tags
        .into_iter()
        .map(|s| spanned(Tag::try_from(s.value).unwrap(), s.span))
        .collect::<Vec<_>>();
    let expected_tags = expected_tags.iter().map(|s| s.as_ref()).collect();

    let expected_links = expected_links
        .into_iter()
        .map(|s| spanned(Link::try_from(s.value).unwrap(), s.span))
        .collect::<Vec<_>>();
    let expected_links = expected_links.iter().map(|s| s.as_ref()).collect();

    let result = tags_links().parse(spanned_tokens).into_result();

    assert_eq!(result, Ok((expected_tags, expected_links)));
}
