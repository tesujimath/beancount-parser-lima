#![cfg(test)]
use super::super::{bare_lex, end_of_input, types::*};
use super::*;
use rust_decimal_macros::dec;
use std::ops::Range;
use test_case::test_case;
use time::Month;

fn bare_lex_with_source(source_id: SourceId, s: &str) -> Vec<(Token, Span)> {
    bare_lex(s)
        .map(|(tok, span)| (tok, chumsky::span::Span::new(source_id, span)))
        .collect::<Vec<_>>()
}

#[test_case(r#"2023-07-03 * "New World Gardens North East Va ;"
"#, ((2023, Month::July, 3), 0..10), (Flag::Asterisk, 11..12), None, Some(("New World Gardens North East Va ;", 13..48)), vec![], vec![])]
fn test_transaction(
    s: &str,
    expected_date: ((i32, Month, u8), Range<usize>),
    expected_flag: (Flag, Range<usize>),
    expected_payee: Option<(&str, Range<usize>)>,
    expected_narration: Option<(&str, Range<usize>)>,
    expected_tags: Vec<(&str, Range<usize>)>,
    expected_links: Vec<(&str, Range<usize>)>,
) {
    let source_id = SourceId::default();
    let tokens = bare_lex_with_source(source_id, s);
    let mut parser_state = chumsky::extra::SimpleState(ParserState::default());
    let spanned_tokens = tokens.map(end_of_input(source_id, s), |(t, s)| (t, s));
    let sourced_span = |range| chumsky::span::Span::new(source_id, range);

    let result = transaction()
        .parse_with_state(spanned_tokens, &mut parser_state)
        .into_result();

    let expected_date = spanned(
        Date::from_calendar_date(expected_date.0 .0, expected_date.0 .1, expected_date.0 .2)
            .unwrap(),
        sourced_span(expected_date.1),
    );

    let expected_flag = spanned(expected_flag.0, sourced_span(expected_flag.1));

    let expected_payee = expected_payee.map(|(s, range)| spanned(s, sourced_span(range)));

    let expected_narration = expected_narration.map(|(s, range)| spanned(s, sourced_span(range)));

    let expected_tags = expected_tags
        .into_iter()
        .map(|(s, range)| spanned(Tag::try_from(s).unwrap(), sourced_span(range)))
        .collect::<HashSet<_>>();

    let expected_links = expected_links
        .into_iter()
        .map(|(s, range)| spanned(Link::try_from(s).unwrap(), sourced_span(range)))
        .collect::<HashSet<_>>();

    assert!(result.is_ok());
    let result = result.unwrap();
    assert_eq!(&result.date, &expected_date);
    assert_eq!(&result.metadata.tags, &expected_tags);
    assert_eq!(&result.metadata.links, &expected_links);
    assert!(
        matches!(&result.variant, DirectiveVariant::Transaction(x) if
            x.flag == expected_flag &&
            x.payee == expected_payee &&
            x.narration == expected_narration
        )
    )
}

#[test_case("@ GBP", PriceSpec::BareCurrency(Currency::try_from("GBP").unwrap()))]
#[test_case("@ 456.78", PriceSpec::BareAmount(ScopedExprValue::PerUnit(Expr::Value(dec!(456.78)).into())))]
#[test_case("@@ 1456.98", PriceSpec::BareAmount(ScopedExprValue::Total(Expr::Value(dec!(1456.98)).into())))]
#[test_case("@ 456.78 NZD", PriceSpec::CurrencyAmount(ScopedExprValue::PerUnit(Expr::Value(dec!(456.78)).into()), Currency::try_from("NZD").unwrap()))]
#[test_case("@@ 1456.98 USD", PriceSpec::CurrencyAmount(ScopedExprValue::Total(Expr::Value(dec!(1456.98)).into()), Currency::try_from("USD").unwrap()))]
#[test_case("@", PriceSpec::Unspecified)]
fn test_price_annotation(s: &str, expected: PriceSpec) {
    let source_id = SourceId::default();
    let tokens = bare_lex_with_source(source_id, s);
    let mut parser_state = chumsky::extra::SimpleState(ParserState::default());
    let spanned_tokens = tokens.map(end_of_input(source_id, s), |(t, s)| (t, s));

    let result = price_annotation()
        .parse_with_state(spanned_tokens, &mut parser_state)
        .into_result();

    assert_eq!(result, Ok(expected));
}

#[test_case("GBP", CompoundAmount::BareCurrency(Currency::try_from("GBP").unwrap()))]
#[test_case("456.78", CompoundAmount::BareAmount(CompoundExprValue::PerUnit(Expr::Value(dec!(456.78)).into())))]
#[test_case("# 1456.98", CompoundAmount::BareAmount(CompoundExprValue::Total(Expr::Value(dec!(1456.98)).into())))]
#[test_case("456.78 NZD", CompoundAmount::CurrencyAmount(CompoundExprValue::PerUnit(Expr::Value(dec!(456.78)).into()), Currency::try_from("NZD").unwrap()))]
#[test_case("# 1456.98 USD", CompoundAmount::CurrencyAmount(CompoundExprValue::Total(Expr::Value(dec!(1456.98)).into()), Currency::try_from("USD").unwrap()))]
#[test_case("123 # 1456.98 USD", CompoundAmount::CurrencyAmount(CompoundExprValue::PerUnitAndTotal(Expr::Value(dec!(123)).into(), Expr::Value(dec!(1456.98)).into()), Currency::try_from("USD").unwrap()))]
fn test_compound_amount(s: &str, expected: CompoundAmount) {
    let source_id = SourceId::default();
    let tokens = bare_lex_with_source(source_id, s);
    let mut parser_state = chumsky::extra::SimpleState(ParserState::default());
    let spanned_tokens = tokens.map(end_of_input(source_id, s), |(t, s)| (t, s));

    let result = compound_amount()
        .parse_with_state(spanned_tokens, &mut parser_state)
        .into_result();

    assert_eq!(result, Ok(expected));
}

#[test_case("123.45", CompoundExprValue::PerUnit(Expr::Value(dec!(123.45)).into()))]
#[test_case("789.45 #", CompoundExprValue::PerUnit(Expr::Value(dec!(789.45)).into()))]
#[test_case("# 123.45", CompoundExprValue::Total(Expr::Value(dec!(123.45)).into()))]
fn test_compound_expr(s: &str, expected: CompoundExprValue) {
    let source_id = SourceId::default();
    let tokens = bare_lex_with_source(source_id, s);
    let mut parser_state = chumsky::extra::SimpleState(ParserState::default());
    let spanned_tokens = tokens.map(end_of_input(source_id, s), |(t, s)| (t, s));

    let result = compound_expr()
        .parse_with_state(spanned_tokens, &mut parser_state)
        .into_result();

    assert_eq!(result, Ok(expected));
}

#[test_case(r#"#a ^b #c-is-my-tag ^d.is_my/link"#, vec![("a", 0..2), ("c-is-my-tag", 6..18)], vec![("b", 3..5), ("d.is_my/link", 19..32)])]
fn test_tags_links(
    s: &str,
    expected_tags: Vec<(&str, Range<usize>)>,
    expected_links: Vec<(&str, Range<usize>)>,
) {
    let source_id = SourceId::default();
    let tokens = bare_lex_with_source(source_id, s);
    let mut parser_state = chumsky::extra::SimpleState(ParserState::default());
    let spanned_tokens = tokens.map(end_of_input(source_id, s), |(t, s)| (t, s));
    let sourced_span = |range| chumsky::span::Span::new(source_id, range);

    let expected_tags = expected_tags
        .into_iter()
        .map(|(s, range)| spanned(Tag::try_from(s).unwrap(), sourced_span(range)))
        .collect::<HashSet<_>>();

    let expected_links = expected_links
        .into_iter()
        .map(|(s, range)| spanned(Link::try_from(s).unwrap(), sourced_span(range)))
        .collect::<HashSet<_>>();

    let result = tags_links()
        .parse_with_state(spanned_tokens, &mut parser_state)
        .into_result();

    assert_eq!(result, Ok((expected_tags, expected_links)));
}

#[test_case("1 + 2 *  3", "(1 + (2 * 3))")]
#[test_case("1 + 2 *  3 / 4 - 5", "((1 + ((2 * 3) / 4)) - 5)")]
#[test_case("(1 + 2) *  3 / (4 - 6)", "(([(1 + 2)] * 3) / [(4 - 6)])")]
#[test_case("72 / 2 / 3", "((72 / 2) / 3)")]
#[test_case("10 - 1", "(10 - 1)")]
#[test_case("10 - -2", "(10 - (-2))")]
#[test_case("-6 - 3", "((-6) - 3)")]
#[test_case("6 - -7", "(6 - (-7))")]
#[test_case("4 + 2 *  3 XYZ", "(4 + (2 * 3))")]
#[test_case("4 + 2 *  3 # freddy", "(4 + (2 * 3))")]
#[test_case("2.718 #", "2.718")]
#[test_case("3.141 # pi", "3.141")]
fn expr_test(s: &str, expected: &str) {
    let source_id = SourceId::default();
    let tokens = bare_lex_with_source(source_id, s);
    let mut parser_state = chumsky::extra::SimpleState(ParserState::default());
    let spanned_tokens = tokens
        .map(end_of_input(source_id, s), |(t, s)| (t, s))
        .with_context(source_id);

    let result = expr()
        .map(|x| format!("{:?}", x))
        .then_ignore(any().repeated().collect::<Vec<Token>>())
        .parse_with_state(spanned_tokens, &mut parser_state)
        .into_result();

    assert_eq!(result, Ok(expected.to_owned()))
}
