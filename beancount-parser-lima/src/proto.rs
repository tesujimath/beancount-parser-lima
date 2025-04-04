use super::types as lima;
use protobuf::MessageField;
use rust_decimal::Decimal;
use time::Date;

// auto-generated from Beancount protobuf schema
include!(concat!(env!("OUT_DIR"), "/proto/mod.rs"));

impl<'a> From<(lima::Posting<'a>, Date)> for data::Posting {
    fn from((value, date): (lima::Posting<'a>, Date)) -> Self {
        let mut result = data::Posting::new();
        result.date = MessageField::some(date.into());
        result.flag = Some(
            value
                .flag()
                .map_or_else(|| "".to_string(), |flag| flag.to_string())
                .into_bytes(),
        );
        // TODO
        result
    }
}

impl<'a> From<lima::Amount<'a>> for data::Amount {
    fn from(value: lima::Amount<'a>) -> Self {
        let mut result = data::Amount::new();
        result.number = MessageField::some(value.number().value().into());
        result.set_currency(value.currency().to_string());
        result
    }
}

impl From<Date> for date::Date {
    fn from(value: Date) -> Self {
        let mut result = date::Date::new();
        result.set_year(value.year());
        result.set_month(value.month() as i32);
        result.set_day(value.day() as i32);
        result
    }
}

impl From<Decimal> for number::Number {
    fn from(value: Decimal) -> Self {
        let mut result = number::Number::new();
        result.set_exact(value.to_string());
        result
    }
}
