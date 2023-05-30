use chrono::NaiveDate;
use nom::{bytes::complete::take, combinator::map_res, IResult};

pub fn date(i: &str) -> IResult<&str, NaiveDate> {
    map_res(take(10usize), |s| NaiveDate::parse_from_str(s, "%Y-%m-%d"))(i)
}

mod tests;
