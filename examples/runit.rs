use anyhow::Result;
use beancount_parser::date;
use nom::Finish;
use std::env::current_dir;

fn main() -> Result<()> {
    let result = date("2023-05-29").finish();
    println!(
        "Hello from runit, result is {:?}, running in {}",
        result,
        current_dir()?.display()
    );

    Ok(())
}
