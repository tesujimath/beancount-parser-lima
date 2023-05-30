use anyhow::Result;
use beancount_parser::add;
use std::env::current_dir;

fn main() -> Result<()> {
    let result = add(1, 2);
    println!(
        "Hello from runit, result is {}, running in {}",
        result,
        current_dir()?.display()
    );

    Ok(())
}
