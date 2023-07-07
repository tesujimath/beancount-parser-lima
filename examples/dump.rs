use anyhow::{anyhow, Result};
use std::env;
use std::io::{prelude::*, stderr};

use beancount_parser::BeancountParser;

fn main() -> Result<()> {
    let file_path = env::args().nth(1).unwrap();
    let mut beancount = BeancountParser::open(file_path)?;
    match beancount.parse(&stderr()) {
        Ok(declarations) => {
            writeln!(&mut stderr(), "parsed {} declarations", declarations.len())?;
            Ok(())
        }
        Err(e) => match e {
            Ok(()) => Err(anyhow!("parsing failed")),
            Err(e) => {
                writeln!(
                    &mut stderr(),
                    "parsing failed, failed to write errors {}",
                    e
                )?;
                Err(e.into())
            }
        },
    }
}
