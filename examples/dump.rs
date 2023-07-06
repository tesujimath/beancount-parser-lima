use anyhow::Result;
use std::env;
use std::io::{prelude::*, stderr};

use beancount_parser::BeancountParser;

fn main() -> Result<()> {
    for file_path in env::args().skip(1) {
        let mut beancount = BeancountParser::open(file_path)?;
        match beancount.parse(Some(&stderr())) {
            Ok(declarations) => {
                writeln!(&mut stderr(), "parsed {} declarations", declarations.len())?;
            }
            Err(e) => writeln!(&mut stderr(), "parsing failed {:?}", e)?,
        }
    }
    Ok(())
}
