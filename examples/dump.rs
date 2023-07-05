use anyhow::Result;
use std::env;
use std::fs::File;
use std::io::{prelude::*, stderr};

use beancount_parser::BeancountParser;

fn main() -> Result<()> {
    for arg in env::args().skip(1) {
        let mut f = File::open(arg)?;
        let mut source = String::new();

        // read the whole file
        f.read_to_string(&mut source)?;

        let mut b = BeancountParser::new(source);
        match b.parse(Some(&mut stderr())) {
            Ok(declarations) => {
                writeln!(&mut stderr(), "parsed {} declarations", declarations.len())?;
            }
            Err(e) => writeln!(&mut stderr(), "parsing failed {:?}", e)?,
        }
    }
    Ok(())
}
