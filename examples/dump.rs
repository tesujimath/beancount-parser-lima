use anyhow::{anyhow, Result};
use std::env;
use std::io::{prelude::*, stderr};
use std::path::PathBuf;

use beancount_parser::{BeancountParser, BeancountSources};

fn main() -> Result<()> {
    let file_path = env::args().nth(1).unwrap();

    let sources = BeancountSources::new(PathBuf::from(file_path));
    writeln!(&mut stderr(), "{:?}", &sources)?;

    let mut beancount_parser = BeancountParser::new(&sources);
    match beancount_parser.parse(&stderr()) {
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
