use anyhow::{anyhow, Result};
use std::env;
use std::io::{prelude::*, stderr};
use std::path::PathBuf;

use beancount_parser::{BeancountParser, BeancountSources, Declaration, Pragma};

fn main() -> Result<()> {
    let file_path = env::args().nth(1).unwrap();

    let sources = BeancountSources::new(PathBuf::from(file_path));
    writeln!(&mut stderr(), "{:?}", &sources)?;

    let beancount_parser = BeancountParser::new(&sources);
    match beancount_parser.parse(&stderr()) {
        Ok(located_declarations) => {
            writeln!(
                &mut stderr(),
                "parsed {} declarations",
                located_declarations.len()
            )?;

            for (d, loc) in &located_declarations {
                writeln!(&mut stderr(), "{:?}", d)?;

                // use Directive::*;
                // use Pragma::*;

                if let Declaration::Pragma(Pragma::Include(filename)) = d {
                    writeln!(&mut stderr(), "Ooo, include spotted with {}", filename)?;
                    sources.write_error(
                        &stderr(),
                        loc,
                        "include",
                        "something bad happened here",
                    )?;
                }
            }
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
