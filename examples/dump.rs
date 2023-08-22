use anyhow::Result;
use std::env;
use std::io::{self, prelude::*};
use std::path::PathBuf;

use beancount_parser::{BeancountParser, BeancountSources, Declaration, Pragma};

fn main() -> Result<()> {
    let mut error_w = &io::stderr();
    let file_path = env::args().nth(1).unwrap();

    let sources = BeancountSources::new(PathBuf::from(file_path));
    writeln!(error_w, "{:?}", &sources)?;

    let beancount_parser = BeancountParser::new(&sources);
    match beancount_parser.parse() {
        Ok(located_declarations) => {
            writeln!(
                error_w,
                "parsed {} declarations",
                located_declarations.len()
            )?;

            for (d, loc) in &located_declarations {
                writeln!(error_w, "{:?}", d)?;

                // use Directive::*;
                // use Pragma::*;

                if let Declaration::Pragma(Pragma::Include(filename)) = d {
                    writeln!(error_w, "Ooo, include spotted with {}", filename)?;
                    sources.write_error(error_w, loc, "include", "something bad happened here")?;
                }
            }
            Ok(())
        }
        Err(errors) => sources
            .write_sourced_errors(error_w, errors)
            .map_err(|e| e.into()),
    }
}
