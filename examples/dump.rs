use anyhow::Result;
use std::env;
use std::io::{self, prelude::*};
use std::path::PathBuf;

use beancount_parser::{BeancountParser, BeancountSources};

fn main() -> Result<()> {
    let mut error_w = &io::stderr();
    let file_path = env::args().nth(1).unwrap();

    let sources = BeancountSources::new(PathBuf::from(file_path));
    writeln!(error_w, "{:?}", &sources)?;

    let beancount_parser = BeancountParser::new(&sources);
    match beancount_parser.parse() {
        Ok(directives) => {
            writeln!(error_w, "parsed {} directives", directives.len())?;

            for directive in &directives {
                println!("{}", directive.spanned.value);
            }
            Ok(())
        }
        Err(errors) => sources
            .write_sourced_errors(error_w, errors)
            .map_err(|e| e.into()),
    }
}
