use anyhow::{anyhow, Result};
use std::env;
use std::io::{prelude::*, stderr};
use std::path::PathBuf;

use beancount_parser::{parse_tokens, Sources, Tokens};

fn main() -> Result<()> {
    let file_path = env::args().nth(1).unwrap();
    let sources = Sources::get(PathBuf::from(file_path));
    let tokens = Tokens::new(&sources);
    match parse_tokens(&sources, &tokens, &stderr()) {
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
