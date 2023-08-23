use anyhow::Result;
use std::env;
use std::io::{self, prelude::*};
use std::path::PathBuf;

// for counting allocations
use stats_alloc::{Region, StatsAlloc, INSTRUMENTED_SYSTEM};
use std::alloc::System;

#[global_allocator]
static GLOBAL: &StatsAlloc<System> = &INSTRUMENTED_SYSTEM;

use beancount_parser::{BeancountParser, BeancountSources};

fn main() -> Result<()> {
    // allocation counting
    let reg = Region::new(GLOBAL);

    let mut error_w = &io::stderr();
    let file_path = env::args().nth(1).unwrap();

    let sources = BeancountSources::new(PathBuf::from(file_path));
    writeln!(error_w, "{:?}", &sources)?;

    eprintln!("Allocations before parsing: {:#?}", reg.change());

    let beancount_parser = BeancountParser::new(&sources);
    match beancount_parser.parse() {
        Ok(directives) => {
            writeln!(error_w, "parsed {} directives", directives.len())?;

            eprintln!(
                "Allocations before printing directives: {:#?}",
                reg.change()
            );

            for directive in &directives {
                println!("{}\n", directive.spanned.value);
            }

            eprintln!("Allocations after printing directives: {:#?}", reg.change());

            Ok(())
        }
        Err(errors) => sources
            .write_sourced_errors(error_w, errors)
            .map_err(|e| e.into()),
    }
}
