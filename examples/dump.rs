use anyhow::Result;
use std::io::{self, prelude::*};
use std::path::PathBuf;

// for counting allocations
use stats_alloc::{Region, StatsAlloc, INSTRUMENTED_SYSTEM};
use std::alloc::System;

#[global_allocator]
static GLOBAL: &StatsAlloc<System> = &INSTRUMENTED_SYSTEM;

use beancount_parser::{BeancountParser, BeancountSources};

fn main() -> Result<()> {
    let flags = xflags::parse_or_exit! {
        /// Show allocations
        optional --show-allocations
        /// File to parse
        required path: PathBuf
    };

    // allocation counting
    let reg = Region::new(GLOBAL);

    let mut error_w = &io::stderr();

    let sources = BeancountSources::new(flags.path);
    writeln!(error_w, "{:?}", &sources)?;

    if flags.show_allocations {
        eprintln!("Allocations before parsing: {:#?}", reg.change());
    }

    let beancount_parser = BeancountParser::new(&sources);
    match beancount_parser.parse() {
        Ok(directives) => {
            writeln!(error_w, "parsed {} directives", directives.len())?;

            let mut directives_as_strings = Vec::new();

            if flags.show_allocations {
                eprintln!(
                    "Allocations before printing directives: {:#?}",
                    reg.change()
                );
            }

            for directive in &directives {
                if flags.show_allocations {
                    let directive_as_string = format!("{}", &directive.spanned.value());
                    directives_as_strings.push(directive_as_string);
                    let last_directive = directives_as_strings.last().unwrap();
                    println!("{}\n", last_directive);
                } else {
                    println!("{}\n", &directive.spanned.value());
                }
            }

            if flags.show_allocations {
                eprintln!("Allocations after printing directives: {:#?}", reg.change());
            }

            Ok(())
        }
        Err(errors) => sources
            .write_sourced_errors(error_w, errors)
            .map_err(|e| e.into()),
    }
}
