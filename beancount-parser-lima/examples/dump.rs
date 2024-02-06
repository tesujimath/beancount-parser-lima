use std::io::{self, prelude::*};
use std::path::PathBuf;

// for counting allocations
use stats_alloc::{Region, StatsAlloc, INSTRUMENTED_SYSTEM};
use std::alloc::System;

#[global_allocator]
static GLOBAL: &StatsAlloc<System> = &INSTRUMENTED_SYSTEM;

use beancount_parser_lima::{
    BeancountParser, BeancountSources, Directive, ParseError, ParseSuccess,
};

fn main() {
    let flags = xflags::parse_or_exit! {
        /// Show allocations
        optional --show-allocations

        /// File to parse
        required path: PathBuf
    };

    let mut stderr = &io::stderr();

    let sources = if flags.path.to_str() == Some("STDIN") {
        let mut source_string = String::new();
        io::stdin().read_to_string(&mut source_string).unwrap();
        BeancountSources::from(source_string)
    } else {
        BeancountSources::from(flags.path)
    };

    let parser = BeancountParser::new(&sources);
    writeln!(stderr, "{:?}", &sources).unwrap();

    parse(&sources, &parser, flags.show_allocations, stderr);
}

fn parse<W>(
    sources: &BeancountSources,
    parser: &BeancountParser,
    show_allocations: bool,
    error_w: W,
) where
    W: Write + Copy,
{
    // allocation counting
    let reg = Region::new(GLOBAL);

    if show_allocations {
        eprintln!("Allocations before parsing: {:#?}", reg.change());
    }

    match parser.parse() {
        Ok(ParseSuccess {
            directives,
            options: _,
            plugins: _,
            warnings,
        }) => {
            let mut directives_as_strings = Vec::new();

            if show_allocations {
                eprintln!(
                    "Allocations before printing directives: {:#?}",
                    reg.change()
                );
            }

            fn show_directive(
                directive: &Directive,
                show_allocations: bool,
                directives_as_strings: &mut Vec<String>,
            ) {
                if show_allocations {
                    let directive_as_string = format!("{}", &directive);
                    directives_as_strings.push(directive_as_string);
                    let last_directive = directives_as_strings.last().unwrap();
                    println!("{}\n", last_directive);
                } else {
                    println!("{}\n", &directive);
                }
            }

            for directive in directives {
                show_directive(&directive, show_allocations, &mut directives_as_strings);
            }

            if show_allocations {
                eprintln!("Allocations after printing directives: {:#?}", reg.change());
            }

            sources.write(error_w, warnings).unwrap();
        }
        Err(ParseError { errors, warnings }) => {
            sources.write(error_w, errors).unwrap();
            sources.write(error_w, warnings).unwrap();
        }
    }
}
