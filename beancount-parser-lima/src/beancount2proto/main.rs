use std::io::{self, prelude::*};
use std::path::PathBuf;

use beancount_parser_lima::{
    BeancountParser, BeancountSources, Directive, ParseError, ParseSuccess,
};

fn main() -> io::Result<()> {
    println!("Hello from beancount2proto");
    let flags = xflags::parse_or_exit! {
        /// File to parse
        required path: PathBuf
    };

    let stderr = &io::stderr();

    let sources = if flags.path.to_str() == Some("STDIN") {
        let mut source_string = String::new();
        io::stdin().read_to_string(&mut source_string).unwrap();
        BeancountSources::from(source_string)
    } else {
        BeancountSources::try_from(flags.path)?
    };

    let parser = BeancountParser::new(&sources);

    parse(&sources, &parser, stderr);

    Ok(())
}

fn parse<W>(sources: &BeancountSources, parser: &BeancountParser, error_w: W)
where
    W: Write + Copy,
{
    match parser.parse() {
        Ok(ParseSuccess {
            directives,
            options: _,
            plugins: _,
            warnings,
        }) => {
            fn show_directive(directive: &Directive) {
                println!("{}\n", &directive);
            }

            for directive in directives {
                show_directive(&directive);
            }

            sources.write(error_w, warnings).unwrap();
        }
        Err(ParseError { errors, warnings }) => {
            sources.write(error_w, errors).unwrap();
            sources.write(error_w, warnings).unwrap();
        }
    }
}
