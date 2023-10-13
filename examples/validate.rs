use anyhow::Result;
use std::io;
use std::path::PathBuf;

use beancount_parser::{
    BeancountParser, BeancountSources, Directive, DirectiveVariant, Link, Metadata, Posting, Tag,
    Transaction,
};

/// This example is really a test that there is sufficient public access to parser output types.
fn main() -> Result<()> {
    let flags = xflags::parse_or_exit! {
        /// File to parse
        required path: PathBuf
    };

    let error_w = &io::stderr();
    let sources = BeancountSources::new(flags.path);
    let beancount_parser = BeancountParser::new(&sources);

    match beancount_parser.parse() {
        Ok(directives) => {
            for d in directives {
                use DirectiveVariant::*;

                match d.variant() {
                    Transaction(t) => {
                        print_transaction(t, &d);
                    }
                    _ => {
                        println!("{}", &d);
                    }
                }
                println!();
            }

            Ok(())
        }
        Err(errors) => sources.write_errors(error_w, errors).map_err(|e| e.into()),
    }
}

fn print_transaction(t: &Transaction, d: &Directive) {
    print!("{} {}", d.date(), t.flag());
    if let Some(payee) = t.payee() {
        print!(r#" "{}""#, payee);
    }
    if let Some(narration) = t.narration() {
        print!(r#" "{}""#, narration);
    }
    let m = d.metadata();
    print_tags_links_inline(m);
    println!();
    print_keys_values(m);

    for p in t.postings() {
        print_posting(p);
    }

    println!("\n")
}

fn print_posting(p: &Posting) {
    if let Some(flag) = p.flag() {
        print!("  {}", flag);
    }
    // TODO
    println!("\n")
}

fn print_tags_links_inline(m: &Metadata) {
    for tag in m.tags() {
        let tag: &Tag = tag;
        print!(" #{}", stringify(tag));
    }

    for link in m.links() {
        let link: &Link = link;
        print!(" ^{}", stringify(link));
    }
}
fn print_keys_values(m: &Metadata) {
    for (k, v) in m.key_values() {
        println!("  {}: {}", k, v);
    }
}

fn stringify<S>(s: &S) -> &str
where
    S: AsRef<str>,
{
    s.as_ref()
}
