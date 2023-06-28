use std::fs::File;
use std::io::{self, prelude::*};

use beancount_parser::dump_tokens;

fn main() -> io::Result<()> {
    let mut f = File::open("examples/data/example.beancount")?;
    let mut buffer = String::new();

    // read the whole file
    f.read_to_string(&mut buffer)?;
    dump_tokens(&buffer);
    Ok(())
}
