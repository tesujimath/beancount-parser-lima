use std::env;
use std::fs::File;
use std::io::{self, prelude::*};

use beancount_parser::logos_dump;

fn main() -> io::Result<()> {
    for arg in env::args().skip(1) {
        let mut f = File::open(arg)?;
        let mut buffer = String::new();

        // read the whole file
        f.read_to_string(&mut buffer)?;
        logos_dump(&buffer);
    }
    Ok(())
}
