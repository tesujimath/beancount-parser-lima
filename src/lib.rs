// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]

pub use lexer::dump as logos_dump;
pub use types::*;

mod lexer;
mod parser;
mod types;
