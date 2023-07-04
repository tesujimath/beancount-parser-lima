// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]

pub use lexer::dump as logos_dump;
pub use lexer_chumsky::dump as chumsky_dump;
pub use types::*;

mod lexer;
mod lexer_chumsky;
mod parser;
mod types;
