// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]

use chumsky::prelude::{Input, Parser};
use lexer::{lex, Token};
use parser::{end_of_input, file, ParserError, Span};
use std::io::{self, Write};
pub use types::*;

pub struct BeancountParser<'a> {
    source: String,
    tokens: Option<Vec<(Token<'a>, Span)>>,
    parse_result: Option<Result<Vec<Declaration<'a>>, Vec<ParserError<'a>>>>,
}

impl<'a> BeancountParser<'a> {
    pub fn new(source: String) -> Self {
        BeancountParser {
            source,
            tokens: None,
            parse_result: None,
        }
    }

    pub fn parse<F>(
        &'a mut self,
        error_f: Option<&mut F>,
    ) -> Result<Vec<Declaration<'a>>, io::Result<()>>
    where
        F: Write,
    {
        self.tokens = Some(lex(&self.source));
        let spanned_tokens = self
            .tokens
            .as_ref()
            .unwrap()
            .spanned(end_of_input(&self.source));

        match file().parse(spanned_tokens).into_result() {
            Ok(declarations) => Ok(declarations),
            Err(errors) => {
                if let Some(error_f) = error_f {
                    for e in errors {
                        writeln!(error_f, "{:?}", e).map_err(|io_err| Err(io_err))?;
                    }
                }
                Err(Ok(()))
            }
        }
    }

    pub fn prettyprint_errors<F>(&self, f: &mut F) -> io::Result<()>
    where
        F: Write,
    {
        if let Some(parse_result) = self.parse_result.as_ref() {
            if let Err(errors) = parse_result {
                for e in errors {
                    writeln!(f, "{:?}", e)?;
                }
            }
        }
        Ok(())
    }
}

mod lexer;
mod parser;
mod types;
