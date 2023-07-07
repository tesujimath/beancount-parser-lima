// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::{Input, Parser};
use lexer::{lex, Token};
use parser::{end_of_input, file, ParserError, Span};
use std::{
    fs::File,
    io::{self, Read, Write},
};
pub use types::*;

pub struct BeancountParser<'a> {
    file_path: String,
    file_content: String,
    tokens: Option<Vec<(Token<'a>, Span)>>,
    parse_result: Option<Result<Vec<Declaration<'a>>, Vec<ParserError<'a>>>>,
}

impl<'a> BeancountParser<'a> {
    pub fn open(file_path: String) -> io::Result<Self> {
        let mut f = File::open(&file_path)?;
        let mut file_content = String::new();

        // read the whole file
        f.read_to_string(&mut file_content)?;

        Ok(BeancountParser {
            file_path,
            file_content,
            tokens: None,
            parse_result: None,
        })
    }

    pub fn parse<W>(&'a mut self, w: W) -> Result<Vec<Declaration<'a>>, io::Result<()>>
    where
        W: Write + Copy,
    {
        self.tokens = Some(lex(&self.file_content));
        let spanned_tokens = self
            .tokens
            .as_ref()
            .unwrap()
            .spanned(end_of_input(&self.file_content));

        match file().parse(spanned_tokens).into_result() {
            Ok(declarations) => Ok(declarations),
            Err(errors) => {
                self.write_errors(w, errors).map_err(Err)?;
                Err(Ok(()))
            }
        }
    }

    fn write_errors<W>(&self, w: W, errors: Vec<ParserError>) -> io::Result<()>
    where
        W: Write + Copy,
    {
        for error in errors
            .into_iter()
            .map(|e| e.map_token(|tok| tok.to_string()))
        {
            Report::build(
                ReportKind::Error,
                self.file_path.clone(),
                error.span().start,
            )
            .with_message(error.to_string())
            .with_label(
                Label::new((self.file_path.clone(), error.span().into_range()))
                    .with_message(error.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(error.contexts().map(|(label, span)| {
                Label::new((self.file_path.clone(), span.into_range()))
                    .with_message(format!("while parsing this {}", label))
                    .with_color(Color::Yellow)
            }))
            .finish()
            // TODO cloning here seems crass, and also unwrap
            .write(
                sources([(self.file_path.clone(), self.file_content.clone())]),
                w,
            )
            .unwrap()
        }
        Ok(())
    }
}

pub use lexer::dump as logos_dump;
mod lexer;
mod parser;
mod types;
