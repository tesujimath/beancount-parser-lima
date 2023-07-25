// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::{Input, Parser};
use lexer::{lex, Token};
use parser::{end_of_input, file, includes, ParserError, Span};
use std::{
    collections::{HashMap, VecDeque},
    fs::File,
    io::{self, stderr, Read, Write},
    path::{Path, PathBuf},
};
pub use types::*;

pub struct Sources {
    content_paths: HashMap<PathBuf, String>,
    error_paths: HashMap<PathBuf, anyhow::Error>,
}

impl Sources {
    pub fn get(source_path: PathBuf) -> Self {
        let mut content_paths = HashMap::new();
        let mut error_paths = HashMap::new();
        let mut paths = VecDeque::from([source_path]);

        while !paths.is_empty() {
            let path = paths.pop_front().unwrap();

            match read(&path) {
                Ok(content) => {
                    // until Entry::insert_entry is stabilised, it seems we have to clone the PathBuf and do a double lookup
                    content_paths.insert(path.clone(), content);
                    let content = content_paths.get(&path).unwrap();

                    let tokens = Some(lex(content));
                    let spanned_tokens = tokens.as_ref().unwrap().spanned(end_of_input(content));

                    // ignore any errors in parsing, we'll pick them up in the next pass
                    // TODO can we do this with &str for includes?
                    if let Some(includes) = includes().parse(spanned_tokens).into_output() {
                        for include in includes {
                            let included_path = path.join(include);
                            if !content_paths.contains_key(&included_path)
                                && !error_paths.contains_key(&included_path)
                            {
                                paths.push_back(included_path);
                            } else {
                                // TODO include cycle error
                                writeln!(
                                    &mut stderr(),
                                    "warning: ignoring include cycle in {} for {}",
                                    &path.display(),
                                    &included_path.display()
                                )
                                .unwrap();
                            }
                        }
                    };
                }
                Err(e) => {
                    error_paths.insert(path, e);
                }
            }
        }

        Self {
            content_paths,
            error_paths,
        }
    }
}

fn read<P>(file_path: P) -> anyhow::Result<String>
where
    P: AsRef<Path>,
{
    let mut f = File::open(&file_path)?;
    let mut file_content = String::new();

    // read the whole file
    f.read_to_string(&mut file_content)?;
    Ok(file_content)
}

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

        let (parse_result, parse_errors) = file().parse(spanned_tokens).into_output_errors();
        if !parse_errors.is_empty() {
            self.write_errors(w, parse_errors).map_err(Err)?;
            Err(Ok(()))
        } else if let Some(declarations) = parse_result {
            Ok(declarations)
        } else {
            Err(Ok(()))
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
