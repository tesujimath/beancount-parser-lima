// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]

use ariadne::{Color, Label, Report, ReportKind};
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
    root_path: PathBuf,
    content_paths: HashMap<PathBuf, String>,
    error_paths: HashMap<PathBuf, anyhow::Error>,
}

impl Sources {
    pub fn get(root_path: PathBuf) -> Self {
        let mut content_paths = HashMap::new();
        let mut error_paths = HashMap::new();
        let mut paths = VecDeque::from([root_path.clone()]);

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
            root_path,
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

#[derive(Default, Debug)]
pub struct Tokens<'a>(HashMap<&'a Path, Vec<(Token<'a>, Span)>>);

impl<'a> Tokens<'a> {
    pub fn new(sources: &Sources) -> Tokens {
        let mut tokens = HashMap::new();

        for (path, content) in sources.content_paths.iter() {
            tokens.insert(path.as_path(), lex(content));
        }

        Tokens(tokens)
    }
}

pub fn parse_tokens<'a, W>(
    sources: &'a Sources,
    tokens: &'a Tokens,
    w: W,
) -> Result<Vec<Declaration<'a>>, io::Result<()>>
where
    W: Write + Copy,
{
    let path: &Path = sources.root_path.as_ref();
    match sources.content_paths.get(path) {
        Some(content) => {
            let spanned_tokens = tokens.0[path].spanned(end_of_input(content));

            let (parse_result, parse_errors) = file().parse(spanned_tokens).into_output_errors();
            if !parse_errors.is_empty() {
                write_errors(w, path, content, parse_errors).map_err(Err)?;
                Err(Ok(()))
            } else if let Some(declarations) = parse_result {
                Ok(declarations)
            } else {
                Err(Ok(()))
            }
        }

        None => {
            // TODO something better here
            writeln!(&mut stderr(), "no content available for {}", path.display()).unwrap();

            Err(Ok(()))
        }
    }
}

fn write_errors<W>(w: W, path: &Path, content: &str, errors: Vec<ParserError>) -> io::Result<()>
where
    W: Write + Copy,
{
    let src_id = path.to_string_lossy().into_owned();

    for error in errors
        .into_iter()
        .map(|e| e.map_token(|tok| tok.to_string()))
    {
        Report::build(ReportKind::Error, src_id.clone(), error.span().start)
            .with_message(error.to_string())
            .with_label(
                Label::new((src_id.clone(), error.span().into_range()))
                    .with_message(error.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(error.contexts().map(|(label, span)| {
                Label::new((src_id.clone(), span.into_range()))
                    .with_message(format!("while parsing this {}", label))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .write(ariadne::sources([(src_id.clone(), content.to_string())]), w)
            .unwrap()
    }
    Ok(())
}

#[derive(Default, Debug)]
pub struct BeancountParser<'a> {
    tokens: HashMap<&'a Path, Vec<(Token<'a>, Span)>>,
}

impl<'a> BeancountParser<'a> {
    // pub fn new() -> Self {

    // }

    pub fn parse<'b, 's, W>(
        &'b mut self,
        sources: &'s Sources,
        w: W,
    ) -> Result<Vec<Declaration<'b>>, io::Result<()>>
    where
        W: Write + Copy,
        's: 'a,
        'a: 'b,
    {
        let path: &Path = sources.root_path.as_ref();
        match sources.content_paths.get(path) {
            Some(content) => {
                self.tokens.insert(path, lex(content));
                let tokens = self.tokens.get(path).unwrap();
                let spanned_tokens = tokens.spanned(end_of_input(content));

                let (parse_result, parse_errors) =
                    file().parse(spanned_tokens).into_output_errors();
                if !parse_errors.is_empty() {
                    self.write_errors(w, path, content, parse_errors)
                        .map_err(Err)?;
                    Err(Ok(()))
                } else if let Some(declarations) = parse_result {
                    Ok(declarations)
                } else {
                    Err(Ok(()))
                }
            }

            None => {
                // TODO something better here
                writeln!(&mut stderr(), "no content available for {}", path.display()).unwrap();

                Err(Ok(()))
            }
        }
    }

    fn write_errors<W>(
        &self,
        w: W,
        path: &Path,
        content: &str,
        errors: Vec<ParserError>,
    ) -> io::Result<()>
    where
        W: Write + Copy,
    {
        let src_id = path.to_string_lossy().into_owned();

        for error in errors
            .into_iter()
            .map(|e| e.map_token(|tok| tok.to_string()))
        {
            Report::build(ReportKind::Error, src_id.clone(), error.span().start)
                .with_message(error.to_string())
                .with_label(
                    Label::new((src_id.clone(), error.span().into_range()))
                        .with_message(error.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(error.contexts().map(|(label, span)| {
                    Label::new((src_id.clone(), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .write(ariadne::sources([(src_id.clone(), content.to_string())]), w)
                .unwrap()
        }
        Ok(())
    }
}

pub use lexer::dump as logos_dump;
mod lexer;
mod parser;
mod types;
