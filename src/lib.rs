// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]

use ariadne::{Color, Label, Report, ReportKind};
use chumsky::prelude::{Input, Parser};
use lexer::{lex, Token};
use parser::{end_of_input, file, includes, ParserError};
use std::{
    collections::{HashMap, VecDeque},
    fmt::{self, Display, Formatter},
    fs::File,
    io::{self, stderr, Read, Write},
    path::{Path, PathBuf},
};
pub use types::*;

/// The transitive closure of all the include'd source files.
pub struct BeancountSources {
    root_path: PathBuf,
    content_paths: HashMap<PathBuf, String>,
    error_paths: HashMap<PathBuf, anyhow::Error>,
}

impl BeancountSources {
    pub fn new(root_path: PathBuf) -> Self {
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
                            let included_path = path
                                .parent()
                                .map_or(PathBuf::from(&include), |parent| parent.join(include));
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

    fn write_error_message<W, StringSpans>(
        &self,
        w: W,
        src_id: String,
        span: Span,
        msg: String,
        reason: String,
        contexts: StringSpans,
    ) -> io::Result<()>
    where
        W: Write + Copy,
        StringSpans: IntoIterator<Item = (&'static str, Span)>,
    {
        Report::build(ReportKind::Error, src_id.clone(), span.start)
            .with_message(msg)
            .with_label(
                Label::new((src_id.clone(), span.into_range()))
                    .with_message(reason)
                    .with_color(Color::Red),
            )
            .with_labels(contexts.into_iter().map(|(label, span)| {
                Label::new((src_id.clone(), span.into_range()))
                    .with_message(format!("while parsing this {}", label))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .write(ariadne::sources(self.sources()), w)
    }

    fn write_parse_errors<W>(&self, w: W, path: &Path, errors: Vec<ParserError>) -> io::Result<()>
    where
        W: Write + Copy,
    {
        let src_id = source_id(path);

        for error in errors
            .into_iter()
            .map(|e| e.map_token(|tok| tok.to_string()))
        {
            self.write_error_message(
                w,
                src_id.clone(),
                *error.span(),
                error.to_string(),
                error.reason().to_string(),
                error.contexts().map(|(label, span)| (*label, *span)),
            )?;
            //     Report::build(ReportKind::Error, src_id.clone(), error.span().start)
            //         .with_message(error.to_string())
            //         .with_label(
            //             Label::new((src_id.clone(), error.span().into_range()))
            //                 .with_message(error.reason().to_string())
            //                 .with_color(Color::Red),
            //         )
            //         .with_labels(error.contexts().map(|(label, span)| {
            //             Label::new((src_id.clone(), span.into_range()))
            //                 .with_message(format!("while parsing this {}", label))
            //                 .with_color(Color::Yellow)
            //         }))
            //         .finish()
            //         .write(ariadne::sources(self.sources()), w)
            //         .unwrap()
        }
        Ok(())
    }

    pub fn write_error<W, M, L>(&self, w: W, loc: &Location, message: M, label: L) -> io::Result<()>
    where
        W: Write + Copy,
        M: Display,
        L: Display,
    {
        let src_id = source_id(loc.path);

        Report::build(ReportKind::Error, src_id.clone(), loc.span.start)
            .with_message(message.to_string())
            .with_label(
                Label::new((src_id, loc.span.into_range()))
                    .with_message(label.to_string())
                    .with_color(Color::Red),
            )
            .finish()
            .write(ariadne::sources(self.sources()), w)
            .unwrap();

        Ok(())
    }

    fn sources(&self) -> Vec<(String, &str)> {
        self.content_paths
            .iter()
            .map(|(path, content)| (source_id(path), content.as_str()))
            .collect()
    }
}

fn source_id<P>(path: P) -> String
where
    P: AsRef<Path>,
{
    path.as_ref().to_string_lossy().into_owned()
}

impl std::fmt::Debug for BeancountSources {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "BeancountSources(",)?;

        for (path, content) in &self.content_paths {
            writeln!(f, "    {} ok len {},", path.display(), content.len())?;
        }

        for (path, error) in &self.error_paths {
            writeln!(f, "    {} {},", path.display(), error)?;
        }

        writeln!(f, ")",)
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

type SpannedToken<'t> = (Token<'t>, Span);

pub struct BeancountParser<'s, 't> {
    sources: &'s BeancountSources,
    tokenized_sources: HashMap<&'t Path, (&'t str, Vec<SpannedToken<'t>>)>,
}

impl<'s, 't> BeancountParser<'s, 't>
where
    's: 't,
{
    pub fn new(sources: &'s BeancountSources) -> Self {
        let mut tokenized_sources = HashMap::new();

        for (pathbuf, content) in &sources.content_paths {
            let path = pathbuf.as_path();

            tokenized_sources.insert(path, (content.as_str(), lex(content)));
        }

        BeancountParser {
            sources,
            tokenized_sources,
        }
    }

    /// Parse the sources, returning declarations or writing errors.
    /// If parsing fails, errors are written to `w`, and the result is Err,
    /// which may or may not include an I/O error from failing to write the errors.
    pub fn parse<W>(&'t self, w: W) -> Result<Vec<(Declaration<'t>, Location<'s>)>, io::Result<()>>
    where
        W: Write + Copy,
        's: 't,
    {
        let mut all_outputs = HashMap::new();
        let mut all_errors = HashMap::new();

        for (pathbuf, content) in &self.sources.content_paths {
            let path = pathbuf.as_path();

            let (_source, tokens) = self.tokenized_sources.get(path).unwrap();
            let spanned_tokens = tokens.spanned(end_of_input(content));

            let (output, errors) = file().parse(spanned_tokens).into_output_errors();

            if let Some(output) = output {
                all_outputs.insert(
                    path,
                    output
                        .into_iter()
                        .map(|(d, span)| (d, Location::new(path, span))),
                );
            }

            if !errors.is_empty() {
                all_errors.insert(path, errors);
            }
        }

        let errors_occured = !all_errors.is_empty();
        for (path, errors) in all_errors.into_iter() {
            let (_source, _tokens) = self.tokenized_sources.get(path).unwrap();
            self.sources
                .write_parse_errors(w, path, errors)
                .map_err(Err)?;
        }

        if !errors_occured {
            Ok(all_outputs.into_values().flatten().collect())
        } else {
            Err(Ok(()))
        }
    }
}

/// Source location of a parsed node.
pub struct Location<'s> {
    path: &'s Path,
    span: Span,
}

impl<'s> Location<'s> {
    fn new(path: &'s Path, span: Span) -> Self {
        Location { path, span }
    }
}

pub use lexer::dump as logos_dump;
mod lexer;
mod parser;
mod types;
