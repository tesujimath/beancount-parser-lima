// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]

use ariadne::{Color, Label, Report, ReportKind};
use chrono::NaiveDate;
use chumsky::prelude::{Input, Parser};
use lazy_format::lazy_format;
use lexer::{lex, Token};
use parser::{end_of_input, file, includes};
use std::{
    collections::{BTreeMap, HashMap, VecDeque},
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

    fn write_error_message<W>(
        &self,
        w: W,
        src_id: String,
        span: Span,
        msg: String,
        reason: Option<String>,
        contexts: Vec<(String, Span)>,
    ) -> io::Result<()>
    where
        W: Write + Copy,
    {
        Report::build(ReportKind::Error, src_id.clone(), span.start)
            .with_message(msg)
            .with_labels(reason.into_iter().map(|reason| {
                Label::new((src_id.clone(), span.into_range()))
                    .with_message(reason)
                    .with_color(Color::Red)
            }))
            .with_labels(contexts.into_iter().map(|(label, span)| {
                Label::new((src_id.clone(), span.into_range()))
                    .with_message(lazy_format!("while parsing this {}", label))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .write(ariadne::sources(self.sources()), w)
    }

    pub fn write_sourced_errors<W>(&self, w: W, errors: Vec<SourcedError>) -> io::Result<()>
    where
        W: Write + Copy,
    {
        for error in errors.into_iter() {
            let src_id = source_id(error.source_path);

            self.write_error_message(
                w,
                src_id.clone(),
                error.span,
                error.message,
                error.reason,
                error.contexts,
            )?;
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
            .with_message(message)
            .with_label(
                Label::new((src_id, loc.span.into_range()))
                    .with_message(label)
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

    /// Parse the sources, returning declarations or errors.
    pub fn parse(&'t self) -> Result<Vec<Sourced<Directive<'t>>>, Vec<SourcedError<'t>>>
    where
        's: 't,
    {
        self.parse_declarations().map(|mut declarations_by_path| {
            let mut builder = DirectiveIteratorBuilder::new();
            for (path, declarations) in declarations_by_path.drain() {
                eprintln!(
                    "{} declarations in {}",
                    declarations.len(),
                    path.to_string_lossy()
                );
                for declaration in declarations.into_iter() {
                    builder.declaration(declaration.value, declaration.span, path)
                }
            }

            builder.build().collect::<Vec<_>>()
        })
    }

    /// Parse the sources, returning declarations or errors.
    fn parse_declarations(
        &'t self,
    ) -> Result<HashMap<&'t Path, Vec<Spanned<Declaration<'t>>>>, Vec<SourcedError<'t>>>
    where
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
                    output, // .into_iter()
                           // .map(|(d, span)| (d, Location::new(path, span))),
                );
            }

            if !errors.is_empty() {
                all_errors.insert(path, errors);
            }
        }

        if all_errors.is_empty() {
            Ok(all_outputs)
        } else {
            let sourced_errors = all_errors
                .into_iter()
                .flat_map(|(path, errors)| {
                    errors
                        .into_iter()
                        .map(|e| SourcedError::from_parser_error(path, e))
                })
                .collect::<Vec<_>>();

            Err(sourced_errors)
        }
    }
}

/// Builder for iterator for date-ordered traversal of `Directive`s.
/// Importantly, directives with the same date must be preserved in source file order.
#[derive(Default, Debug)]
struct DirectiveIteratorBuilder<'t> {
    date_buckets: BTreeMap<NaiveDate, Vec<Sourced<'t, Directive<'t>>>>,
    // TODO tags and metadata from push/pop pragma processing
}

impl<'t> DirectiveIteratorBuilder<'t> {
    fn new() -> Self {
        Self::default()
    }

    fn declaration<'a>(
        &'a mut self,
        declaration: Declaration<'t>,
        span: Span,
        source_path: &'t Path,
    ) where
        't: 'a,
    {
        use Declaration::*;

        match declaration {
            Directive(directive) => {
                let date = *directive.date();
                let sourced = Sourced {
                    spanned: spanned(directive, span),
                    source_path,
                };
                match self.date_buckets.get_mut(&date) {
                    None => {
                        self.date_buckets.insert(date, Vec::new());
                        let bucket = self.date_buckets.get_mut(&date).unwrap();
                        bucket.push(sourced);
                    }
                    Some(directives) => directives.push(sourced),
                }
            }
            Pragma(_pragma) => {
                // TODO
            }
        }
    }

    fn build(&mut self) -> impl Iterator<Item = Sourced<'t, Directive<'t>>> {
        let date_buckets = std::mem::take(&mut self.date_buckets);
        date_buckets
            .into_iter()
            .flat_map(|(_date, directives)| directives.into_iter())
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
