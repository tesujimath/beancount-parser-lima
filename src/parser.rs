use crate::types::*;
use ariadne::{Color, Label, Report, ReportKind};
use chumsky::prelude::{Input, Parser};
use lazy_format::lazy_format;
use lexer::{lex, Token};
use parsers::{file, includes};
use path_clean::PathClean;
use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    fmt::{self, Display, Formatter},
    fs::File,
    io::{self, stderr, Read, Write},
    path::{Path, PathBuf},
};
use time::Date;
use types::*;

fn end_of_input(source_id: SourceId, s: &str) -> Span {
    chumsky::span::Span::new(source_id, s.len()..s.len())
}

/// a `SourceId` identifies a source file.
#[derive(PartialEq, Eq, Copy, Clone, Default, Debug)]
pub struct SourceId(u32);

impl From<usize> for SourceId {
    fn from(value: usize) -> Self {
        SourceId(value as u32)
    }
}

impl From<SourceId> for usize {
    fn from(value: SourceId) -> Self {
        value.0 as usize
    }
}

impl Display for SourceId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The transitive closure of all the include'd source files.
pub struct BeancountSources {
    // The source_id is the index in `content_paths`, and the first of these is the `root_path`.
    path_content: Vec<(PathBuf, String, String)>,
    path_errors: Vec<(PathBuf, anyhow::Error)>,
}

impl BeancountSources {
    pub fn new(root_path: PathBuf) -> Self {
        let mut path_content = Vec::new();
        let mut path_errors = Vec::new();

        let mut pending_paths = VecDeque::from([root_path.clone()]);
        let mut all_paths = HashSet::new();

        while !pending_paths.is_empty() {
            let path = pending_paths.pop_front().unwrap();
            all_paths.insert(path.clone());

            match read(&path) {
                Ok(content) => {
                    let source_id = SourceId::from(path_content.len());
                    let source_id_string = path.to_string_lossy().into_owned();

                    path_content.push((path, source_id_string, content));
                    let (path, _, content) = path_content.last().unwrap();

                    let tokens = Some(lex(source_id, content));
                    let spanned_tokens = tokens
                        .as_ref()
                        .unwrap()
                        .spanned(end_of_input(source_id, content))
                        .with_context(source_id);

                    // ignore any errors in parsing, we'll pick them up in the next pass
                    // TODO can we do this with &str for includes?
                    if let Some(includes) = includes().parse(spanned_tokens).into_output() {
                        for include in includes {
                            let included_path =
                                path.parent().map_or(PathBuf::from(&include), |parent| {
                                    parent.join(include).clean()
                                });
                            if !all_paths.contains(included_path.as_path()) {
                                pending_paths.push_back(included_path);
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
                    path_errors.push((path, e));
                }
            }
        }

        Self {
            path_content,
            path_errors,
        }
    }

    fn write_error_message<W>(
        &self,
        w: W,
        span: Span,
        msg: String,
        reason: Option<String>,
        contexts: Vec<(String, Span)>,
    ) -> io::Result<()>
    where
        W: Write + Copy,
    {
        use chumsky::span::Span;

        let src_id = self.source_id_string(&span);
        Report::build(ReportKind::Error, src_id.to_string(), span.start)
            .with_message(msg)
            .with_labels(reason.into_iter().map(|reason| {
                Label::new((src_id.to_string(), span.start()..span.end()))
                    .with_message(reason)
                    .with_color(Color::Red)
            }))
            .with_labels(contexts.into_iter().map(|(label, span)| {
                Label::new((
                    self.source_id_string(&span).to_string(),
                    span.start()..span.end(),
                ))
                .with_message(lazy_format!("while parsing this {}", label))
                .with_color(Color::Yellow)
            }))
            .finish()
            .write(ariadne::sources(self.sources()), w)
    }

    pub fn write_errors<W>(&self, w: W, errors: Vec<Error>) -> io::Result<()>
    where
        W: Write + Copy,
    {
        for error in errors.into_iter() {
            self.write_error_message(w, error.span, error.message, error.reason, error.contexts)?;
        }
        Ok(())
    }

    fn source_id_string(&self, span: &Span) -> &str {
        use chumsky::span::Span;

        let source_index: usize = span.context().into();
        &self.path_content[source_index].1
    }

    fn sources(&self) -> Vec<(String, &str)> {
        self.path_content
            .iter()
            .map(|(_, source_id_string, content)| (source_id_string.to_string(), content.as_str()))
            .collect()
    }
}

fn path_to_string<P>(path: P) -> String
where
    P: AsRef<Path>,
{
    path.as_ref().to_string_lossy().into_owned()
}

impl std::fmt::Debug for BeancountSources {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "BeancountSources(",)?;

        for (path, _, content) in &self.path_content {
            writeln!(f, "    {} ok len {},", path.display(), content.len())?;
        }

        for (path, error) in &self.path_errors {
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
    // indexed by source_id as per sources
    tokenized_sources: Vec<Vec<SpannedToken<'t>>>,
}

impl<'s, 't> BeancountParser<'s, 't>
where
    's: 't,
{
    pub fn new(sources: &'s BeancountSources) -> Self {
        let mut tokenized_sources = Vec::new();

        // TODO make a better iterator for this
        for (i, (_, _, content)) in sources.path_content.iter().enumerate() {
            tokenized_sources.push(lex(SourceId::from(i), content));
        }

        BeancountParser {
            sources,
            tokenized_sources,
        }
    }

    /// Parse the sources, returning declarations or errors.
    /// No date ordering is performed.  But see [BeancountStore].
    pub fn parse(&'t self) -> Result<Vec<Spanned<Directive<'t>>>, Vec<Error>>
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
                    builder.declaration(declaration)
                }
            }

            builder.build().collect::<Vec<_>>()
        })
    }

    /// Parse the sources, returning declarations or errors.
    fn parse_declarations(
        &'t self,
    ) -> Result<HashMap<&'t Path, Vec<Spanned<Declaration<'t>>>>, Vec<Error>>
    where
        's: 't,
    {
        let mut all_outputs = HashMap::new();
        let mut all_errors = Vec::new();

        for (i, (pathbuf, _, content)) in self.sources.path_content.iter().enumerate() {
            let source_id = SourceId::from(i);
            let path = pathbuf.as_path();

            let tokens = &self.tokenized_sources[i];
            let spanned_tokens = tokens
                .spanned(end_of_input(source_id, content))
                .with_context(source_id);

            let (output, errors) = file().parse(spanned_tokens).into_output_errors();

            if let Some(output) = output {
                all_outputs.insert(
                    path,
                    output, // .into_iter()
                           // .map(|(d, span)| (d, Location::new(path, span))),
                );
            }

            if !errors.is_empty() {
                all_errors.extend(errors.into_iter().map(Error::from));
            }
        }

        if all_errors.is_empty() {
            Ok(all_outputs)
        } else {
            Err(all_errors)
        }
    }
}

/// Builder for iterator for date-ordered traversal of `Directive`s.
/// Importantly, directives with the same date must be preserved in source file order.
#[derive(Default, Debug)]
struct DirectiveIteratorBuilder<'t> {
    date_buckets: BTreeMap<Date, Vec<Spanned<Directive<'t>>>>,
    // TODO tags and metadata from push/pop pragma processing
}

impl<'t> DirectiveIteratorBuilder<'t> {
    fn new() -> Self {
        Self::default()
    }

    fn declaration<'a>(&'a mut self, declaration: Spanned<Declaration<'t>>)
    where
        't: 'a,
    {
        use Declaration::*;

        match declaration.value {
            Directive(directive) => {
                let date = *directive.date();
                let directive = spanned(directive, declaration.span);

                match self.date_buckets.get_mut(&date) {
                    None => {
                        self.date_buckets.insert(date, Vec::new());
                        let bucket = self.date_buckets.get_mut(&date).unwrap();
                        bucket.push(directive);
                    }
                    Some(directives) => directives.push(directive),
                }
            }
            Pragma(_pragma) => {
                // TODO
            }
        }
    }

    fn build(&mut self) -> impl Iterator<Item = Spanned<Directive<'t>>> {
        let date_buckets = std::mem::take(&mut self.date_buckets);
        date_buckets
            .into_iter()
            .flat_map(|(_date, directives)| directives.into_iter())
    }
}

#[cfg(test)]
pub use lexer::bare_lex;
pub use lexer::dump_tokens;
mod lexer;
mod parsers;
pub mod types;
