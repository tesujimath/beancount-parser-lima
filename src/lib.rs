// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]

use ariadne::{Color, Label, Report};
use chumsky::prelude::{Input, Parser};
use lazy_format::lazy_format;
use lexer::{lex, Token};
use parsers::{file, includes, ParserState};
use path_clean::PathClean;
use sort::SortIteratorAdaptor;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::{self, Formatter},
    fs::File,
    io::{self, stderr, Read, Write},
    path::{Path, PathBuf},
};
pub use types::*;

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

    pub fn write<W, K>(&self, w: W, errors_or_warnings: Vec<ErrorOrWarning<K>>) -> io::Result<()>
    where
        W: Write + Copy,
        K: ErrorOrWarningKind,
    {
        for error_or_warning in errors_or_warnings.into_iter() {
            use chumsky::span::Span;

            let src_id = self.source_id_string(&error_or_warning.span);
            let color = error_or_warning.color();
            let report_kind = error_or_warning.report_kind();

            Report::build(report_kind, src_id.to_string(), error_or_warning.span.start)
                .with_message(error_or_warning.message)
                .with_labels(Some(
                    Label::new((
                        src_id.to_string(),
                        error_or_warning.span.start()..error_or_warning.span.end(),
                    ))
                    .with_message(error_or_warning.reason)
                    .with_color(color),
                ))
                .with_labels(error_or_warning.contexts.into_iter().map(|(label, span)| {
                    Label::new((
                        self.source_id_string(&span).to_string(),
                        span.start()..span.end(),
                    ))
                    .with_message(lazy_format!("in this {}", label))
                    .with_color(Color::Yellow)
                }))
                .with_labels(error_or_warning.related.into_iter().map(|(label, span)| {
                    Label::new((
                        self.source_id_string(&span).to_string(),
                        span.start()..span.end(),
                    ))
                    .with_message(lazy_format!("{}", label))
                    .with_color(Color::Yellow)
                }))
                .finish()
                .write(ariadne::sources(self.sources()), w)?;
        }
        Ok(())
    }

    fn source_id_string(&self, span: &Span) -> &str {
        use chumsky::span::Span;

        let source_index: usize = span.context().into();
        &self.path_content[source_index].1
    }

    /// Can't fail as SourceIds are constructed here
    fn path(&self, source_id: SourceId) -> &Path {
        let i: usize = source_id.into();
        self.path_content[i].0.as_path()
    }

    fn sources(&self) -> Vec<(String, &str)> {
        self.path_content
            .iter()
            .map(|(_, source_id_string, content)| (source_id_string.to_string(), content.as_str()))
            .collect()
    }

    fn content_iter(&self) -> impl Iterator<Item = (SourceId, &Path, &str)> {
        self.path_content
            .iter()
            .enumerate()
            .map(|(i, (pathbuf, _, content))| {
                (SourceId::from(i), pathbuf.as_path(), content.as_str())
            })
    }

    /// Build a source_id lookup from path
    fn source_id_lookup(&self) -> HashMap<&Path, SourceId> {
        let mut lookup = HashMap::new();

        for (source_id, pathbuf) in self
            .path_content
            .iter()
            .enumerate()
            .map(|(i, (pathbuf, _, _))| (SourceId::from(i), pathbuf))
        {
            lookup.insert(pathbuf.as_path(), source_id);
        }

        lookup
    }
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

// We seem to need to actual input type in places, ugh!
// It makes me sad that I had to do this.  Perhaps when the
// dust has settled I'll find a way to get rid of it.
type ConcreteInput<'t> = chumsky::input::WithContext<
    Span,
    chumsky::input::SpannedInput<Token<'t>, Span, &'t [(Token<'t>, Span)]>,
>;

/// result of parsing
pub struct ParseResult<'t> {
    pub directives: Vec<Spanned<Directive<'t>>>,
    pub options: Options<'t>,
    pub warnings: Vec<Warning>,
}

/// error result of parsing
pub struct ParseError {
    pub errors: Vec<Error>,
    pub warnings: Vec<Warning>,
}

// result of parse_declarations
type ParseDeclarationsResult<'t> = (
    Vec<Vec<Spanned<Declaration<'t>>>>,
    Options<'t>,
    Vec<Error>,
    Vec<Warning>,
);

impl<'s, 't> BeancountParser<'s, 't>
where
    's: 't,
{
    pub fn new(sources: &'s BeancountSources) -> Self {
        let mut tokenized_sources = Vec::new();

        for (source_id, _source_path, content) in sources.content_iter() {
            tokenized_sources.push(lex(source_id, content));
        }

        BeancountParser {
            sources,
            tokenized_sources,
        }
    }

    /// Parse the sources, returning date-sorted directives and options, or errors, along with warnings in both cases.
    pub fn parse(&'t self) -> Result<ParseResult<'t>, ParseError>
    where
        's: 't,
    {
        let (all_declarations, options, mut errors, warnings) = self.parse_declarations();
        let mut p = PragmaProcessor::new(all_declarations, options);

        let directives = p
            .by_ref()
            .sort(|d| *d.item().date().item())
            .collect::<Vec<_>>();
        let (options, mut pragma_errors) = p.result();
        errors.append(&mut pragma_errors);

        if errors.is_empty() {
            Ok(ParseResult {
                directives,
                options,
                warnings,
            })
        } else {
            Err(ParseError { errors, warnings })
        }
    }

    /// Parse the sources, returning declarations and any errors.
    /// The declarations are indexed by SourceId
    fn parse_declarations(&'t self) -> ParseDeclarationsResult<'t>
    where
        's: 't,
    {
        let mut all_outputs = Vec::new();
        let mut all_errors = Vec::new();
        let mut parser_state = ParserState::default();

        for (source_id, source_path, content) in self.sources.content_iter() {
            let i_source: usize = source_id.into();
            let tokens = &self.tokenized_sources[i_source];

            // type assertion here is to ensure we keep these in step
            let spanned_tokens: ConcreteInput = tokens
                .spanned(end_of_input(source_id, content))
                .with_context(source_id);

            let (output, errors) = file(source_path)
                .parse_with_state(spanned_tokens, &mut parser_state)
                .into_output_errors();

            all_outputs.push(output.unwrap_or(Vec::new()));
            all_errors.extend(errors);
        }

        let ParserState { options, warnings } = parser_state;

        (
            all_outputs,
            Options::new(options),
            all_errors.into_iter().map(Error::from).collect(),
            warnings,
        )
    }
}

/// `PragmaProcessor` is an iterator which folds in the pragmas into the sequence of `Directive`s.
///
/// When the iterator is exhausted, any errors should be collected by the caller.
#[derive(Debug)]
struct PragmaProcessor<'s, 't> {
    included: HashMap<&'s Path, Span>,
    current: VecDeque<Spanned<Declaration<'t>>>,
    stacked: VecDeque<VecDeque<Spanned<Declaration<'t>>>>,
    remaining: VecDeque<VecDeque<Spanned<Declaration<'t>>>>,
    // tags and meta key/values for pragma push/pop
    tags: HashSet<Spanned<&'t Tag<'t>>>,
    meta_key_values: HashMap<Spanned<&'t Key<'t>>, Spanned<MetaValue<'t>>>,
    options: Options<'t>,
    // errors, for collection when the iterator is exhausted
    errors: Vec<Error>,
}

impl<'s, 't> PragmaProcessor<'s, 't> {
    fn new(all_declarations: Vec<Vec<Spanned<Declaration<'t>>>>, options: Options<'t>) -> Self {
        let mut remaining = all_declarations
            .into_iter()
            .map(VecDeque::from)
            .collect::<VecDeque<_>>();

        let current = remaining.pop_front().unwrap_or(VecDeque::new());

        PragmaProcessor {
            included: HashMap::new(),
            current,
            stacked: VecDeque::new(),
            remaining,
            tags: HashSet::new(),
            meta_key_values: HashMap::new(),
            options,
            errors: Vec::new(),
        }
    }

    fn result(self) -> (Options<'t>, Vec<Error>) {
        (self.options, self.errors)
    }
}

impl<'s, 't> Iterator for PragmaProcessor<'s, 't> {
    type Item = Spanned<Directive<'t>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current.pop_front() {
            Some(declaration) => {
                match declaration.item {
                    Declaration::Directive(mut directive) => {
                        directive.metadata.merge_tags(&self.tags, &mut self.errors);
                        directive
                            .metadata
                            .merge_key_values(&self.meta_key_values, &mut self.errors);

                        Some(spanned(directive, declaration.span))
                    }

                    Declaration::Pragma(pragma) => {
                        use Pragma::*;

                        match pragma {
                            Pushtag(tag) => {
                                self.tags.insert(tag);
                            }
                            Poptag(tag) => {
                                self.tags.remove(&tag);
                            }
                            Pushmeta(meta) => {
                                self.meta_key_values.insert(meta.key, meta.value);
                            }
                            Popmeta(key) => {
                                self.meta_key_values.remove(&key);
                            }

                            Include(_path) => {
                                // TODO need to validate the path against what we have in remaining
                                match self.remaining.pop_front() {
                                    Some(mut switcheroo) => {
                                        std::mem::swap(&mut self.current, &mut switcheroo);
                                        self.stacked.push_front(switcheroo);
                                    }

                                    None => {
                                        eprintln!("warning, include found but nothing remaining");
                                    }
                                }
                            }

                            Option(opt) => {
                                if let Err(e) = self.options.assimilate(opt) {
                                    self.errors.push(e);
                                }
                            }
                        }

                        // having silently consumed a pragma, go on to the next declaration
                        self.next()
                    }
                }
            }
            None => match self.stacked.pop_front() {
                Some(current) => {
                    self.current = current;
                    self.next()
                }
                None => None,
            },
        }
    }
}

fn end_of_input(source_id: SourceId, s: &str) -> Span {
    chumsky::span::Span::new(source_id, s.len()..s.len())
}

#[cfg(test)]
pub use lexer::bare_lex;
pub use lexer::dump_tokens;
mod format;
mod lexer;
pub use options::Options;
mod options;
mod parsers;
mod sort;
pub mod types;
