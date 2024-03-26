// TODO remove suppression for dead code warning
#![allow(dead_code)]
#![recursion_limit = "256"]
#![doc = include_str!("../README.md")]

//! # Examples
//!
//! This example generates the output as shown above.
//! The supporting function `parse` is required in order to avoid lifetime problems.
//!
//!```
//! # use rust_decimal::Decimal;
//! # use std::io::{self, Write};
//! # use std::path::PathBuf;
//!
//!use beancount_parser_lima::{
//!    BeancountParser, BeancountSources, DirectiveVariant, ParseError, ParseSuccess,
//!};
//!
//!fn main() {
//!    let sources = BeancountSources::try_from(PathBuf::from("examples/data/error-post-balancing.beancount")).unwrap();
//!    let parser = BeancountParser::new(&sources);
//!
//!    parse(&sources, &parser, &io::stderr());
//!}
//!
//!fn parse<W>(sources: &BeancountSources, parser: &BeancountParser, error_w: W)
//!where
//!    W: Write + Copy,
//!{
//!    match parser.parse() {
//!        Ok(ParseSuccess {
//!            directives,
//!            options: _,
//!            plugins: _,
//!            mut warnings,
//!        }) => {
//!            let mut errors = Vec::new();
//!
//!            for directive in directives {
//!                if let DirectiveVariant::Transaction(transaction) = directive.variant() {
//!                    let mut postings = transaction.postings().collect::<Vec<_>>();
//!                    let n_postings = postings.len();
//!                    let n_amounts = itertools::partition(&mut postings, |p| p.amount().is_some());
//!
//!                    if postings.is_empty() {
//!                        warnings.push(directive.warning("no postings"));
//!                    } else if n_amounts + 1 < n_postings {
//!                        errors.push(
//!                            directive
//!                                .error("multiple postings without amount specified")
//!                                .related_to_all(postings[n_amounts..].iter().copied()),
//!                        );
//!                    } else if n_amounts == n_postings {
//!                        let total: Decimal =
//!                            postings.iter().map(|p| p.amount().unwrap().value()).sum();
//!
//!                        if total != Decimal::ZERO {
//!                            let last_amount = postings.pop().unwrap().amount().unwrap();
//!                            let other_amounts = postings.iter().map(|p| p.amount().unwrap());
//!
//!                            errors.push(
//!                                last_amount
//!                                    .error(format!("sum is {}, expected zero", total))
//!                                    .related_to_all(other_amounts)
//!                                    .in_context(&directive),
//!                            )
//!                        }
//!                    }
//!                }
//!            }
//!
//!            sources.write(error_w, errors).unwrap();
//!            sources.write(error_w, warnings).unwrap();
//!        }
//!
//!        Err(ParseError { errors, warnings }) => {
//!            sources.write(error_w, errors).unwrap();
//!            sources.write(error_w, warnings).unwrap();
//!        }
//!    }
//!}
//!```

use ariadne::{Color, Label, Report};
use chumsky::prelude::{Input, Parser};
use lazy_format::lazy_format;
use lexer::{lex, Token};
use parsers::{file, includes, ParserState};
use sort::SortIteratorAdaptor;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    ffi::OsStr,
    fmt::{self, Formatter},
    fs::File,
    io::{self, Read, Write},
    iter::once,
    path::{Path, PathBuf},
};
pub use types::*;

/// Contains the content of the Beancount source file, and the content of
/// the transitive closure of all the include'd source files.
///
/// Zero-copy parsing means that all string values are returned as references into these strings.
///
/// # Examples
/// ```
/// # use std::path::PathBuf;
/// use beancount_parser_lima::{BeancountParser, BeancountSources};
///
/// let sources = BeancountSources::try_from(PathBuf::from("examples/data/full.beancount")).unwrap();
/// let beancount_parser = BeancountParser::new(&sources);
///
/// let result = beancount_parser.parse();
/// ```
pub struct BeancountSources {
    root_path: Option<PathBuf>,
    root_source_id: SourceId,
    root_content: String,
    included_content: HashMap<PathBuf, IncludedSource>,
    source_id_strings: Vec<String>, // indexed by SourceId
}

enum IncludedSource {
    Content(SourceId, String),
    IoError(io::Error),
    Duplicate,
}

// get all includes, discarding errors
fn get_includes(content: &str, source_id: SourceId) -> Vec<String> {
    fn get_includes_for_tokens(
        tokens: Vec<(Token, Span)>,
        source_id: SourceId,
        end_of_input: Span,
    ) -> Vec<String> {
        let spanned_tokens = tokens.spanned(end_of_input).with_context(source_id);

        // ignore any errors in parsing, we'll pick them up in the next pass
        includes()
            .parse(spanned_tokens)
            .into_output()
            .unwrap_or_default()
    }

    let tokens = lex_with_source(source_id, content);
    get_includes_for_tokens(tokens, source_id, end_of_input(source_id, content))
}

// get directory for a path if any
fn path_dir(p: &Path) -> Option<&Path> {
    p.parent().and_then(|p| {
        if !AsRef::<OsStr>::as_ref(&p).is_empty() {
            Some(p)
        } else {
            None
        }
    })
}

// get included path relative to including path
fn resolve_included_path(including_path: Option<&PathBuf>, included_path: &Path) -> PathBuf {
    match including_path.and_then(|p| path_dir(p.as_ref())) {
        Some(p) => p.join(included_path),
        None => included_path.to_path_buf(),
    }
}

impl BeancountSources {
    fn try_read_with_includes(root_path: PathBuf) -> io::Result<Self> {
        let root_content = read(&root_path)?;
        Ok(Self::read_with_includes(Some(root_path), root_content))
    }

    fn read_with_includes(root_path: Option<PathBuf>, root_content: String) -> Self {
        let root_source_id = SourceId::default();
        let root_source_id_string = root_path
            .as_ref()
            .map(|p| p.to_string_lossy().into())
            .unwrap_or("inline".to_string());
        let mut source_id_strings = Vec::from([root_source_id_string]);

        let mut pending_paths = get_includes(&root_content, root_source_id)
            .into_iter()
            .map(|included_path| resolve_included_path(root_path.as_ref(), included_path.as_ref()))
            .collect::<VecDeque<_>>();

        let mut included_content: HashMap<PathBuf, IncludedSource> = HashMap::new();

        // for duplicate detection
        let mut canonical_paths =
            HashSet::from([root_path.as_ref().and_then(|p| p.canonicalize().ok())]);

        while !pending_paths.is_empty() {
            let path = pending_paths.pop_front().unwrap();
            let canonical_path = path.canonicalize().ok();

            if canonical_paths.contains(&canonical_path) {
                included_content.insert(path, IncludedSource::Duplicate);
            } else {
                canonical_paths.insert(canonical_path);

                let source_id = SourceId::from(source_id_strings.len());
                source_id_strings.push(path.to_string_lossy().into());

                let included_source = read(&path).map_or_else(IncludedSource::IoError, |c| {
                    IncludedSource::Content(source_id, c)
                });

                // stabilisation of VacantEntry::insert_entry() would enable us to avoid cloning the path here
                // and doing an immediate lookup
                included_content.insert(path.clone(), included_source);
                let included_source = included_content.get(&path).unwrap();

                if let IncludedSource::Content(_, content) = included_source {
                    let mut includes = get_includes(content, source_id)
                        .into_iter()
                        .map(|included_path| {
                            resolve_included_path(Some(&path), included_path.as_ref())
                        })
                        .collect::<VecDeque<_>>();
                    pending_paths.append(&mut includes);
                }
            }
        }

        Self {
            root_path,
            root_source_id,
            root_content,
            included_content,
            source_id_strings,
        }
    }

    pub fn write<W, K>(&self, w: W, errors_or_warnings: Vec<ErrorOrWarning<K>>) -> io::Result<()>
    where
        W: Write + Copy,
        K: ErrorOrWarningKind,
    {
        for error_or_warning in errors_or_warnings.into_iter() {
            use chumsky::span::Span;

            let src_id = self.span_source_id_string(&error_or_warning.span);
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
                        self.span_source_id_string(&span).to_string(),
                        span.start()..span.end(),
                    ))
                    .with_message(lazy_format!("in this {}", label))
                    .with_color(Color::Yellow)
                }))
                .with_labels(error_or_warning.related.into_iter().map(|(label, span)| {
                    Label::new((
                        self.span_source_id_string(&span).to_string(),
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

    fn span_source_id_string(&self, span: &Span) -> &str {
        use chumsky::span::Span;

        self.source_id_string(span.context())
    }

    fn source_id_string(&self, source_id: SourceId) -> &str {
        self.source_id_strings[Into::<usize>::into(source_id)].as_str()
    }

    fn sources(&self) -> Vec<(String, &str)> {
        once((
            self.source_id_string(self.root_source_id).to_string(),
            self.root_content.as_str(),
        ))
        .chain(
            self.included_content
                .iter()
                .filter_map(|(_, included_source)| {
                    if let IncludedSource::Content(source_id, content) = included_source {
                        Some((
                            self.source_id_string(*source_id).to_string(),
                            content.as_str(),
                        ))
                    } else {
                        None
                    }
                }),
        )
        .collect()
    }

    fn content_iter(&self) -> impl Iterator<Item = (SourceId, Option<&Path>, &str)> {
        once((
            self.root_source_id,
            self.root_path.as_deref(),
            self.root_content.as_str(),
        ))
        .chain(
            self.included_content
                .iter()
                .filter_map(|(pathbuf, included_source)| {
                    if let IncludedSource::Content(source_id, content) = included_source {
                        Some((*source_id, Some(pathbuf.as_path()), content.as_str()))
                    } else {
                        None
                    }
                }),
        )
    }

    fn error_path_iter(&self) -> impl Iterator<Item = (Option<&Path>, &io::Error)> {
        self.included_content
            .iter()
            .filter_map(|(pathbuf, included_source)| {
                if let IncludedSource::IoError(e) = included_source {
                    Some((Some(pathbuf.as_path()), e))
                } else {
                    None
                }
            })
    }
}

impl TryFrom<PathBuf> for BeancountSources {
    type Error = io::Error;

    fn try_from(source_path: PathBuf) -> io::Result<Self> {
        Self::try_read_with_includes(source_path)
    }
}

impl TryFrom<&Path> for BeancountSources {
    type Error = io::Error;

    fn try_from(source_path: &Path) -> io::Result<Self> {
        Self::try_read_with_includes(source_path.to_owned())
    }
}

impl From<String> for BeancountSources {
    fn from(source_string: String) -> Self {
        Self::read_with_includes(None, source_string)
    }
}

impl From<&str> for BeancountSources {
    fn from(source_string: &str) -> Self {
        Self::read_with_includes(None, source_string.to_owned())
    }
}

impl std::fmt::Debug for BeancountSources {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "BeancountSources(",)?;

        for (path, included_source) in &self.included_content {
            match included_source {
                IncludedSource::Content(source_id, content) => writeln!(
                    f,
                    "    {} ok len {},",
                    self.source_id_string(*source_id),
                    content.len()
                )?,
                IncludedSource::IoError(e) => writeln!(f, "    {:?} err {},", path, e)?,
                IncludedSource::Duplicate => writeln!(f, "    {:?} duplicate include", path)?,
            }
        }

        writeln!(f, ")",)
    }
}

pub fn lex_with_source(source_id: SourceId, s: &str) -> Vec<(Token, Span)> {
    lex(s)
        .map(|(tok, span)| (tok, chumsky::span::Span::new(source_id, span)))
        .collect::<Vec<_>>()
}

fn read<P>(file_path: P) -> io::Result<String>
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

/// The Beancount parser itself, which tokenizes and parses the source files
/// contained in `BeancountSources`.
///
/// # Examples
/// ```
/// # use std::io::{self, Write};
/// # use std::path::PathBuf;
///
/// use beancount_parser_lima::{BeancountParser, BeancountSources, ParseError, ParseSuccess};
///
/// fn main() {
///     let sources = BeancountSources::try_from(PathBuf::from("examples/data/full.beancount")).unwrap();
///     let parser = BeancountParser::new(&sources);
///
///     parse(&sources, &parser, &io::stderr());
/// }
///
/// fn parse<W>(sources: &BeancountSources, parser: &BeancountParser, error_w: W)
/// where
///     W: Write + Copy,
/// {
///     match parser.parse() {
///         Ok(ParseSuccess {
///             directives,
///             options: _,
///             plugins: _,
///             warnings,
///         }) => {
///             for directive in directives {
///                 println!("{}\n", &directive);
///             }
///    
///             sources.write(error_w, warnings).unwrap();
///         }
///         Err(ParseError { errors, warnings }) => {
///             sources.write(error_w, errors).unwrap();
///             sources.write(error_w, warnings).unwrap();
///         }
///     }
/// }
/// ````
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

/// A successful parsing all the files, containing date-ordered `Directive`s, `Options`, `Plugin`s, and any `Warning`s.
#[derive(Debug)]
pub struct ParseSuccess<'t> {
    pub directives: Vec<Spanned<Directive<'t>>>,
    pub options: Options<'t>,
    pub plugins: Vec<Plugin<'t>>,
    pub warnings: Vec<Warning>,
}

/// The value returned when parsing fails.
#[derive(Debug)]
pub struct ParseError {
    pub errors: Vec<Error>,
    pub warnings: Vec<Warning>,
}

// result of parse_declarations
type ParseDeclarationsResult<'s, 't> = (
    HashMap<Option<&'s Path>, Vec<Spanned<Declaration<'t>>>>,
    Options<'t>,
    Vec<Error>,
    Vec<Warning>,
);

impl<'s, 't> BeancountParser<'s, 't>
where
    's: 't,
{
    /// Create a `BeancountParser` from `BeancountSources` read from all input files.
    pub fn new(sources: &'s BeancountSources) -> Self {
        let mut tokenized_sources = Vec::new();

        for (source_id, _path, content) in sources.content_iter() {
            tokenized_sources.push(lex_with_source(source_id, content));
        }

        BeancountParser {
            sources,
            tokenized_sources,
        }
    }

    /// Parse the sources, returning date-sorted directives and options, or errors, along with warnings in both cases.
    pub fn parse(&'t self) -> Result<ParseSuccess<'t>, ParseError>
    where
        's: 't,
    {
        let (parsed_sources, options, mut errors, warnings) = self.parse_declarations();
        let error_paths = self.sources.error_path_iter().collect::<HashMap<_, _>>();
        let mut p = PragmaProcessor::new(self.root_path(), parsed_sources, error_paths, options);

        let directives = p
            .by_ref()
            .sort(|d| *d.item().date().item())
            .collect::<Vec<_>>();
        let (options, plugins, mut pragma_errors) = p.result();
        errors.append(&mut pragma_errors);

        if errors.is_empty() {
            Ok(ParseSuccess {
                directives,
                options,
                plugins,
                warnings,
            })
        } else {
            Err(ParseError { errors, warnings })
        }
    }

    fn root_path(&'t self) -> Option<&'s Path>
    where
        's: 't,
    {
        self.sources.root_path.as_deref()
    }

    /// Parse the sources, returning declarations and any errors.
    /// The declarations are indexed by SourceId
    fn parse_declarations(&'t self) -> ParseDeclarationsResult<'s, 't>
    where
        's: 't,
    {
        let mut all_outputs = HashMap::new();
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

            all_outputs.insert(source_path, output.unwrap_or(Vec::new()));
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

/// Iterator which applies pragmas to the sequence of `Directive`s.
///
/// When the iterator is exhausted, any errors should be collected by the caller.
#[derive(Debug)]
struct PragmaProcessor<'s, 't> {
    current_path: Option<PathBuf>,
    current_declarations: VecDeque<Spanned<Declaration<'t>>>,
    stacked: VecDeque<(Option<PathBuf>, VecDeque<Spanned<Declaration<'t>>>)>,
    remaining: HashMap<Option<PathBuf>, VecDeque<Spanned<Declaration<'t>>>>,
    error_paths: HashMap<Option<PathBuf>, &'s io::Error>,
    // tags and meta key/values for pragma push/pop
    tags: HashMap<Spanned<Tag<'t>>, Vec<Spanned<Tag<'t>>>>,
    meta_key_values: HashMap<Spanned<Key<'t>>, Vec<(Span, Spanned<MetaValue<'t>>)>>,
    options: Options<'t>,
    plugins: Vec<Plugin<'t>>,
    // errors, for collection when the iterator is exhausted
    errors: Vec<Error>,
}

impl<'s, 't> PragmaProcessor<'s, 't>
where
    's: 't,
{
    fn new(
        root_path: Option<&Path>,
        parsed_sources: HashMap<Option<&Path>, Vec<Spanned<Declaration<'t>>>>,
        error_paths: HashMap<Option<&Path>, &'s io::Error>,
        options: Options<'t>,
    ) -> Self {
        let mut remaining = parsed_sources
            .into_iter()
            .map(|(path, declarations)| {
                (path.map(|p| p.to_path_buf()), VecDeque::from(declarations))
            })
            .collect::<HashMap<_, _>>();
        let error_paths = error_paths
            .into_iter()
            .map(|(path, e)| (path.map(|p| p.to_path_buf()), e))
            .collect::<HashMap<_, _>>();

        let current_path = root_path.map(|p| p.to_path_buf());
        let current_declarations = remaining.remove(&current_path).unwrap();

        PragmaProcessor {
            current_path,
            current_declarations,
            stacked: VecDeque::new(),
            remaining,
            error_paths,
            tags: HashMap::new(),
            meta_key_values: HashMap::new(),
            options,
            plugins: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn result(self) -> (Options<'t>, Vec<Plugin<'t>>, Vec<Error>) {
        // any leftover tags or key/values is an error
        let mut errors = self.errors;

        for (key, _value) in self.meta_key_values {
            let e = Error::new(
                "invalid pushmeta",
                "missing corresponding popmeta",
                key.span,
            );
            errors.push(e);
        }

        for (tag, others) in self.tags {
            let e = Error::new("invalid pushtag", "missing corresponding poptag", tag.span);
            errors.push(e);
            for other in others {
                let e = Error::new(
                    "invalid pushtag",
                    "missing corresponding poptag",
                    other.span,
                );
                errors.push(e);
            }
        }

        (self.options, self.plugins, errors)
    }
}

impl<'s, 't> Iterator for PragmaProcessor<'s, 't>
where
    's: 't,
{
    type Item = Spanned<Directive<'t>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current_declarations.pop_front() {
            Some(declaration) => {
                match declaration.item {
                    Declaration::Directive(mut directive) => {
                        directive.metadata.augment_tags(&self.tags);
                        directive.metadata.augment_key_values(&self.meta_key_values);

                        Some(spanned(directive, declaration.span))
                    }

                    Declaration::Pragma(pragma) => {
                        use Pragma::*;

                        match pragma {
                            Pushtag(tag) => match self.tags.get_mut(&tag) {
                                Some(others) => {
                                    others.push(tag);
                                }
                                None => {
                                    self.tags.insert(tag, Vec::default());
                                }
                            },
                            Poptag(tag) => {
                                let mut last_tag = false;

                                match self.tags.get_mut(&tag) {
                                    Some(others) => {
                                        if others.is_empty() {
                                            last_tag = true;
                                            // need to remove later because of borrowing
                                        } else {
                                            others.pop();
                                        }
                                    }
                                    None => {
                                        let e = Error::new(
                                            "invalid poptag",
                                            "missing corresponding pushtag",
                                            tag.span,
                                        );
                                        self.errors.push(e);
                                    }
                                }

                                if last_tag {
                                    self.tags.remove(&tag);
                                }
                            }
                            Pushmeta(meta) => match self.meta_key_values.get_mut(&meta.key) {
                                Some(values) => {
                                    values.push((meta.key.span, meta.value));
                                }
                                None => {
                                    self.meta_key_values
                                        .insert(meta.key, vec![(meta.key.span, meta.value)]);
                                }
                            },
                            Popmeta(meta) => {
                                let mut last_meta = false;

                                match self.meta_key_values.get_mut(&meta) {
                                    Some(values) => {
                                        values.pop();
                                        if values.is_empty() {
                                            last_meta = true;
                                            // need to remove later because of borrowing
                                        }
                                    }
                                    None => {
                                        let e = Error::new(
                                            "invalid popmeta",
                                            "missing corresponding pushmeta",
                                            meta.span,
                                        );
                                        self.errors.push(e);
                                    }
                                }

                                if last_meta {
                                    self.meta_key_values.remove(&meta);
                                }
                            }
                            Include(relpath) => {
                                let (path, span) = (
                                    Some(resolve_included_path(
                                        self.current_path.as_ref(),
                                        AsRef::<Path>::as_ref(*relpath.item()),
                                    )),
                                    *relpath.span(),
                                );
                                match self.remaining.remove_entry(&path) {
                                    Some((path, mut switcheroo)) => {
                                        std::mem::swap(
                                            &mut self.current_declarations,
                                            &mut switcheroo,
                                        );
                                        self.stacked.push_front((path, switcheroo));
                                    }

                                    None => {
                                        // either a known error path or a duplicate
                                        let e = match self.error_paths.get(&path) {
                                            Some(e) => {
                                                Error::new("can't read file", e.to_string(), span)
                                            }
                                            None => Error::new(
                                                "duplicate include",
                                                "file already included",
                                                span,
                                            ),
                                        };
                                        self.errors.push(e);
                                    }
                                }
                            }

                            Option(opt) => {
                                if let Err(e) = self.options.assimilate(opt) {
                                    self.errors.push(e);
                                }
                            }

                            Plugin(plugin) => self.plugins.push(plugin),
                        }

                        // having silently consumed a pragma, go on to the next declaration
                        self.next()
                    }
                }
            }
            None => match self.stacked.pop_front() {
                Some((path, declarations)) => {
                    self.current_path = path;
                    self.current_declarations = declarations;
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
mod format;
mod lexer;
pub use options::Options;
mod options;
mod parsers;
mod sort;
pub mod types;
