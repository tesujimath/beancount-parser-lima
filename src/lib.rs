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

pub use crate::{trim::trim_trailing_whitespace, types::*};

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
#[derive(Clone)]
pub struct BeancountSources {
    root_path: Option<PathBuf>,
    root_source_id: SourceId,
    root_content: String,
    root_content_char_indices: Vec<usize>,
    included_content: HashMap<PathBuf, IncludedSource>,
    source_id_strings: Vec<String>, // indexed by SourceId
}

#[derive(Clone, Debug)]
enum IncludedSource {
    Content(SourceId, String, Vec<usize>), // the content and its char indices
    IoError(String),
    Duplicate,
}

// get all includes, discarding errors
fn get_includes(content: &str, source_id: SourceId) -> Vec<String> {
    fn get_includes_for_tokens(
        tokens: Vec<(Token, Span)>,
        source_id: SourceId,
        end_of_input: Span,
    ) -> Vec<String> {
        let mut parser_state = chumsky::extra::SimpleState(ParserState::default());

        let spanned_tokens = tokens
            .map(end_of_input, |(t, s)| (t, s))
            .with_context(source_id);

        // ignore any errors in parsing, we'll pick them up in the next pass
        includes()
            .parse_with_state(spanned_tokens, &mut parser_state)
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
                // don't overwrite existing content
                included_content
                    .entry(path)
                    .or_insert(IncludedSource::Duplicate);
            } else {
                canonical_paths.insert(canonical_path);

                let source_id = SourceId::from(source_id_strings.len());
                source_id_strings.push(path.to_string_lossy().into());

                let included_source = read(&path).map_or_else(
                    |e| IncludedSource::IoError(e.to_string()),
                    |c| {
                        // find the char indices for the content
                        // needed for mapping byte indices to char indices, to convert Chumsky spans to Ariadne spans
                        // see https://github.com/zesterer/chumsky/issues/65#issuecomment-1689216633
                        let char_indices = c.char_indices().map(|(i, _)| i).collect::<Vec<_>>();
                        tracing::debug!(
                            "content byte len {} char len {}",
                            c.len(),
                            char_indices.len()
                        );
                        IncludedSource::Content(source_id, c, char_indices)
                    },
                );

                // stabilisation of VacantEntry::insert_entry() would enable us to avoid cloning the path here
                // and doing an immediate lookup
                included_content.insert(path.clone(), included_source);
                let included_source = included_content.get(&path).unwrap();

                if let IncludedSource::Content(_, content, _) = included_source {
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

        let root_content_char_indices = root_content
            .char_indices()
            .map(|(i, _)| i)
            .collect::<Vec<_>>();

        Self {
            root_path,
            root_source_id,
            root_content,
            root_content_char_indices,
            included_content,
            source_id_strings,
        }
    }

    pub fn write<W, E, K>(&self, mut w: W, errors_or_warnings: Vec<E>) -> io::Result<()>
    where
        W: Write + Copy,
        E: Into<AnnotatedErrorOrWarning<K>>,
        K: ErrorOrWarningKind,
    {
        for error_or_warning in errors_or_warnings.into_iter() {
            use chumsky::span::Span;
            let AnnotatedErrorOrWarning {
                error_or_warning,
                annotation,
            } = error_or_warning.into();
            let (src_id, span) = self.source_id_string_and_adjusted_span(&error_or_warning.span);
            let color = error_or_warning.color();
            let report_kind = error_or_warning.report_kind();

            Report::build(report_kind, (src_id.clone(), (span.start()..span.end())))
                .with_message(error_or_warning.message)
                .with_labels(Some(
                    Label::new((src_id, (span.start()..span.end())))
                        .with_message(error_or_warning.reason)
                        .with_color(color),
                ))
                .with_labels(error_or_warning.contexts.into_iter().map(|(label, span)| {
                    let (src_id, span) = self.source_id_string_and_adjusted_span(&span);
                    Label::new((src_id, (span.start()..span.end())))
                        .with_message(lazy_format!("in this {}", label))
                        .with_color(Color::Yellow)
                }))
                .with_labels(error_or_warning.related.into_iter().map(|(label, span)| {
                    let (src_id, span) = self.source_id_string_and_adjusted_span(&span);
                    Label::new((src_id, (span.start()..span.end())))
                        .with_message(lazy_format!("{}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .write(ariadne::sources(self.sources()), w)?;

            if let Some(annotation) = annotation {
                // clippy thinks this is better than write! 🤷
                w.write_fmt(core::format_args!("{}\n", &annotation))?;
            }
        }
        Ok(())
    }

    fn byte_to_rune(&self, char_indices: &[usize], byte_span: Span) -> Span {
        let mut rune_span = byte_span;

        rune_span.start = char_indices.partition_point(|&i| i < byte_span.start);
        rune_span.end = char_indices.partition_point(|&i| i < byte_span.end);
        tracing::debug!(
            "byte_to_rune({:?}) chars in source = {}, ruined span {:?}",
            &byte_span,
            char_indices.len(),
            &rune_span
        );
        rune_span
    }

    fn source_id_string_and_adjusted_span(&self, span: &Span) -> (String, Span) {
        use chumsky::span::Span;
        let source_id = span.context();
        let source_id_str = self.source_id_string(source_id);
        let empty_char_indices = Vec::default();
        let (source_content, source_content_char_indices) = if source_id == self.root_source_id {
            (self.root_content.as_str(), &self.root_content_char_indices)
        } else if let IncludedSource::Content(_, content, content_char_indices) =
            self.included_content.get(Path::new(source_id_str)).unwrap()
        {
            (content.as_str(), content_char_indices)
        } else {
            ("", &empty_char_indices)
        };

        let trimmed = trimmed_span(source_content, span);
        let rune_span = self.byte_to_rune(source_content_char_indices, trimmed);

        tracing::debug!(
            "original span {:?} `{:?}`, trimmed {:?} `{:?}`, ruined {:?} `{:?}`",
            &span,
            &source_content[span.start..span.end],
            trimmed,
            &source_content[trimmed.start..trimmed.end],
            rune_span,
            &source_content[rune_span.start..rune_span.end]
        );
        tracing::debug!(
            "original span {:?} `{:?}`",
            &span,
            &source_content[span.start..span.end],
        );
        tracing::debug!(
            "trimmed span {:?} `{:?}`",
            trimmed,
            &source_content[trimmed.start..trimmed.end],
        );
        tracing::debug!(
            "ruined span {:?} `{:?}`",
            rune_span,
            &source_content[rune_span.start..rune_span.end]
        );

        (source_id_str.to_string(), rune_span)
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
                    if let IncludedSource::Content(source_id, content, _) = included_source {
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
                    if let IncludedSource::Content(source_id, content, _) = included_source {
                        Some((*source_id, Some(pathbuf.as_path()), content.as_str()))
                    } else {
                        None
                    }
                }),
        )
    }

    fn error_path_iter(&self) -> impl Iterator<Item = (Option<&Path>, String)> {
        self.included_content
            .iter()
            .filter_map(|(pathbuf, included_source)| {
                if let IncludedSource::IoError(e) = included_source {
                    Some((Some(pathbuf.as_path()), e.clone()))
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
                IncludedSource::Content(source_id, content, _) => writeln!(
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
pub struct BeancountParser<'s> {
    sources: &'s BeancountSources,
    // indexed by source_id as per sources
    tokenized_sources: Vec<Vec<SpannedToken<'s>>>,
}

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
type ParseDeclarationsResult<'t> = (
    HashMap<Option<&'t Path>, Vec<Spanned<Declaration<'t>>>>,
    Options<'t>,
    Vec<Error>,
    Vec<Warning>,
);

impl<'s> BeancountParser<'s> {
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
    pub fn parse(&self) -> Result<ParseSuccess, ParseError> {
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

    fn root_path(&self) -> Option<&'s Path> {
        self.sources.root_path.as_deref()
    }

    /// Parse the sources, returning declarations and any errors.
    /// The declarations are indexed by SourceId
    fn parse_declarations(&self) -> ParseDeclarationsResult {
        let mut all_outputs = HashMap::new();
        let mut all_errors = Vec::new();
        let mut parser_state = chumsky::extra::SimpleState(ParserState::default());

        for (source_id, source_path, content) in self.sources.content_iter() {
            let i_source: usize = source_id.into();
            let tokens = &self.tokenized_sources[i_source];

            let spanned_tokens = tokens
                .map(end_of_input(source_id, content), |(t, s)| (t, s))
                .with_context(source_id);

            let (output, errors) = file(source_path)
                .parse_with_state(spanned_tokens, &mut parser_state)
                .into_output_errors();

            all_outputs.insert(source_path, output.unwrap_or(Vec::new()));
            all_errors.extend(errors);
        }

        let ParserState { options, warnings } = parser_state.0;

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
struct PragmaProcessor<'s> {
    current_path: Option<PathBuf>,
    current_declarations: VecDeque<Spanned<Declaration<'s>>>,
    stacked: VecDeque<(Option<PathBuf>, VecDeque<Spanned<Declaration<'s>>>)>,
    remaining: HashMap<Option<PathBuf>, VecDeque<Spanned<Declaration<'s>>>>,
    error_paths: HashMap<Option<PathBuf>, String>,
    include_span_by_canonical_path: HashMap<PathBuf, Span>,
    // tags and meta key/values for pragma push/pop
    tags: HashMap<Spanned<Tag<'s>>, Vec<Spanned<Tag<'s>>>>,
    meta_key_values: HashMap<Spanned<Key<'s>>, Vec<(Span, Spanned<MetaValue<'s>>)>>,
    options: Options<'s>,
    plugins: Vec<Plugin<'s>>,
    // errors, for collection when the iterator is exhausted
    errors: Vec<Error>,
}

impl<'s> PragmaProcessor<'s> {
    fn new(
        root_path: Option<&Path>,
        parsed_sources: HashMap<Option<&Path>, Vec<Spanned<Declaration<'s>>>>,
        error_paths: HashMap<Option<&Path>, String>,
        options: Options<'s>,
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
            include_span_by_canonical_path: HashMap::default(),
            tags: HashMap::new(),
            meta_key_values: HashMap::new(),
            options,
            plugins: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn result(self) -> (Options<'s>, Vec<Plugin<'s>>, Vec<Error>) {
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

impl<'s> Iterator for PragmaProcessor<'s> {
    type Item = Spanned<Directive<'s>>;

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
                                let canonical_path =
                                    path.as_ref().and_then(|p| p.canonicalize().ok());

                                match self.remaining.remove_entry(&path) {
                                    Some((included_path, included_declarations)) => {
                                        let stacked_path = std::mem::replace(
                                            &mut self.current_path,
                                            included_path,
                                        );
                                        let stacked_declarations = std::mem::replace(
                                            &mut self.current_declarations,
                                            included_declarations,
                                        );
                                        self.stacked
                                            .push_front((stacked_path, stacked_declarations));

                                        // record the span in case of a duplicate include error later
                                        if let Some(canonical_path) = canonical_path {
                                            self.include_span_by_canonical_path
                                                .insert(canonical_path, span);
                                        }
                                    }

                                    None => {
                                        // either a known error path or a duplicate
                                        let e = match self.error_paths.get(&path) {
                                            Some(e) => {
                                                Error::new("can't read file", e.to_string(), span)
                                            }
                                            None => {
                                                let e = Error::new(
                                                    "duplicate include",
                                                    "file already included",
                                                    span,
                                                );

                                                // relate the error to the first include if we can
                                                if let Some(span) = canonical_path.and_then(|p| {
                                                    self.include_span_by_canonical_path.get(&p)
                                                }) {
                                                    e.related_to_named_span("file", *span)
                                                } else {
                                                    e
                                                }
                                            }
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

fn trimmed_span(source: &str, span: &Span) -> Span {
    let mut trimmed = *span;
    trimmed.end = trim_trailing_whitespace(source, span.start, span.end);
    trimmed
}

#[cfg(test)]
pub use lexer::bare_lex;
mod format;
mod lexer;
pub use options::Options;
mod options;
mod parsers;
mod sort;
mod trim;
pub mod types;
