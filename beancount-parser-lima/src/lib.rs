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
//!    let sources = BeancountSources::from(PathBuf::from("examples/data/error-post-balancing.beancount"));
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
use either::Either::{self, Left, Right};
use lazy_format::lazy_format;
use lexer::{lex, Token};
use parsers::{file, includes, ParserState};
use path_clean::PathClean;
use sort::SortIteratorAdaptor;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    env::current_dir,
    fmt::{self, Formatter},
    fs::File,
    io::{self, stderr, Read, Write},
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
/// let sources = BeancountSources::from(PathBuf::from("examples/data/full.beancount"));
/// let beancount_parser = BeancountParser::new(&sources);
///
/// let result = beancount_parser.parse();
/// ```
pub struct BeancountSources {
    // The source_id is the index in `content`, and the first of these is the root source.
    // To support parsing from inline strings, we allow for the path to be optional, but
    // there can only ever be one of these, the root.
    all_content: Vec<(Option<PathBuf>, String, String)>,
    all_errors: Vec<(PathBuf, anyhow::Error)>,
    current_dir: Option<PathBuf>,
}

impl BeancountSources {
    fn read_with_includes(root: Either<String, PathBuf>) -> Self {
        let mut all_content = Vec::new();
        let mut all_errors = Vec::new();

        let mut pending = VecDeque::from([root]);
        let mut all_paths = HashSet::new();

        let current_dir = current_dir().ok();

        while !pending.is_empty() {
            let (path, content) = match pending.pop_front().unwrap() {
                Left(content) => (None, Some(content)),
                Right(path) => {
                    all_paths.insert(path.clone());
                    match read(&path) {
                        Ok(content) => (Some(path), Some(content)),
                        Err(e) => {
                            all_errors.push((path, e));
                            (None, None)
                        }
                    }
                }
            };

            if let Some(content) = content {
                let source_id = SourceId::from(all_content.len());
                let source_id_string = path.as_ref().map_or("inline".to_owned(), |path| {
                    path.to_string_lossy().into_owned()
                });

                all_content.push((path, source_id_string, content));
                let (path, source_id_string, content) = all_content.last().unwrap();

                let tokens = Some(lex_with_source(source_id, content));
                let spanned_tokens = tokens
                    .as_ref()
                    .unwrap()
                    .spanned(end_of_input(source_id, content))
                    .with_context(source_id);

                // ignore any errors in parsing, we'll pick them up in the next pass
                if let Some(includes) = includes().parse(spanned_tokens).into_output() {
                    for include in includes {
                        let included_path =
                            path.as_ref().map_or(PathBuf::from(&include), |parent| {
                                parent.join(include).clean()
                            });
                        if !all_paths.contains(included_path.as_path()) {
                            pending.push_back(Right(included_path));
                        } else {
                            // TODO include cycle error
                            writeln!(
                                &mut stderr(),
                                "warning: ignoring include cycle in {} for {}",
                                source_id_string,
                                &included_path.display()
                            )
                            .unwrap();
                        }
                    }
                };
            }
        }

        Self {
            all_content,
            all_errors,
            current_dir,
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
        &self.all_content[source_index].1
    }

    fn sources(&self) -> Vec<(String, &str)> {
        self.all_content
            .iter()
            .map(|(_, source_id_string, content)| (source_id_string.to_string(), content.as_str()))
            .collect()
    }

    fn content_iter(&self) -> impl Iterator<Item = (SourceId, Option<&Path>, &str)> {
        self.all_content
            .iter()
            .enumerate()
            .map(|(i, (pathbuf, _, content))| {
                (
                    SourceId::from(i),
                    pathbuf.as_ref().map(|pathbuf| pathbuf.as_path()),
                    content.as_str(),
                )
            })
    }
}

impl From<PathBuf> for BeancountSources {
    fn from(source_path: PathBuf) -> Self {
        Self::read_with_includes(Right(source_path))
    }
}

impl From<&Path> for BeancountSources {
    fn from(source_path: &Path) -> Self {
        Self::read_with_includes(Right(source_path.to_owned()))
    }
}

impl From<String> for BeancountSources {
    fn from(source_string: String) -> Self {
        Self::read_with_includes(Left(source_string))
    }
}

impl From<&str> for BeancountSources {
    fn from(source_string: &str) -> Self {
        Self::read_with_includes(Left(source_string.to_owned()))
    }
}

impl std::fmt::Debug for BeancountSources {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "BeancountSources(",)?;

        for (_path, source_id_string, content) in &self.all_content {
            writeln!(f, "    {} ok len {},", source_id_string, content.len())?;
        }

        for (path, error) in &self.all_errors {
            writeln!(f, "    {} {},", path.display(), error)?;
        }

        writeln!(f, ")",)
    }
}

pub fn lex_with_source(source_id: SourceId, s: &str) -> Vec<(Token, Span)> {
    lex(s)
        .map(|(tok, span)| (tok, chumsky::span::Span::new(source_id, span)))
        .collect::<Vec<_>>()
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
///     let sources = BeancountSources::from(PathBuf::from("examples/data/full.beancount"));
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
    /// Create a `BeancountParser` from `BeancountSources` read from all input files.
    pub fn new(sources: &'s BeancountSources) -> Self {
        let mut tokenized_sources = Vec::new();

        for (source_id, _source_path, content) in sources.content_iter() {
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
        let (all_declarations, options, mut errors, warnings) = self.parse_declarations();
        let mut p = PragmaProcessor::new(all_declarations, options);

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

/// Iterator which applies pragmas to the sequence of `Directive`s.
///
/// When the iterator is exhausted, any errors should be collected by the caller.
#[derive(Debug)]
struct PragmaProcessor<'s, 't> {
    included: HashMap<&'s Path, Span>,
    current: VecDeque<Spanned<Declaration<'t>>>,
    stacked: VecDeque<VecDeque<Spanned<Declaration<'t>>>>,
    remaining: VecDeque<VecDeque<Spanned<Declaration<'t>>>>,
    // tags and meta key/values for pragma push/pop
    tags: HashSet<Spanned<Tag<'t>>>,
    meta_key_values: HashMap<Spanned<Key<'t>>, Spanned<MetaValue<'t>>>,
    options: Options<'t>,
    plugins: Vec<Plugin<'t>>,
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
            plugins: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn result(self) -> (Options<'t>, Vec<Plugin<'t>>, Vec<Error>) {
        (self.options, self.plugins, self.errors)
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

                            Plugin(plugin) => self.plugins.push(plugin),
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
mod format;
mod lexer;
pub use options::Options;
mod options;
mod parsers;
mod sort;
pub mod types;
