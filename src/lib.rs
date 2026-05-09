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
//!    parse(&sources, &parser, &mut io::stderr());
//!}
//!
//!fn parse<W>(sources: &BeancountSources, parser: &BeancountParser, error_w: &mut W)
//!where
//!    W: Write,
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
//!            sources.write_errors_or_warnings(error_w, errors).unwrap();
//!            sources.write_errors_or_warnings(error_w, warnings).unwrap();
//!        }
//!
//!        Err(ParseError { errors, warnings }) => {
//!            sources.write_errors_or_warnings(error_w, errors).unwrap();
//!            sources.write_errors_or_warnings(error_w, warnings).unwrap();
//!        }
//!    }
//!}
//!```

use chumsky::prelude::{Input, Parser};
use lexer::{lex, Token};
use parsers::{file, ParserState};
use sort::SortIteratorAdaptor;
use std::{
    collections::{HashMap, VecDeque},
    path::{Path, PathBuf},
};

use crate::{parsers::includes, sources::resolve_included_path};
pub use crate::{trim::trim_trailing_whitespace, types::*};

#[derive(Clone, Debug)]
enum IncludedGlob {
    Expanded(Vec<PathBuf>), // the content and its char indices
    Error(String),
}

// get all includes, discarding errors
fn get_includes(content: &str, source_id: SourceId) -> Vec<String> {
    fn get_includes_for_tokens(
        tokens: Vec<(Token, Span_)>,
        source_id: SourceId,
        end_of_input: Span_,
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

pub fn lex_with_source<'a>(source_id: SourceId, s: &'a str) -> Vec<(Token<'a>, Span_)> {
    lex(s)
        .map(|(tok, span)| (tok, chumsky::span::Span::new(source_id, span)))
        .collect::<Vec<_>>()
}

type SpannedToken<'t> = (Token<'t>, Span_);

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
///     parse(&sources, &parser, &mut io::stderr());
/// }
///
/// fn parse<W>(sources: &BeancountSources, parser: &BeancountParser, error_w: &mut W)
/// where
///     W: Write,
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
///             sources.write_errors_or_warnings(error_w, warnings).unwrap();
///         }
///         Err(ParseError { errors, warnings }) => {
///             sources.write_errors_or_warnings(error_w, errors).unwrap();
///             sources.write_errors_or_warnings(error_w, warnings).unwrap();
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
        // `content_iter()` walks a `HashMap`, so iteration order is not guaranteed.
        // We must index by `SourceId` rather than relying on iteration order.
        let mut tokenized_sources = vec![Vec::new(); sources.num_sources()];

        for (source_id, _path, content) in sources.content_iter() {
            let i_source: usize = source_id.into();
            tokenized_sources[i_source] = lex_with_source(source_id, content);
        }

        BeancountParser {
            sources,
            tokenized_sources,
        }
    }

    /// Parse the sources, returning date-sorted directives and options, or errors, along with warnings in both cases.
    pub fn parse<'a>(&'a self) -> Result<ParseSuccess<'a>, ParseError> {
        let (parsed_sources, options, mut errors, mut warnings) = self.parse_declarations();
        let mut p = PragmaProcessor::new(
            self.root_path(),
            parsed_sources,
            self.sources.included_globs(),
            self.sources.error_paths(),
            options,
        );

        // directives are stable-sorted by date, with a secondary key enforcing the stream
        // invariants from https://beancount.github.io/docs/beancount_design_doc.html#stream-invariants:
        // Open sorts before Transaction; Balance sorts before Transaction.
        let directives = p
            .by_ref()
            .sort(|d| {
                (
                    *d.item().date().item(),
                    match d.variant() {
                        DirectiveVariant::Open(_) => 0u8,
                        DirectiveVariant::Balance(_) => 1u8,
                        _ => 2u8,
                    },
                )
            })
            .collect::<Vec<_>>();
        let (options, plugins, mut pragma_errors, mut pragma_warnings) = p.result();
        errors.append(&mut pragma_errors);
        warnings.append(&mut pragma_warnings);

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
        self.sources.root_path()
    }

    /// Parse the sources, returning declarations and any errors.
    /// The declarations are indexed by SourceId
    fn parse_declarations<'a>(&'a self) -> ParseDeclarationsResult<'a> {
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
    included_globs: &'s HashMap<PathBuf, IncludedGlob>,
    error_paths: HashMap<Option<PathBuf>, String>,
    include_by_canonical_path: HashMap<PathBuf, IncludeContext<'s>>,
    // tags and meta key/values for pragma push/pop
    tags: HashMap<Spanned<Tag<'s>>, Vec<Spanned<Tag<'s>>>>,
    meta_key_values: HashMap<Spanned<Key<'s>>, Vec<(Span, Spanned<MetaValue<'s>>)>>,
    options: Options<'s>,
    plugins: Vec<Plugin<'s>>,
    // errors and warnings, for collection when the iterator is exhausted
    errors: Vec<Error>,
    warnings: Vec<Warning>,
}

#[derive(Debug)]
struct IncludeContext<'s> {
    tags: HashMap<Spanned<Tag<'s>>, Vec<Spanned<Tag<'s>>>>,
    meta_key_values: HashMap<Spanned<Key<'s>>, Vec<(Span, Spanned<MetaValue<'s>>)>>,
    span: Span,
}

fn fmt_include_context<'s>(
    tags: &HashMap<Spanned<Tag<'s>>, Vec<Spanned<Tag<'s>>>>,
    meta_key_values: &HashMap<Spanned<Key<'s>>, Vec<(Span, Spanned<MetaValue<'s>>)>>,
) -> String {
    let tags_s = itertools::intersperse(
        tags.keys().map(|tag| tag.item().to_string()),
        " ".to_string(),
    )
    .collect::<String>();
    let meta_s = itertools::intersperse(
        meta_key_values.iter().map(|(k, v)| {
            format!(
                "{}: [{}]",
                k.item(),
                itertools::intersperse(
                    v.iter().map(|(_, v)| v.item().to_string()),
                    " ".to_string()
                )
                .collect::<String>()
            )
        }),
        " ".to_string(),
    )
    .collect::<String>();
    format!("{} {}", tags_s, meta_s)
}

impl<'s> PragmaProcessor<'s> {
    fn new(
        root_path: Option<&Path>,
        parsed_sources: HashMap<Option<&Path>, Vec<Spanned<Declaration<'s>>>>,
        included_globs: &'s HashMap<PathBuf, IncludedGlob>,
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
            included_globs,
            error_paths,
            include_by_canonical_path: HashMap::default(),
            tags: HashMap::new(),
            meta_key_values: HashMap::new(),
            options,
            plugins: Vec::new(),
            errors: Vec::new(),
            warnings: Vec::new(),
        }
    }

    fn result(self) -> (Options<'s>, Vec<Plugin<'s>>, Vec<Error>, Vec<Warning>) {
        // any leftover tags or key/values is an error
        let mut errors = self.errors;
        let warnings = self.warnings;

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

        (self.options, self.plugins, errors, warnings)
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
                            Include(rel_glob) => {
                                let (abs_glob, span) = (
                                    resolve_included_path(
                                        self.current_path.as_ref(),
                                        AsRef::<Path>::as_ref(*rel_glob.item()),
                                    ),
                                    *rel_glob.span(),
                                );

                                match self.included_globs.get(&abs_glob) {
                                    None => panic!("impossible, I hope"),
                                    Some(IncludedGlob::Expanded(paths)) => {
                                        if paths.is_empty() {
                                            // this is an error rather than a warning to catch plain paths whic fail to match
                                            let e =
                                                Error::new("include failed", "no such file", span);

                                            self.errors.push(e)
                                        }

                                        for included in paths {
                                            let included = Some(included.clone());

                                            match self.remaining.remove_entry(&included) {
                                                Some((included_path, included_declarations)) => {
                                                    let stacked_path = std::mem::replace(
                                                        &mut self.current_path,
                                                        included_path,
                                                    );
                                                    let stacked_declarations = std::mem::replace(
                                                        &mut self.current_declarations,
                                                        included_declarations,
                                                    );
                                                    self.stacked.push_front((
                                                        stacked_path,
                                                        stacked_declarations,
                                                    ));

                                                    // record the span in case of a duplicate include error later
                                                    if let Ok(canonical_path) = self
                                                        .current_path
                                                        .as_ref()
                                                        .unwrap()
                                                        .canonicalize()
                                                    {
                                                        self.include_by_canonical_path.insert(
                                                            canonical_path,
                                                            IncludeContext {
                                                                tags: self.tags.clone(),
                                                                meta_key_values: self
                                                                    .meta_key_values
                                                                    .clone(),
                                                                span,
                                                            },
                                                        );
                                                    }
                                                }

                                                None => {
                                                    // either a known error path or a duplicate include
                                                    if let Some(e) = self.error_paths.get(&included)
                                                    {
                                                        self.errors.push(Error::new(
                                                            "can't read file",
                                                            e.to_string(),
                                                            span,
                                                        ));
                                                    } else {
                                                        // duplicate include, only allowed if the include context is the same

                                                        let e = Error::new(
                                                            "duplicate include",
                                                            format!(
                                                                "context {}",
                                                                fmt_include_context(
                                                                    &self.tags,
                                                                    &self.meta_key_values
                                                                )
                                                            ),
                                                            span,
                                                        );

                                                        // relate the error to the first include if we can
                                                        let e = if let Some(canonical_path) =
                                                            included.and_then(|included| {
                                                                included.canonicalize().ok()
                                                            }) {
                                                            if let Some(include_context) = self
                                                                .include_by_canonical_path
                                                                .get(&canonical_path)
                                                            {
                                                                if include_context.tags == self.tags
                                                                    && include_context
                                                                        .meta_key_values
                                                                        == self.meta_key_values
                                                                {
                                                                    // include context is identical, so the include is harmless
                                                                    // and we ignore it
                                                                    None
                                                                } else {
                                                                    Some(e.related_to_named_span(
                                                                        format!("context {}", fmt_include_context(&include_context.tags, &include_context.meta_key_values)),
                                                                        include_context.span,
                                                                    ))
                                                                }
                                                            } else {
                                                                Some(e)
                                                            }
                                                        } else {
                                                            Some(e)
                                                        };

                                                        if let Some(e) = e {
                                                            self.errors.push(e);
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    Some(IncludedGlob::Error(e)) => {
                                        let e = Error::new("can't expand glob", e, span);
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

fn end_of_input(source_id: SourceId, s: &str) -> Span_ {
    chumsky::span::Span::new(source_id, s.len()..s.len())
}

#[cfg(test)]
pub use lexer::bare_lex;
mod format;
mod lexer;
pub use options::Options;
pub(crate) mod options;
mod parsers;
mod sort;
mod sources;
pub use sources::{BeancountSources, SyntheticSources};
mod trim;
pub mod types;
