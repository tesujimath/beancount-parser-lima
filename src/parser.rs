use crate::{sort::SortIteratorAdaptor, types::*};
use ariadne::{Color, Label, Report, ReportKind};
use chumsky::prelude::{Input, Parser};
use lazy_format::lazy_format;
use lexer::{lex, Token};
use parsers::{file, includes};
use path_clean::PathClean;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::{self, Display, Formatter},
    fs::File,
    io::{self, stderr, Read, Write},
    path::{Path, PathBuf},
};
pub use types::*;

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

    fn content_iter(&self) -> impl Iterator<Item = (SourceId, &str)> {
        self.path_content
            .iter()
            .enumerate()
            .map(|(i, (_, _, content))| (SourceId::from(i), content.as_str()))
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

        for (source_id, content) in sources.content_iter() {
            tokenized_sources.push(lex(source_id, content));
        }

        BeancountParser {
            sources,
            tokenized_sources,
        }
    }

    /// Parse the sources, returning declarations or errors.
    /// No date ordering is performed.  But see [BeancountStore].
    pub fn parse(&'t self) -> Result<ParseResult<'s, 't>, Vec<Error>>
    where
        's: 't,
    {
        self.parse_declarations()
            .map(|declarations| ParseResult::new(DirectiveIterator::new(declarations)))
    }

    /// Parse the sources, returning declarations or errors.
    /// The declarations are indexed by SourceId
    fn parse_declarations(&'t self) -> Result<Vec<Vec<Spanned<Declaration<'t>>>>, Vec<Error>>
    where
        's: 't,
    {
        let mut all_outputs = Vec::new();
        let mut all_errors = Vec::new();

        for (source_id, content) in self.sources.content_iter() {
            let i_source: usize = source_id.into();
            let tokens = &self.tokenized_sources[i_source];

            let spanned_tokens = tokens
                .spanned(end_of_input(source_id, content))
                .with_context(source_id);

            let (output, errors) = file().parse(spanned_tokens).into_output_errors();

            all_outputs.push(output.unwrap_or(Vec::new()));

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

    fn directives(&'t self) -> impl Iterator<Item = Spanned<Directive<'t>>> {
        std::iter::empty()
    }
}

/// A `ParseResult` is an iterator over directives.
///
/// It may be useful to attach an ordering adapter to return them in date order.
pub struct ParseResult<'s, 't> {
    directives: DirectiveIterator<'s, 't>,
}

impl<'s, 't> ParseResult<'s, 't> {
    fn new(directives: DirectiveIterator<'s, 't>) -> Self {
        ParseResult { directives }
    }

    pub fn by_date(self) -> impl Iterator<Item = Spanned<Directive<'t>>> {
        self.directives.sort(|d| d.value().date())
    }
}

impl<'s, 't> Iterator for ParseResult<'s, 't> {
    type Item = Spanned<Directive<'t>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.directives.next()
    }
}

/// Iterator for in-order traversal of `Directive`s.
#[derive(Debug)]
struct DirectiveIterator<'s, 't> {
    included: HashMap<&'s Path, Span>,
    current: VecDeque<Spanned<Declaration<'t>>>,
    stacked: VecDeque<VecDeque<Spanned<Declaration<'t>>>>,
    remaining: VecDeque<VecDeque<Spanned<Declaration<'t>>>>,
    // tags for pragma push/pop
    tags: HashMap<&'t Tag<'t>, Span>,
    meta_key_values: HashMap<&'t Key<'t>, Spanned<MetaValue<'t>>>,
}

impl<'s, 't> DirectiveIterator<'s, 't> {
    fn new(all_declarations: Vec<Vec<Spanned<Declaration<'t>>>>) -> Self {
        let mut remaining = all_declarations
            .into_iter()
            .map(VecDeque::from)
            .collect::<VecDeque<_>>();

        let current = remaining.pop_front().unwrap_or(VecDeque::new());

        DirectiveIterator {
            included: HashMap::new(),
            current,
            stacked: VecDeque::new(),
            remaining,
            tags: HashMap::new(),
            meta_key_values: HashMap::new(),
        }
    }
}

impl<'s, 't> Iterator for DirectiveIterator<'s, 't> {
    type Item = Spanned<Directive<'t>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current.pop_front() {
            Some(declaration) => {
                match declaration.value {
                    Declaration::Directive(directive) => Some(spanned(directive, declaration.span)),

                    Declaration::Pragma(pragma) => {
                        use Pragma::*;

                        match pragma {
                            Pushtag(tag) => {
                                self.tags.insert(tag.value, tag.span);
                            }
                            Poptag(tag) => {
                                self.tags.remove(tag);
                            }
                            Pushmeta(m) => {
                                self.meta_key_values.insert(m.key.value, m.value);
                            }
                            Popmeta(key) => {
                                self.meta_key_values.remove(key);
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
                        }
                        // TODO
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

#[cfg(test)]
pub use lexer::bare_lex;
pub use lexer::dump_tokens;
mod lexer;
mod parsers;
pub mod types;
