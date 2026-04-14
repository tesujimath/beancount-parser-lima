use ariadne::{Color, Label};
use glob::{self, glob_with};
use lazy_format::lazy_format;
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

use crate::{IncludedGlob, SourceId, get_includes};

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
    included_globs: HashMap<PathBuf, IncludedGlob>,
    included_content: HashMap<PathBuf, IncludedSource>,
    source_id_strings: Vec<String>, // indexed by SourceId
}

#[derive(Clone, Debug)]
enum IncludedSource {
    Content(SourceId, String, Vec<usize>), // the content and its char indices
    Error(String),
    Duplicate,
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

        let mut pending_includes = get_includes(&root_content, root_source_id)
            .into_iter()
            .map(|included| resolve_included_path(root_path.as_ref(), included.as_ref()))
            .collect::<VecDeque<_>>();

        let mut included_globs = HashMap::new();
        let mut included_content: HashMap<PathBuf, IncludedSource> = HashMap::new();

        // for duplicate detection
        let mut canonical_paths =
            if let Some(canonical_root) = root_path.as_ref().and_then(|p| p.canonicalize().ok()) {
                HashSet::from([canonical_root])
            } else {
                HashSet::default()
            };

        while !pending_includes.is_empty() {
            let included = pending_includes.pop_front().unwrap();
            let included_str = included.to_string_lossy();
            let included_str = included_str.as_ref();

            match glob_with(
                included_str,
                glob::MatchOptions {
                    case_sensitive: true,
                    require_literal_separator: true,
                    require_literal_leading_dot: true,
                },
            ) {
                Err(e) => {
                    included_globs.insert(included, IncludedGlob::Error(e.to_string()));
                }
                Ok(globbed_includes) => {
                    let mut glob_expansions = Vec::default();

                    for globbed_include in globbed_includes {
                        match globbed_include {
                            Err(e) => {
                                let path = e.path().to_path_buf();
                                glob_expansions.push(path.clone());
                                included_content.insert(path, IncludedSource::Error(e.to_string()));
                            }
                            Ok(globbed_include) => {
                                glob_expansions.push(globbed_include.clone());

                                if let Ok(canonical_path) = globbed_include.canonicalize() {
                                    if canonical_paths.contains(&canonical_path) {
                                        // don't overwrite existing content
                                        included_content
                                            .entry(globbed_include)
                                            .or_insert(IncludedSource::Duplicate);
                                    } else {
                                        canonical_paths.insert(canonical_path);

                                        let source_id = SourceId::from(source_id_strings.len());
                                        source_id_strings
                                            .push(globbed_include.to_string_lossy().into());

                                        let included_source = read(&globbed_include).map_or_else(
                                            |e| {
                                                IncludedSource::Error(format!(
                                                    "{}: {}",
                                                    globbed_include.to_string_lossy(),
                                                    e
                                                ))
                                            },
                                            |c| {
                                                // find the char indices for the content
                                                // needed for mapping byte indices to char indices, to convert Chumsky spans to Ariadne spans
                                                // see https://github.com/zesterer/chumsky/issues/65#issuecomment-1689216633
                                                let char_indices = c
                                                    .char_indices()
                                                    .map(|(i, _)| i)
                                                    .collect::<Vec<_>>();
                                                IncludedSource::Content(source_id, c, char_indices)
                                            },
                                        );

                                        // stabilisation of VacantEntry::insert_entry() would enable us to avoid cloning the path here
                                        // and doing an immediate lookup
                                        included_content
                                            .insert(globbed_include.clone(), included_source);
                                        let included_source =
                                            included_content.get(&globbed_include).unwrap();

                                        if let IncludedSource::Content(_, content, _) =
                                            included_source
                                        {
                                            let mut includes = get_includes(content, source_id)
                                                .into_iter()
                                                .map(|included_path| {
                                                    resolve_included_path(
                                                        Some(&globbed_include),
                                                        included_path.as_ref(),
                                                    )
                                                })
                                                .collect::<VecDeque<_>>();
                                            pending_includes.append(&mut includes);
                                        }
                                    }
                                }
                            }
                        }
                    }

                    included_globs.insert(included, IncludedGlob::Expanded(glob_expansions));
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
            included_globs,
            included_content,
            source_id_strings,
        }
    }

    #[deprecated(since = "0.12.0", note = "Use `write_errors_or_warnings` instead")]
    pub fn write<W, E, K>(&self, w: &mut W, errors_or_warnings: Vec<E>) -> io::Result<()>
    where
        W: Write,
        E: Into<AnnotatedErrorOrWarning<K>>,
        K: ErrorOrWarningKind,
    {
        self.write_errors_or_warnings(w, errors_or_warnings)
    }

    /// Write human-readable error reports.
    pub fn write_errors_or_warnings<W, E, K>(
        &self,
        w: &mut W,
        errors_or_warnings: Vec<E>,
    ) -> io::Result<()>
    where
        W: Write,
        E: Into<AnnotatedErrorOrWarning<K>>,
        K: ErrorOrWarningKind,
    {
        for error_or_warning in errors_or_warnings.into_iter() {
            let AnnotatedErrorOrWarning {
                error_or_warning,
                annotation,
            } = error_or_warning.into();

            self.write_report::<W, K, ErrorOrWarning<K>>(w, &error_or_warning)?;

            if let Some(annotation) = annotation {
                // clippy thinks this is better than write! 🤷
                w.write_fmt(core::format_args!("{}\n", &annotation))?;
            }
        }
        Ok(())
    }

    /// Write human-readable error or warning report.
    pub fn write_report<W, K, R>(&self, w: &mut W, report: &R) -> io::Result<()>
    where
        W: Write,
        K: ErrorOrWarningKind,
        R: Report,
    {
        write_report::<W, K, R, _>(
            w,
            report,
            &|span| self.get_adjusted_source(span),
            self.sources(),
        )
    }

    /// Resolve the span into filename, line number range, and spanned content.
    /// Filename will be present unless the sources were created from an inline string.
    pub fn resolve_span<'a>(&'a self, span: &Span) -> SpannedSource<'a> {
        let (source_content, source_id_str, byte_span, rune_span) = self.get_adjusted_source(*span);

        let file_name = if Into::<SourceId>::into(span.source) == self.root_source_id {
            self.root_path.as_ref().and(Some(source_id_str))
        } else {
            Some(source_id_str)
        };

        let mut source_chars = source_content.chars();
        let start_line = source_chars
            .by_ref()
            .take(rune_span.start)
            .filter(|c| *c == '\n')
            .count()
            + 1;
        let lines_spanned = source_chars
            .by_ref()
            .take(rune_span.end - rune_span.start)
            .filter(|c| *c == '\n')
            .count();
        let end_line = start_line + lines_spanned;

        SpannedSource {
            file_name,
            start_line,
            end_line,
            content: source_content
                .get(byte_span.start..byte_span.end)
                .unwrap_or(""),
        }
    }

    fn byte_to_rune(&self, char_indices: &[usize], byte_span: Span) -> Span {
        let mut rune_span = byte_span;
        rune_span.start = char_indices.partition_point(|&i| i < byte_span.start);
        rune_span.end = char_indices.partition_point(|&i| i < byte_span.end);
        rune_span
    }

    pub fn error_source_text<'a, K>(&'a self, error_or_warning: &ErrorOrWarning<K>) -> &'a str
    where
        K: ErrorOrWarningKind,
    {
        let (source_content, _, byte_span, _rune_span) =
            self.get_adjusted_source(error_or_warning.0.span);
        &source_content[byte_span.start..byte_span.end]
    }

    fn get_adjusted_source(&self, span: Span) -> (&str, &str, Span, Span) {
        let safe_span = if span.source >= self.source_id_strings.len() {
            // bad source collapses down to empty span,
            // because we don't really have a good way to reject that
            // and at least we mustn't panic
            Span {
                source: self.root_source_id.into(),
                start: 0,
                end: 0,
            }
        } else {
            span
        };
        let source_id = safe_span.source.into();
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

        let byte_span = trimmed_span(source_content, safe_span);
        let rune_span = byte_to_rune(source_content_char_indices, byte_span);

        (source_content, source_id_str, byte_span, rune_span)
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

    pub(crate) fn content_iter(&self) -> impl Iterator<Item = (SourceId, Option<&Path>, &str)> {
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

    /// Number of real sources, excluding synthetic ones
    pub(crate) fn num_sources(&self) -> usize {
        self.source_id_strings.len()
    }

    pub(crate) fn root_path(&self) -> Option<&Path> {
        self.root_path.as_deref()
    }

    pub(crate) fn included_globs(&self) -> &HashMap<PathBuf, IncludedGlob> {
        &self.included_globs
    }

    pub(crate) fn error_paths(&self) -> HashMap<Option<&Path>, String> {
        self.included_content
            .iter()
            .filter_map(|(pathbuf, included_source)| {
                if let IncludedSource::Error(e) = included_source {
                    Some((Some(pathbuf.as_path()), e.clone()))
                } else {
                    None
                }
            })
            .collect::<HashMap<_, _>>()
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
                IncludedSource::Error(e) => writeln!(f, "    {:?} err {},", path, e)?,
                IncludedSource::Duplicate => writeln!(f, "    {:?} duplicate include", path)?,
            }
        }

        writeln!(f, ")",)
    }
}

#[derive(Clone)]
pub struct SyntheticSources<'a> {
    sources: &'a BeancountSources,
    base_id: usize,
    content: HashMap<String, (SourceId, String, Vec<usize>)>, // content and char indices, indexed by source name
    source_id_strings: Vec<String>,                           // indexed by SourceId - base_id
}

impl<'a> SyntheticSources<'a> {
    pub fn new(sources: &'a BeancountSources) -> Self {
        SyntheticSources {
            sources,
            base_id: sources.num_sources(),
            content: HashMap::default(),
            source_id_strings: Vec::default(),
        }
    }
}

impl<'a> SyntheticSources<'a> {
    fn sources(&self) -> Vec<(String, &str)> {
        let mut sources = self.sources.sources();
        sources.extend(
            self.content.iter().map(|(source_id_str, (_, content, _))| {
                (source_id_str.to_string(), content.as_str())
            }),
        );
        sources
    }

    /// A synthetic span is a content fragment which doesn't occur in the original sources, but
    /// may be referred to in error reports.  Multiple fragments may share the same source name.
    pub fn create_synthetic_span(&mut self, source_name: &str, content_fragment: &str) -> Span {
        if let Some((source_id, content, char_indices)) = self.content.get_mut(source_name) {
            let start = content.len();
            let end = start + content_fragment.len();
            let span = Span {
                source: (*source_id).into(),
                start,
                end,
            };

            let original_len = char_indices.len();
            char_indices.extend(
                content_fragment
                    .char_indices()
                    .map(|(i, _)| i + original_len),
            );
            content.push_str(content_fragment);

            span
        } else {
            let source = self.source_id_strings.len() + self.base_id;
            self.source_id_strings.push(source_name.to_string());

            let span = Span {
                source,
                start: 0,
                end: content_fragment.len(),
            };

            let content = content_fragment.to_string();
            let char_indices = content.char_indices().map(|(i, _)| i).collect::<Vec<_>>();
            self.content.insert(
                source_name.to_string(),
                (source.into(), content, char_indices),
            );

            span
        }
    }

    /// Write human-readable error reports.
    pub fn write_errors_or_warnings<W, E, K>(
        &self,
        w: &mut W,
        errors_or_warnings: Vec<E>,
    ) -> io::Result<()>
    where
        W: Write,
        E: Into<AnnotatedErrorOrWarning<K>>,
        K: ErrorOrWarningKind,
    {
        for error_or_warning in errors_or_warnings.into_iter() {
            let AnnotatedErrorOrWarning {
                error_or_warning,
                annotation,
            } = error_or_warning.into();

            self.write_report::<W, K, ErrorOrWarning<K>>(w, &error_or_warning)?;

            if let Some(annotation) = annotation {
                // clippy thinks this is better than write! 🤷
                w.write_fmt(core::format_args!("{}\n", &annotation))?;
            }
        }
        Ok(())
    }

    /// Write human-readable error or warning report.
    pub fn write_report<W, K, R>(&self, w: &mut W, report: &R) -> io::Result<()>
    where
        W: Write,
        K: ErrorOrWarningKind,
        R: Report,
    {
        write_report::<W, K, R, _>(
            w,
            report,
            &|span| self.get_adjusted_source(span),
            self.sources(),
        )
    }

    /// Resolve the span into filename, line number range, and spanned content.
    /// Filename will be present unless the sources were created from an inline string.
    pub fn resolve_span<'s>(&'s self, span: &Span) -> SpannedSource<'s> {
        resolve_span(*span, &|span| self.get_adjusted_source(span), true)
    }

    pub fn error_source_text<'s, K>(&'s self, error_or_warning: &ErrorOrWarning<K>) -> &'s str
    where
        K: ErrorOrWarningKind,
    {
        let (source_content, _, byte_span, _rune_span) =
            self.get_adjusted_source(error_or_warning.0.span);
        &source_content[byte_span.start..byte_span.end]
    }

    fn get_adjusted_source(&self, span: Span) -> (&str, &str, Span, Span) {
        if span.source >= self.base_id && span.source < self.base_id + self.source_id_strings.len()
        {
            let source_id_str = self.source_id_strings[span.source - self.base_id].as_str();

            let (_, content, content_char_indices) = self.content.get(source_id_str).unwrap();
            let content = content.as_str();

            let byte_span = trimmed_span(content, span);
            let rune_span = byte_to_rune(content_char_indices, byte_span);

            (content, source_id_str, byte_span, rune_span)
        } else {
            self.sources.get_adjusted_source(span)
        }
    }
}

// get included path relative to including path
pub(crate) fn resolve_included_path(
    including_path: Option<&PathBuf>,
    included_path: &Path,
) -> PathBuf {
    match including_path.and_then(|p| path_dir(p.as_ref())) {
        Some(p) => p.join(included_path),
        None => included_path.to_path_buf(),
    }
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

fn write_report<'a, W, K, R, F>(
    w: &mut W,
    report: &R,
    get_adjusted_source: &F,
    sources: Vec<(String, &str)>,
) -> io::Result<()>
where
    W: Write,
    K: ErrorOrWarningKind,
    R: Report,
    F: Fn(Span) -> (&'a str, &'a str, Span, Span),
{
    let (src_id, span) =
        source_id_string_and_adjusted_rune_span(report.span(), get_adjusted_source);
    let color = K::color();
    let report_kind = K::report_kind();

    ariadne::Report::build(report_kind, (src_id.clone(), (span.start..span.end)))
        .with_message(report.message())
        .with_labels(Some(
            Label::new((src_id, (span.start..span.end)))
                .with_message(report.reason())
                .with_color(color),
        ))
        .with_labels(report.contexts().map(|(label, span)| {
            let (src_id, span) = source_id_string_and_adjusted_rune_span(span, get_adjusted_source);
            Label::new((src_id, (span.start..span.end)))
                .with_message(lazy_format!("in this {}", label))
                .with_color(Color::Yellow)
        }))
        .with_labels(report.related().map(|(label, span)| {
            let (src_id, span) = source_id_string_and_adjusted_rune_span(span, get_adjusted_source);
            Label::new((src_id, (span.start..span.end)))
                .with_message(lazy_format!("{}", label))
                .with_color(Color::Yellow)
        }))
        .finish()
        .write(ariadne::sources(sources), w)
}

/// Resolve the span into filename, line number range, and spanned content.
/// Filename will be present unless the sources were created from an inline string.
fn resolve_span<'a, F>(
    span: Span,
    get_adjusted_source: &F,
    source_id_is_file_name: bool,
) -> SpannedSource<'a>
where
    F: Fn(Span) -> (&'a str, &'a str, Span, Span),
{
    let (source_content, source_id_str, byte_span, rune_span) = get_adjusted_source(span);

    let mut source_chars = source_content.chars();
    let start_line = source_chars
        .by_ref()
        .take(rune_span.start)
        .filter(|c| *c == '\n')
        .count()
        + 1;
    let lines_spanned = source_chars
        .by_ref()
        .take(rune_span.end - rune_span.start)
        .filter(|c| *c == '\n')
        .count();
    let end_line = start_line + lines_spanned;

    SpannedSource {
        file_name: source_id_is_file_name.then_some(source_id_str),
        start_line,
        end_line,
        content: source_content
            .get(byte_span.start..byte_span.end)
            .unwrap_or(""),
    }
}

fn source_id_string_and_adjusted_rune_span<'a, F>(
    span: Span,
    get_adjusted_source: &F,
) -> (String, Span)
where
    F: Fn(Span) -> (&'a str, &'a str, Span, Span),
{
    let (_, source_id, _byte_span, rune_span) = get_adjusted_source(span);
    (source_id.to_string(), rune_span)
}

fn trimmed_span(source: &str, span: Span) -> Span {
    let mut trimmed = span;

    // invalid spans fall back to nothing
    if source.get(span.start..span.end).is_none() {
        trimmed.start = 0;
        trimmed.end = 0;
    } else {
        trimmed.end = trim_trailing_whitespace(source, span.start, span.end);
    }
    trimmed
}

fn byte_to_rune(char_indices: &[usize], byte_span: Span) -> Span {
    let mut rune_span = byte_span;
    rune_span.start = char_indices.partition_point(|&i| i < byte_span.start);
    rune_span.end = char_indices.partition_point(|&i| i < byte_span.end);
    rune_span
}
