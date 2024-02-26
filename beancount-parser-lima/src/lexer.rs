use super::types::*;
use logos::Logos;
use rust_decimal::Decimal;
use std::{
    borrow::Cow,
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    ops::Range,
    str::FromStr,
};
use time::{Date, Month, Time};
use unescaper::unescape;

// when adjusting any of these regexes, be sure to check whether `RecoveryToken` needs the same
#[derive(Logos, Clone, Debug, PartialEq, Eq)]
#[logos(error = LexerError, skip r"[ \t]+")]
#[logos(subpattern ignored_whole_line= r"([*:!&#?%][^\n]*\n)")] // rolled into end-of-line handling below
#[logos(subpattern comment_to_eol= r"(;[^\n]*)")] // rolled into end-of-line handling below
#[logos(subpattern currency = r"[A-Z][A-Z0-9'\._-]*|/[0-9'\._-]*[A-Z][A-Z0-9'\._-]*")]
#[logos(subpattern date = r"\d{4}[\-/]\d{2}[\-/]\d{2}")]
#[logos(subpattern time = r"\d{1,2}:\d{2}(:\d{2})?")]
#[logos(subpattern account_type = r"[\p{Lu}\p{Lo}][\p{L}\p{N}\-]*")]
#[logos(subpattern account_name = r"[\p{Lu}\p{Lo}\p{N}][\p{L}\p{N}\-]*")]
#[logos(subpattern string_literal = r#""([^\\"]|\\.)*""#)]
#[logos(subpattern number = r"\d+(,\d{3})*(\.\d+)?")]
#[logos(subpattern tag_or_link_identifier = r"[A-Za-z0-9\-_/.]+")]
#[logos(subpattern key = r"[a-z][a-zA-Z0-9\-_]+")]
pub enum Token<'a> {
    #[token("TRUE")]
    True,
    #[token("FALSE")]
    False,
    #[token("NULL")]
    Null,

    #[regex(r#"(?&currency)"#, |lex| lex.slice() )]
    Currency(&'a str),

    #[token("|")]
    Pipe,
    #[token("@@")]
    AtAt,
    #[token("@")]
    At,
    #[token("{{")]
    LcurlCurl,
    #[token("}}")]
    RcurlCurl,
    #[token("{")]
    Lcurl,
    #[token("}")]
    Rcurl,
    #[token(",")]
    Comma,
    #[token("~")]
    Tilde,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token("(")]
    Lparen,
    #[token(")")]
    Rparen,
    #[token("#")]
    Hash,
    #[token("*")]
    Asterisk,
    #[token(":")]
    Colon,

    #[token("!", |_lex| Flag::Exclamation)]
    #[token("&", |_lex| Flag::Ampersand)]
    #[token("?", |_lex| Flag::Question)]
    #[token("%", |_lex| Flag::Percent)]
    #[regex("'[A-Z]", |lex| FlagLetter::try_from(lex.slice().chars().nth(1).unwrap()).map(Flag::Letter))]
    DedicatedFlag(Flag),

    #[token("txn")]
    Txn,
    #[token("balance")]
    Balance,
    #[token("open")]
    Open,
    #[token("close")]
    Close,
    #[token("commodity")]
    Commodity,
    #[token("pad")]
    Pad,
    #[token("event")]
    Event,
    #[token("query")]
    Query,
    #[token("custom")]
    Custom,
    #[token("price")]
    Price,
    #[token("note")]
    Note,
    #[token("document")]
    Document,
    #[token("pushtag")]
    Pushtag,
    #[token("poptag")]
    Poptag,
    #[token("pushmeta")]
    Pushmeta,
    #[token("popmeta")]
    Popmeta,
    #[token("option")]
    Option,
    #[token("options")]
    Options,
    #[token("plugin")]
    Plugin,
    #[token("include")]
    Include,

    #[regex(r"(?&date)", |lex| parse_date(lex.slice()))]
    Date(Date),

    #[regex(r"(?&time)", |lex| parse_time(lex.slice()))]
    Time(Time),

    #[regex(r"(?&account_type)(:(?&account_name))+", |lex| lex.slice())]
    Account(&'a str),

    #[regex(r"(?&string_literal)", |lex| {
        let len = lex.slice().len();
        if len >= 16384 {
            Err(LexerError::new("string too long"))
        } else {
            unescape_string_literal(&lex.slice()[1..len-1])
        }
    })]
    StringLiteral(Cow<'a, str>),

    #[regex(r"(?&number)", |lex| parse_number(lex.slice()))]
    Number(Decimal),

    #[regex(r"#(?&tag_or_link_identifier)", |lex| &lex.slice()[1..])]
    Tag(&'a str),

    #[regex(r"\^(?&tag_or_link_identifier)", |lex| &lex.slice()[1..])]
    Link(&'a str),

    // A key is only supposed to be matched in a trailing colon context, but Logos doesn't support that,
    // so we work around it using parsers::keyword(), which is able to hijack a keyword out of another token.
    #[regex(r"(?&key)", |lex| lex.slice())]
    Key(&'a str),

    // end-of-line and indent, which is the only significant whitespace
    // ignored_whole_line must be anchored immediately after a newline
    #[regex(r"(?&comment_to_eol)?\n(?&ignored_whole_line)*[ \t]+")]
    EolThenIndent,

    #[regex(r"(?&comment_to_eol)?\n(?&ignored_whole_line)*")]
    Eol,

    // indent handling is post-processed by lexer, when `EolThenIndent` is broken into separate `Eol` and `Indent`
    // which fails to capture an indented first line, but that isn't a thing in Beancount anyway
    Indent,

    // errors are returned as an error token
    Error(LexerError),
}

impl<'a> Token<'a> {
    fn is_eol(&self) -> bool {
        use Token::*;

        *self == EolThenIndent || *self == Eol
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Token::*;
        const INDENT: &str = "    ";

        match self {
            True => f.write_str("TRUE"),
            False => f.write_str("FALSE"),
            Null => f.write_str("NULL"),

            Currency(cur) => write!(f, "{}", cur),

            Pipe => f.write_str("|"),
            AtAt => f.write_str("@@"),
            At => f.write_str("@"),
            LcurlCurl => f.write_str("{{"),
            RcurlCurl => f.write_str("}}"),
            Lcurl => f.write_str("{"),
            Rcurl => f.write_str("}"),
            Comma => f.write_str(","),
            Tilde => f.write_str("~"),
            Plus => f.write_str("+"),
            Minus => f.write_str("-"),
            Slash => f.write_str("/"),
            Lparen => f.write_str("("),
            Rparen => f.write_str(")"),
            Hash => f.write_str("#"),
            Asterisk => f.write_str("*"),
            Colon => f.write_str(":"),

            DedicatedFlag(x) => write!(f, "{}", x),

            Txn => f.write_str("txn"),
            Balance => f.write_str("balance"),
            Open => f.write_str("open"),
            Close => f.write_str("close"),
            Commodity => f.write_str("commodity"),
            Pad => f.write_str("pad"),
            Event => f.write_str("event"),
            Query => f.write_str("query"),
            Custom => f.write_str("custom"),
            Price => f.write_str("price"),
            Note => f.write_str("note"),
            Document => f.write_str("document"),
            Pushtag => f.write_str("pushtag"),
            Poptag => f.write_str("poptag"),
            Pushmeta => f.write_str("pushmeta"),
            Popmeta => f.write_str("popmeta"),
            Option => f.write_str("option"),
            Options => f.write_str("options"),
            Plugin => f.write_str("plugin"),
            Include => f.write_str("include"),

            Date(x) => write!(f, "{}", x),
            Time(x) => write!(f, "{}", x),
            Account(x) => write!(f, "{}", x),

            // TODO escapes
            StringLiteral(x) => write!(f, "\"{}\"", x),
            Number(x) => write!(f, "{}", x),
            Tag(x) => write!(f, "{}", x),
            Link(x) => write!(f, "{}", x),
            Key(x) => write!(f, "{}", x),

            EolThenIndent => write!(f, "\\n{}", INDENT),
            Eol => write!(f, "\\n"),
            Indent => write!(f, "{}", INDENT),

            Error(e) => write!(f, "ERROR {}", e),
        }
    }
}

type RangedToken<'a> = (Token<'a>, Range<usize>);
type RangedTokenOrError<'a> = (Result<Token<'a>, LexerError>, Range<usize>);
type SpannedToken<'a> = (Token<'a>, Span);

// Work-around for Logos issue #315.  See `RecoveryAttempter`.
//
// when adjusting any of these regexes, be sure to make the same change in `Token`
#[derive(Logos, Clone, Debug, PartialEq, Eq)]
#[logos(error = LexerError)]
#[logos(subpattern currency = r"[A-Z][A-Z0-9'\._-]*|/[0-9'\._-]*[A-Z][A-Z0-9'\._-]*")]
#[logos(subpattern date = r"\d{4}[\-/]\d{2}[\-/]\d{2}")]
#[logos(subpattern number = r"\d+(,\d{3})*(\.\d+)?")]
enum RecoveryToken {
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token(":")]
    Colon,

    #[regex(r"(?&date)", |lex| parse_date(lex.slice()))]
    Date(Date),

    #[regex(r"(?&number)", |lex| parse_number(lex.slice()))]
    Number(Decimal),
}

impl<'a> From<RecoveryToken> for Token<'a> {
    fn from(value: RecoveryToken) -> Self {
        use RecoveryToken::*;

        match value {
            Minus => Token::Minus,
            Slash => Token::Slash,
            Colon => Token::Colon,
            Date(date) => Token::Date(date),
            Number(decimal) => Token::Number(decimal),
        }
    }
}

/// Lex the input discarding empty lines,
/// and forcing a final `Eol` in case missing.
///
/// Lexing errors are returned as `Error` tokens.
pub fn lex(s: &str) -> impl Iterator<Item = RangedToken> {
    let end_of_input = s.len()..s.len();
    lex_with_final_eol(s, Some(end_of_input))
}

/// Lex the input discarding empty lines.
#[cfg(test)]
pub fn bare_lex(s: &str) -> impl Iterator<Item = RangedToken> {
    lex_with_final_eol(s, None)
}

fn lex_with_final_eol(
    s: &str,
    final_eol: Option<Range<usize>>,
) -> impl Iterator<Item = RangedToken> {
    Token::lexer(s)
        .spanned()
        .attempt_recovery(s)
        .keyword_then_colon_to_key()
        .handle_eol_indent(final_eol)
}

// This is a work-around for Logos issue #315.
// Logos starts matching text like '/1.24' as a currency, and when it fails, it doesn't retry as slash followed by number.
//
// What we do is to attempt to lex the failed span using a subset of the original tokens, namely `RecoveryToken`.
// The set of `RecoveryToken` has been chosen to match where we currently get lexing failures, namely:
// 1. currency failures, which preclude shorter regexes which should match
// 2. colons
//
// It may be necessary to extend `RecoveryToken` as and when further failures in lexing arise.
// The long-term solution is the Logos rewrite, mentioned in that issue.
struct RecoveryAttempter<'a, I> {
    primary_iter: I,
    recovery: Option<(logos::SpannedIter<'a, RecoveryToken>, usize)>,
    source: &'a str,
}

impl<'a, I> RecoveryAttempter<'a, I>
where
    I: Iterator<Item = RangedTokenOrError<'a>>,
{
    fn new(iter: I, source: &'a str) -> Self {
        RecoveryAttempter {
            primary_iter: iter,
            recovery: None,
            source,
        }
    }

    fn create_recovery_lexer(&mut self, failed_span: &Range<usize>) {
        let failed_source = &self.source[failed_span.start..failed_span.end];

        self.recovery = Some((
            RecoveryToken::lexer(failed_source).spanned(),
            failed_span.start,
        ));
    }

    fn next_primary_item(&mut self) -> Option<RangedToken<'a>> {
        match self.primary_iter.next() {
            Some((Ok(t), span)) => Some((t, span)),
            Some((Err(_), span)) => {
                self.create_recovery_lexer(&span);
                self.next_recovery_item()
            }
            None => None,
        }
    }

    fn next_recovery_item(&mut self) -> Option<RangedToken<'a>> {
        fn offset(r: &Range<usize>, o: usize) -> Range<usize> {
            r.start + o..r.end + o
        }

        if let Some((recovery_iter, recovery_offset)) = self.recovery.as_mut() {
            if let Some((recovery_item, span)) = recovery_iter.next() {
                let span = offset(&span, *recovery_offset);
                match recovery_item {
                    Ok(tok) => Some((tok.into(), span)),
                    Err(e) => Some((Token::Error(e), span)),
                }
            } else {
                self.recovery = None;

                self.next_primary_item()
            }
        } else {
            self.next_primary_item()
        }
    }
}

impl<'a, I> Iterator for RecoveryAttempter<'a, I>
where
    I: Iterator<Item = RangedTokenOrError<'a>>,
{
    type Item = RangedToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_recovery_item()
    }
}

trait RecoveryAttempterIteratorAdaptor<'a>: Iterator<Item = RangedTokenOrError<'a>> + Sized {
    fn attempt_recovery(self, source: &'a str) -> RecoveryAttempter<'a, Self> {
        RecoveryAttempter::new(self, source)
    }
}

impl<'a, I: Iterator<Item = RangedTokenOrError<'a>>> RecoveryAttempterIteratorAdaptor<'a> for I {}

struct KeywordThenColonToKey<'a, I> {
    iter: I,
    pending_tok: Option<(RangedToken<'a>, Option<&'static str>)>,
}

impl<'a, I> KeywordThenColonToKey<'a, I> {
    fn new(iter: I) -> Self {
        KeywordThenColonToKey {
            iter,
            pending_tok: None,
        }
    }

    // for a token which is potentially a key return that key string
    fn potential_key(tok: &Token) -> Option<&'static str> {
        match tok {
            Token::Txn => Some("txn"),
            Token::Balance => Some("balance"),
            Token::Open => Some("open"),
            Token::Close => Some("close"),
            Token::Commodity => Some("commodity"),
            Token::Pad => Some("pad"),
            Token::Event => Some("event"),
            Token::Query => Some("query"),
            Token::Custom => Some("custom"),
            Token::Price => Some("price"),
            Token::Note => Some("note"),
            Token::Document => Some("document"),
            Token::Pushtag => Some("pushtag"),
            Token::Poptag => Some("poptag"),
            Token::Pushmeta => Some("pushmeta"),
            Token::Popmeta => Some("popmeta"),
            Token::Option => Some("option"),
            Token::Options => Some("options"),
            Token::Plugin => Some("plugin"),
            Token::Include => Some("include"),
            _ => None,
        }
    }
}

impl<'a, I> Iterator for KeywordThenColonToKey<'a, I>
where
    I: Iterator<Item = RangedToken<'a>>,
{
    type Item = RangedToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pending_tok.is_none() {
            if let Some(tok) = self.iter.next() {
                let potential_key = Self::potential_key(&tok.0);
                self.pending_tok = Some((tok, potential_key));
            }
        }

        match self.pending_tok.take() {
            Some((pending_tok, Some(potential_key))) => match self.iter.next() {
                None => Some(pending_tok),
                Some(colon @ (Token::Colon, _)) => {
                    self.pending_tok = Some((colon, None));
                    Some((Token::Key(potential_key), pending_tok.1))
                }
                Some(other_tok) => {
                    let potential_key = Self::potential_key(&other_tok.0);
                    self.pending_tok = Some((other_tok, potential_key));
                    Some(pending_tok)
                }
            },
            Some((pending_tok, None)) => Some(pending_tok),
            None => None,
        }
    }
}

trait KeywordThenColonToKeyIteratorAdaptor<'a>: Iterator<Item = RangedToken<'a>> + Sized {
    /// Iterator adapter for mapping keyword-then-colon to key.
    fn keyword_then_colon_to_key(self) -> KeywordThenColonToKey<'a, Self> {
        KeywordThenColonToKey::new(self)
    }
}

impl<'a, I: Iterator<Item = RangedToken<'a>>> KeywordThenColonToKeyIteratorAdaptor<'a> for I {}

/// An iterator adapter which does all of the following:
/// 1. Expand EolThenIndent into separate Eol and Indent tokens
/// 2. Fold consecutive Eol tokens into a single one
/// 3. Drop any initial Eol
/// 4. Optionally, force a final Eol
struct EolIndentHandler<'a, I> {
    iter: I,
    first_tok: Option<RangedToken<'a>>,
    pending: [Option<RangedToken<'a>>; 2],
    final_eol: Option<Range<usize>>,
    previous_was_eol: bool,
}

impl<'a, I> EolIndentHandler<'a, I>
where
    I: Iterator<Item = RangedToken<'a>>,
{
    fn new(mut iter: I, final_eol: Option<Range<usize>>) -> Self {
        let mut first_tok = Some((Token::Eol, Range::default()));
        while first_tok.as_ref().is_some_and(|tok| tok.0.is_eol()) {
            first_tok = iter.next()
        }

        EolIndentHandler {
            iter,
            first_tok,
            pending: [None, None],
            final_eol,
            previous_was_eol: false,
        }
    }

    fn merge(&mut self) -> Option<RangedToken<'a>> {
        let mut first_tok = self.iter.next();
        // only get a second token if the first is some kind of EOL, because only then is there anything to merge
        let mut second_tok = if first_tok.as_ref().is_some_and(|tok| tok.0.is_eol()) {
            self.iter.next()
        } else {
            None
        };

        let mut merging = true;
        while merging {
            match (&mut first_tok, &mut second_tok) {
                (Some(first), Some(second)) if first.0.is_eol() && second.0.is_eol() => {
                    let (tok, span) = second_tok.take().unwrap();
                    first_tok = Some((tok, first.1.start..span.end));
                    second_tok = self.iter.next();
                }
                _ => {
                    merging = false;
                }
            }
        }

        match first_tok {
            Some((Token::EolThenIndent, span)) => {
                self.pending = [Some((Token::Indent, span.clone())), second_tok];
                Some((Token::Eol, span))
            }
            Some(tok) => {
                self.pending = [second_tok, None];
                Some(tok)
            }
            None => None,
        }
    }

    fn next_merged(&mut self) -> Option<RangedToken<'a>> {
        match [self.pending[0].take(), self.pending[1].take()] {
            [Some(first), second] => {
                self.pending = [second, None];
                Some(first)
            }
            _ => self.merge(),
        }
    }
}

impl<'a, I> Iterator for EolIndentHandler<'a, I>
where
    I: Iterator<Item = RangedToken<'a>>,
{
    type Item = RangedToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = if self.first_tok.is_some() {
            self.first_tok.take()
        } else {
            self.next_merged()
        };

        match (next, &self.final_eol) {
            (None, Some(span)) => {
                if !self.previous_was_eol {
                    self.previous_was_eol = true;
                    Some((Token::Eol, span.clone()))
                } else {
                    None
                }
            }
            (None, None) => None,
            (Some(item), _) => {
                if item.0 == Token::Eol {
                    self.previous_was_eol = true;
                }
                Some(item)
            }
        }
    }
}

trait EolIndentHandlerIteratorAdaptor<'a>: Iterator<Item = RangedToken<'a>> + Sized {
    /// Iterator adapter for mapping keyword-then-colon to key.
    fn handle_eol_indent(self, final_eol: Option<Range<usize>>) -> EolIndentHandler<'a, Self> {
        EolIndentHandler::new(self, final_eol)
    }
}

impl<'a, I: Iterator<Item = RangedToken<'a>>> EolIndentHandlerIteratorAdaptor<'a> for I {}

fn parse_date(s: &str) -> Result<Date, LexerError> {
    let mut date = s.split(&['-', '/']);
    let year = date.by_ref().next().unwrap().parse::<i32>().unwrap();
    let month = date.by_ref().next().unwrap().parse::<u8>().unwrap();
    let month = Month::try_from(month).or(Err(LexerError::new("month out of range")))?;
    let day = date.by_ref().next().unwrap().parse::<u8>().unwrap();
    Date::from_calendar_date(year, month, day).or(Err(LexerError::new("date out of range")))
}

fn parse_time(s: &str) -> Result<Time, LexerError> {
    let mut time = s.split(':');
    let hour = time.by_ref().next().unwrap().parse::<u8>().unwrap();
    let min = time.by_ref().next().unwrap().parse::<u8>().unwrap();
    let sec = time
        .by_ref()
        .next()
        .map(|s| s.parse::<u8>().unwrap())
        .unwrap_or(0);

    Time::from_hms(hour, min, sec).or(Err(LexerError::new("time out of range")))
}

// Unescape string literal using the inverse of std::ascii::escape_default
// https://doc.rust-lang.org/std/ascii/fn.escape_default.html
fn unescape_string_literal(s: &str) -> Result<Cow<str>, LexerError> {
    if s.contains('\\') {
        unescape(s)
            .map(Cow::Owned)
            .map_err(|e| LexerError::new(e.to_string()))
    } else {
        Ok(Cow::Borrowed(s))
    }
}

fn parse_number(s: &str) -> Result<Decimal, LexerError> {
    let result = if s.contains(',') {
        let mut without_commas = s.to_string();
        without_commas.retain(|c| c != ',');
        FromStr::from_str(&without_commas)
    } else {
        FromStr::from_str(s)
    };

    result.map_err(|e: <rust_decimal::Decimal as std::str::FromStr>::Err| {
        LexerError::new(e.to_string())
    })
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct LexerError {
    message: String,
}

impl LexerError {
    fn new<T: Into<String>>(s: T) -> Self {
        Self { message: s.into() }
    }
}

impl Default for LexerError {
    fn default() -> Self {
        Self::new("unrecognized token")
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for LexerError {}

impl From<CurrencyError> for LexerError {
    fn from(e: CurrencyError) -> Self {
        Self::new(e.to_string())
    }
}

impl From<FlagLetterError> for LexerError {
    fn from(e: FlagLetterError) -> Self {
        Self::new(e.to_string())
    }
}

impl From<AccountNameError> for LexerError {
    fn from(e: AccountNameError) -> Self {
        Self::new(e.to_string())
    }
}

impl From<AccountTypeNameError> for LexerError {
    fn from(e: AccountTypeNameError) -> Self {
        Self::new(e.to_string())
    }
}

impl From<TagOrLinkIdentifierError> for LexerError {
    fn from(e: TagOrLinkIdentifierError) -> Self {
        Self::new(e.to_string())
    }
}

impl From<KeyError> for LexerError {
    fn from(e: KeyError) -> Self {
        Self::new(e.to_string())
    }
}

impl From<strum::ParseError> for LexerError {
    fn from(e: strum::ParseError) -> Self {
        Self::new(e.to_string())
    }
}

mod tests;
