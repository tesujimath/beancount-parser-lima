use super::{
    parser::{end_of_input, Span},
    *,
};
use chrono::{NaiveDate, NaiveTime};
use logos::Logos;
use nonempty::NonEmpty;
use rust_decimal::Decimal;
use std::{
    borrow::Cow,
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    str::FromStr,
};

#[derive(Logos, Clone, Debug, PartialEq, Eq)]
#[logos(error = LexerError, skip r"[ \t]+")]
#[logos(subpattern comment_to_eol= r"(;[^\n]*)")] // rolled into end-of-line handling below
#[logos(subpattern currency = r"[A-Z][A-Z0-9'\._-]*|/[A-Z0-9'\._-]+")] // not all matches are valid so we lean on the validation provided by try_from
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

    #[regex(r#"(?&currency)"#, |lex| super::Currency::try_from(lex.slice()) )]
    Currency(super::Currency<'a>),

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
    Date(NaiveDate),

    #[regex(r"(?&time)", |lex| parse_time(lex.slice()))]
    Time(NaiveTime),

    #[regex(r"(?&account_type)(:(?&account_name))+", |lex| parse_account(lex.slice()))]
    Account(super::Account<'a>),

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

    #[regex(r"#(?&tag_or_link_identifier)", |lex| TagOrLinkIdentifier::try_from(&lex.slice()[1..]).map(super::Tag::new))]
    Tag(super::Tag<'a>),

    #[regex(r"\^(?&tag_or_link_identifier)", |lex| TagOrLinkIdentifier::try_from(&lex.slice()[1..]).map(super::Link::new))]
    Link(super::Link<'a>),

    // TODO only in trailing colon context
    #[regex(r"(?&key)", |lex| Key::try_from(lex.slice()))]
    Key(super::Key<'a>),

    // end-of-line and indent, which is the only significant whitespace
    #[regex(r"(?&comment_to_eol)?\n[ \t]+")]
    EolThenIndent,

    #[regex(r"(?&comment_to_eol)?\n")]
    Eol,

    // indent handling is post-processed by lexer, when `EolThenIndent` is broken into separate `Eol` and `Indent`
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

// TODO remove this temporary diagnostic
pub fn dump(s: &str) {
    for (tok, span) in lex(s) {
        match tok {
            Token::Error(e) => println!("{:?} at {:?}", e, span),
            tok => println!("{:?}", tok),
        }
    }
}

/// Lex the input discarding empty lines, and mapping `Range` span into `Span`
/// and forcing a final Eol in case missing.
///
/// Lexing errors are returned as `Error` tokens.
pub fn lex(s: &str) -> Vec<(Token, Span)> {
    lex_with_final_eol(s, Some(end_of_input(s)))
}

/// Lex the input discarding empty lines, and mapping `Range` span into `Span`
#[cfg(test)]
pub fn bare_lex(s: &str) -> Vec<(Token, Span)> {
    lex_with_final_eol(s, None)
}

fn lex_with_final_eol(s: &str, final_eol: Option<Span>) -> Vec<(Token, Span)> {
    Token::lexer(s)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, Span::from(span)),
            Err(e) => (Token::Error(e), Span::from(span)),
        })
        .fold(EmptyLineFolder::new(final_eol), EmptyLineFolder::fold)
        .finalize()
}

struct EmptyLineFolder<'a> {
    forced_final_eol_span: Option<Span>,
    committed: Vec<(Token<'a>, Span)>,
    pending_eol: Option<(Token<'a>, Span)>,
}

impl<'a> EmptyLineFolder<'a> {
    fn new(forced_final_eol_span: Option<Span>) -> Self {
        EmptyLineFolder {
            forced_final_eol_span,
            committed: Vec::new(),
            pending_eol: None,
        }
    }

    fn finalize(mut self) -> Vec<(Token<'a>, Span)> {
        if let Some(pending) = self.pending_eol.take() {
            self.committed.push(pending)
        } else if let Some(eol_span) = self.forced_final_eol_span.take() {
            // force a final newline
            self.committed.push((Token::Eol, eol_span))
        }
        self.committed
    }

    fn fold(mut self, item: (Token<'a>, Span)) -> Self {
        if item.0.is_eol() {
            if let Some((_, span)) = self.pending_eol.take() {
                self.pending_eol = Some((item.0, Span::new(span.start, item.1.end)))
            } else {
                self.pending_eol = Some(item)
            }
        } else {
            if let Some(pending) = self.pending_eol.take() {
                use Token::*;

                // don't push an initial empty line
                if !self.committed.is_empty() {
                    if pending.0 == EolThenIndent {
                        // expand into separate tokens
                        let (start, end) = (pending.1.start, pending.1.end);
                        self.committed.push((Eol, Span::new(start, end - 1)));
                        self.committed.push((Indent, Span::new(end - 1, end)));
                    } else {
                        self.committed.push(pending);
                    }
                }
            }
            self.committed.push(item);
        }

        self
    }
}

fn parse_date(s: &str) -> Result<NaiveDate, LexerError> {
    let mut date = s.split(&['-', '/']);
    let year = date.by_ref().next().unwrap().parse::<i32>().unwrap();
    let month = date.by_ref().next().unwrap().parse::<u32>().unwrap();
    let day = date.by_ref().next().unwrap().parse::<u32>().unwrap();
    NaiveDate::from_ymd_opt(year, month, day).ok_or(LexerError::new("date out of range"))
}

fn parse_time(s: &str) -> Result<NaiveTime, LexerError> {
    let mut time = s.split(':');
    let hour = time.by_ref().next().unwrap().parse::<u32>().unwrap();
    let min = time.by_ref().next().unwrap().parse::<u32>().unwrap();
    let sec = time
        .by_ref()
        .next()
        .map(|s| s.parse::<u32>().unwrap())
        .unwrap_or(0);

    NaiveTime::from_hms_opt(hour, min, sec).ok_or(LexerError::new("time out of range"))
}

fn parse_account(s: &str) -> Result<Account, LexerError> {
    let mut account = s.split(':');
    let account_type = account.by_ref().next().unwrap().parse::<AccountType>()?;
    let account_names = account
        .by_ref()
        .map(AccountName::try_from)
        .collect::<Result<Vec<AccountName>, _>>()?;
    Ok(Account::new(
        account_type,
        NonEmpty::collect(account_names.into_iter()).unwrap(),
    ))
}

fn unescape_string_literal(s: &str) -> Result<Cow<str>, LexerError> {
    if s.contains('\\') {
        // TODO be more careful here to check for malformed backslash escapes - maybe there's a crate for this?
        Ok(Cow::Owned(
            s.replace("\\n", "\n")
                .replace("\\t", "\t")
                .replace("\\\"", "\"")
                .replace("\\\\", "\\"),
        ))
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
