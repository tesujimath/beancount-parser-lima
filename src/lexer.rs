use std::borrow::Cow;

use super::*;
use chrono::{NaiveDate, NaiveTime};
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexerError, skip r"[ \t\n]+")] // TODO don't skip on newline, need Eol and Indent tokens
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

    #[regex(r"#(?&tag_or_link_identifier)", |lex| TagOrLinkIdentifier::try_from(&lex.slice()[1..]).map(super::Tag))]
    Tag(super::Tag<'a>),

    #[regex(r"\^(?&tag_or_link_identifier)", |lex| TagOrLinkIdentifier::try_from(&lex.slice()[1..]).map(super::Link))]
    Link(super::Link<'a>),

     // TODO only in trailing colon context
    #[regex(r"(?&key)", |lex| Key::try_from(lex.slice()))]
    Key(super::Key<'a>),

    #[regex(r".", priority = 0, callback = |lex| println!("unrecognized token '{}' at {:?}", lex.slice(), lex.span()))]
    Unrecognized,
}

// TODO remove this temporary diagnostic
pub fn dump(s: &str) {
    for tok in Token::lexer(s) {
        match tok {
            Ok(tok) => println!("{:?} ", tok),
            Err(e) => println!("failed{:?}", e),
        }
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
            s
                .replace("\\n", "\n")
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

    result.map_err(|e: <rust_decimal::Decimal as std::str::FromStr>::Err| LexerError::new(e.to_string()))
}

#[derive(PartialEq, Clone, Debug)]
pub struct LexerError {
    message: String,
}

impl LexerError {
    fn new<T: Into<String>>(s: T) -> Self {
        LexerError { message: s.into() }
    }
}

impl Default for LexerError {
    fn default() -> Self {
        LexerError::new("unspecified lexer failure")
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for LexerError {}

impl From<CurrencyError> for LexerError {
    fn from(e: CurrencyError) -> LexerError {
        LexerError::new(e.to_string())
    }
}

impl From<FlagLetterError> for LexerError {
    fn from(e: FlagLetterError) -> LexerError {
        LexerError::new(e.to_string())
    }
}

impl From<AccountNameError> for LexerError {
    fn from(e: AccountNameError) -> LexerError {
        LexerError::new(e.to_string())
    }
}

impl From<TagOrLinkIdentifierError> for LexerError {
    fn from(e: TagOrLinkIdentifierError) -> LexerError {
        LexerError::new(e.to_string())
    }
}

impl From<KeyError> for LexerError {
    fn from(e: KeyError) -> LexerError {
        LexerError::new(e.to_string())
    }
}

impl From<strum::ParseError> for LexerError {
    fn from(e: strum::ParseError) -> LexerError {
        LexerError::new(e.to_string())
    }
}
