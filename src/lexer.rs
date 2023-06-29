use super::*;
use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexerError, skip r"[ \t]+")] // Ignore this regex pattern between tokens
pub enum Token<'a> {
    #[token("TRUE")]
    True,
    #[token("FALSE")]
    False,
    #[token("NULL")]
    Null,

    // not all matches are valid so we lean on the validation provided by try_from
    #[regex(r#"[A-Z][A-Z0-9'\._-]*|/[A-Z0-9'\._-]+"#, |lex| super::Currency::try_from(lex.slice()) )]
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

    #[regex(r"\d{4}[\-/]\d{2}[\-/]\d{2}", |lex| parse_date(lex.slice()))]
    Date(NaiveDate),

    #[regex(r"\d{4}[\-/]\d{2}[\-/]\d{2}[ \t]+\d{1,2}:\d{2}(:\d{2})?", |lex| parse_datetime(lex.slice()))]
    DateTime(NaiveDateTime),
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

fn parse_datetime(s: &str) -> Result<NaiveDateTime, LexerError> {
    let mut datetime = s.split_ascii_whitespace();
    let date = parse_date(datetime.by_ref().next().unwrap())?;
    let mut time = datetime.by_ref().next().unwrap().split(':');
    let hour = time.by_ref().next().unwrap().parse::<u32>().unwrap();
    let min = time.by_ref().next().unwrap().parse::<u32>().unwrap();
    let sec = time
        .by_ref()
        .next()
        .map(|s| s.parse::<u32>().unwrap())
        .unwrap_or(0);
    let time =
        NaiveTime::from_hms_opt(hour, min, sec).ok_or(LexerError::new("time out of range"))?;

    Ok(NaiveDateTime::new(date, time))
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
        LexerError {
            message: e.to_string(),
        }
    }
}

impl From<FlagLetterError> for LexerError {
    fn from(e: FlagLetterError) -> LexerError {
        LexerError {
            message: e.to_string(),
        }
    }
}
