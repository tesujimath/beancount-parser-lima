use super::*;
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
