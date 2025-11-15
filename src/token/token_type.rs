/// A validated identifier following C-style naming rules.
/// Must start with a letter or underscore, and contain only alphanumeric characters and underscores.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Identifier(String);

impl Identifier {
    /// Create a new Identifier from a string with validation.
    /// 
    /// # Panics
    /// Panics if the string doesn't follow C-style identifier rules:
    /// - Must start with a letter or underscore
    /// - Must contain only alphanumeric characters and underscores
    pub fn new(s: impl ToString) -> Self {
        Self::try_from(s.to_string()).expect("Invalid identifier")
    }
    
    /// Get the inner string value.
    pub fn as_str(&self) -> &str {
        &self.0
    }
    
    /// Get the inner string value as a string reference.
    pub fn to_str_ref(&self) -> &str {
        &self.0
    }
    
    /// Convert to the inner string.
    pub fn into_string(self) -> String {
        self.0
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::convert::TryFrom<String> for Identifier {
    type Error = String;
    
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err("Identifier cannot be empty".to_string());
        }
        
        let mut chars = value.chars();
        let first_char = chars.next().unwrap();
        
        // First character must be a letter or underscore
        if !first_char.is_ascii_alphabetic() && first_char != '_' {
            return Err(format!(
                "Identifier '{}' must start with a letter or underscore, not '{}'",
                value, first_char
            ));
        }
        
        // Remaining characters must be alphanumeric or underscore
        for ch in chars {
            if !ch.is_ascii_alphanumeric() && ch != '_' {
                return Err(format!(
                    "Identifier '{}' contains invalid character '{}'. Only letters, digits, and underscores are allowed",
                    value, ch
                ));
            }
        }
        
        Ok(Identifier(value))
    }
}

impl std::convert::TryFrom<&str> for Identifier {
    type Error = String;
    
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::try_from(value.to_string())
    }
}

use thiserror::Error;
use num_bigint::BigInt;
use num_rational::BigRational;
// Token types for the Anochi language.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Identifier(Identifier),
    Integer(BigInt),
    Float(BigRational),
    String(String),
    Keyword(Keyword),
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
   Colon,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
   Pipe,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Arrow,
    //Newline,
    Error(TokenizerError),
}
#[derive(Debug,Clone,PartialEq,Error)]
pub enum TokenizerError{
    #[error("Can't convert float from the string")]
    InvalidFloat,
    #[error("Can't convert integer from string")]
    InvalidInt,
    #[error("You are using multiline inside string,To have multiline use \\")]
    StringInNewLine,
    #[error("No closing bracket")]
    NoClosingBracket,
    #[error("No right quote")]
    NoRightQuote,
    #[error("Unknown special character")]
    UnknownSpeicalChar,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Break,
    Continue,
    If,
    Else,
    Loop,
    For,
    Or,
    And,
    Not,
    True,
    False,
    Debug,
    Let
}

impl std::convert::TryFrom<String> for Keyword {
    type Error = ();
    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "loop" => Ok(Keyword::Loop),
            "continue" => Ok(Keyword::Continue),
            "break"=> Ok(Keyword::Break),
            "for" => Ok(Keyword::For),
            "or" => Ok(Keyword::Or),
            "and" => Ok(Keyword::And),
            "not" => Ok(Keyword::Not),
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            "if" => Ok(Keyword::If),
            "else" => Ok(Keyword::Else),
            "debug" => Ok(Keyword::Debug),
            "let" => Ok(Keyword::Let),
            _ => Err(()),
        }
    }
}

impl Keyword {
    pub fn into_str(&self) -> &'static str {
        match self {
            Keyword::Loop => "loop",
            Keyword::For => "for",
            Keyword::Or => "or",
            Keyword::And => "and",
            Keyword::Not => "not",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::Debug => "debug",
            Keyword::Let => "let",
            Keyword::Break=>"break",
            Keyword::Continue=>"continue",
        }
    }
}
