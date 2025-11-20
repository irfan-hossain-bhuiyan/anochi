/// A validated identifier following C-style naming rules.
/// Must start with a letter or underscore, and contain only alphanumeric characters and underscores.
use derive_more::{Display, Deref, Into};

/// A validated identifier following C-style naming rules.
/// Must start with a letter or underscore, and contain only alphanumeric characters and underscores.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Display, Deref, Into)]
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
use enum_as_inner::EnumAsInner;

// Token types for the Anochi language.
#[derive(Debug, Clone, PartialEq, EnumAsInner)]
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

use strum::{EnumString, Display as StrumDisplay, IntoStaticStr};

#[derive(Debug, Clone, PartialEq, EnumString, StrumDisplay, IntoStaticStr)]
#[strum(serialize_all = "lowercase")]
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
    Let,
    Fn
}
