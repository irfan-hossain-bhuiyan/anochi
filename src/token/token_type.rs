// Token types for the Anochi language.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Keyword(Keyword),
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Newline,
    Eof,
    Error(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    While,
    For,
    Or,
    And,
    Not,
    True,
    False,
}

impl std::convert::TryFrom<String> for Keyword {
    type Error = ();
    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "while" => Ok(Keyword::While),
            "for" => Ok(Keyword::For),
            "or" => Ok(Keyword::Or),
            "and" => Ok(Keyword::And),
            "not" => Ok(Keyword::Not),
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            _ => Err(()),
        }
    }
}

impl Keyword {
    pub fn into_str(&self) -> &'static str {
        match self {
            Keyword::While => "while",
            Keyword::For => "for",
            Keyword::Or => "or",
            Keyword::And => "and",
            Keyword::Not => "not",
            Keyword::True => "true",
            Keyword::False => "false",
        }
    }
}
