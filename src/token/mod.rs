//! Token module for the Anochi programming language lexer.
//! Token module for the Anochi programming language lexer.
pub mod token_type;
#[cfg(test)]
mod tests;
pub use token_type::{TokenType, Identifier};


use std::num::NonZeroUsize;
use std::ops::Deref;
use std::fmt;

use crate::token::token_type::{Keyword, TokenizerError};

#[derive(Debug, Clone, PartialEq)]
pub enum CharType {
    Alpha,
    Numeric,
    Special,
    Whitespace,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Position<'a> {
    pub line: NonZeroUsize,
    pub column: NonZeroUsize,
    pub slice: &'a str,
}

impl<'a> Position<'a> {
    pub fn new(line: usize, column: usize, slice: &'a str) -> Option<Self> {
        Some(Self {
            line: NonZeroUsize::new(line)?,
            column: NonZeroUsize::new(column)?,
            slice,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub position: Position<'a>,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, position: Position<'a>) -> Self {
        Self {
            token_type,
            position,
        }
    }
}

pub struct Tokenizer<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    current: usize,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            current: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(mut self) -> TokenContainer<'a> {
        while self.peek().is_some() {
            match self.char_type() {
                Some(CharType::Alpha) => {
                    let token = self.parse_identifier_or_keyword();
                    self.tokens.push(token);
                }
                Some(CharType::Numeric) => {
                    let token = self.parse_numeric();
                    self.tokens.push(token);
                }
                Some(CharType::Whitespace) => {
                    self.skip_whitespace();
                }
                Some(CharType::Special) => {
                    if self.peek() == Some('"') {
                        let token = self.parse_string();
                        self.tokens.push(token);
                    } else {
                        let token = self.parse_special_char();
                        self.tokens.push(token);
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }
        TokenContainer::new(self.tokens)
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    fn peek_next(&self) -> Option<char> {
        self.source.chars().nth(self.current + 1)
    }

    fn advance(&mut self) -> Option<char> {
        let current = self.source.chars().nth(self.current);
        if let Some(ch) = current {
            self.current += ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        current
    }
    /// Determines the type of the current character
    fn char_type(&self) -> Option<CharType> {
        if let Some(ch) = self.peek() {
            if ch.is_alphabetic() || ch == '_' {
                Some(CharType::Alpha)
            } else if ch.is_ascii_digit() {
                Some(CharType::Numeric)
            } else if ch == ' ' || ch == '\t' || ch == '\r' {
                Some(CharType::Whitespace)
            } else {
                // Characters like +, -, *, /, =, ", etc.
                Some(CharType::Special)
            }
        } else {
            None
        }
    }

    /// Parses an identifier or keyword token
    fn parse_identifier_or_keyword(&mut self) -> Token<'a> {
        let start_line = self.line;
        let start_column = self.column;
        let start_pos = self.current;
        let mut identifier = String::new();
        let mut first_loop = true;
        // Collect all alphanumeric characters and underscores
        while let Some(ch) = self.peek() {
            if first_loop {
                first_loop = false;
                if ch.is_alphabetic() || ch == '_' {
                    identifier.push(ch);
                    self.advance();
                    continue;
                } else {
                    unreachable!("parse identifier should be called if current ch is alpha.")
                }
            }
            if ch.is_alphanumeric() || ch == '_' {
                identifier.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let slice = &self.source[start_pos..self.current];

        if !identifier.is_empty() {
            // Check if identifier is a keyword
            match Keyword::try_from(identifier.clone()) {
                Ok(keyword) => Token::new(
                    TokenType::Keyword(keyword),
                    Position::new(start_line, start_column, slice).unwrap(),
                ),
                Err(_) => Token::new(
                    TokenType::Identifier(Identifier::new(identifier)),
                    Position::new(start_line, start_column, slice).unwrap(),
                ),
            }
        } else {
            unreachable!("Parse identifier should be called on alpha,So it can't be null")
        }
    }

    /// Parses a numeric token (integer or float)
    fn parse_numeric(&mut self) -> Token<'a> {
        let start_line = self.line;
        let start_column = self.column;
        let start_pos = self.current;
        let mut number = String::new();
        let mut is_float = false;

        // Collect digits
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                number.push(ch);
                self.advance();
            } else if ch == '.'
                && !is_float
                && self.peek_next().is_some_and(|next| next.is_ascii_digit())
            {
                // Handle decimal point only if it's followed by a digit
                is_float = true;
                number.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let slice = &self.source[start_pos..self.current];

        if number.is_empty() {
            unreachable!("parse number should be called when digit is accessed.")
        }

        if is_float {
            match number.parse::<num_rational::BigRational>() {
                Ok(value) => Token::new(
                    TokenType::Float(value),
                    Position::new(start_line, start_column, slice).unwrap(),
                ),
                Err(_x) =>
                    Token::new(
                    TokenType::Error(TokenizerError::InvalidFloat),
                    Position::new(start_line, start_column, slice).unwrap(),
                ),
            }
        } else {
            match number.parse::<num_bigint::BigInt>() {
                Ok(value) => Token::new(
                    TokenType::Integer(value),
                    Position::new(start_line, start_column, slice).unwrap(),
                ),
                Err(_) => Token::new(
                    TokenType::Error(TokenizerError::InvalidInt),
                    Position::new(start_line, start_column, slice).unwrap(),
                ),
            }
        }
    }

    /// Parses a string literal token
    fn parse_string(&mut self) -> Token<'a> {
        let start_line = self.line;
        let start_column = self.column;
        let start_pos = self.current;
        let mut string_value = String::new();
        if Some('"')==self.peek(){
            self.advance();
        } else{
            unreachable!("Should be checked before.")
        }
        // Skip opening quote

        while let Some(ch) = self.peek() {
            if ch == '"' {
                // Found closing quote
                self.advance();
                let slice = &self.source[start_pos..self.current];
                return Token::new(
                    TokenType::String(string_value),
                    Position::new(start_line, start_column, slice).unwrap(),
                );
            } else if ch == '\\' {
                // Handle escape sequences
                self.advance(); // Skip backslash
                if let Some(escaped) = self.peek() {
                    match escaped {
                        'n' => {
                            string_value.push('\n');
                            self.advance();
                        }
                        't' => {
                            string_value.push('\t');
                            self.advance();
                        }
                        'r' => {
                            string_value.push('\r');
                            self.advance();
                        }
                        '\\' => {
                            string_value.push('\\');
                            self.advance();
                        }
                        '"' => {
                            string_value.push('"');
                            self.advance();
                        }
                        '\n' => {}
                        _ => {
                            // Unknown escape sequence, treat as literal
                            string_value.push('\\');
                            string_value.push(escaped);
                            self.advance();
                        }
                    }
                } else {
                    // Backslash at end of input
                    string_value.push('\\');
                }
            } else if ch == '\n' {
                // Unterminated string at newline
                let slice = &self.source[start_pos..self.current];
                return Token::new(
                    TokenType::Error(TokenizerError::StringInNewLine),
                    Position::new(start_line, start_column, slice).unwrap(),
                );
            } else {
                string_value.push(ch);
                self.advance();
            }
        }

        // Reached end of input without closing quote
        let slice = &self.source[start_pos..self.current];
        Token::new(
            TokenType::Error(TokenizerError::NoClosingBracket),
            Position::new(start_line, start_column, slice).unwrap(),
        )
    }

    /// Parses special characters and operators
    fn parse_special_char(&mut self) -> Token<'a> {
        let start_line = self.line;
        let start_column = self.column;
        let start_pos = self.current;

        if let Some(ch) = self.peek() {
            match ch {
                // Check for two-character tokens first
                '!' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        let slice = &self.source[start_pos..self.current];
                        Token::new(
                            TokenType::BangEqual,
                            Position::new(start_line, start_column, slice).unwrap(),
                        )
                    } else {
                        let slice = &self.source[start_pos..self.current];
                        Token::new(
                            TokenType::Bang,
                            Position::new(start_line, start_column, slice).unwrap(),
                        )
                    }
                }
                '=' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        let slice = &self.source[start_pos..self.current];
                        Token::new(
                            TokenType::EqualEqual,
                            Position::new(start_line, start_column, slice).unwrap(),
                        )
                    } else {
                        let slice = &self.source[start_pos..self.current];
                        Token::new(
                            TokenType::Equal,
                            Position::new(start_line, start_column, slice).unwrap(),
                        )
                    }
                }
                '>' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        let slice = &self.source[start_pos..self.current];
                        Token::new(
                            TokenType::GreaterEqual,
                            Position::new(start_line, start_column, slice).unwrap(),
                        )
                    } else {
                        let slice = &self.source[start_pos..self.current];
                        Token::new(
                            TokenType::Greater,
                            Position::new(start_line, start_column, slice).unwrap(),
                        )
                    }
                }
                '<' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        let slice = &self.source[start_pos..self.current];
                        Token::new(
                            TokenType::LessEqual,
                            Position::new(start_line, start_column, slice).unwrap(),
                        )
                    } else {
                        let slice = &self.source[start_pos..self.current];
                        Token::new(
                            TokenType::Less,
                            Position::new(start_line, start_column, slice).unwrap(),
                        )
                    }
                }
                // Single-character tokens
                '(' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::LeftParen,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
                ')' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::RightParen,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
                '{' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::LeftBrace,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
                '}' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::RightBrace,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
                ',' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::Comma,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
                '.' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::Dot,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
               ':' => {
                   self.advance();
                   let slice = &self.source[start_pos..self.current];
                   Token::new(
                       TokenType::Colon,
                       Position::new(start_line, start_column, slice).unwrap(),
                   )
               }
                '-' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::Minus,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
                '+' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::Plus,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
                ';' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::Semicolon,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
                '/' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::Slash,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
                '*' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::Star,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
               '|' => {
                   self.advance();
                   let slice = &self.source[start_pos..self.current];
                   Token::new(
                       TokenType::Pipe,
                       Position::new(start_line, start_column, slice).unwrap(),
                   )
               }
                '\n' => {
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::Newline,
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
                _ => {
                    // Unknown special character, create an error token
                    let _ = ch;
                    self.advance();
                    let slice = &self.source[start_pos..self.current];
                    Token::new(
                        TokenType::Error(TokenizerError::UnknownSpeicalChar),
                        Position::new(start_line, start_column, slice).unwrap(),
                    )
                }
            }
        } else {
            Token::new(
                TokenType::Error(TokenizerError::NoRightQuote),
                Position::new(start_line, start_column, "").unwrap(),
            )
        }
    }

    /// Skips whitespace characters (except newlines which might be significant)
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_whitespace() && ch != '\n' {
                self.advance();
            } else {
                break;
            }
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct TokenContainer<'a>(Vec<Token<'a>>);

impl<'a> TokenContainer<'a> {
    fn new(tokens: Vec<Token<'a>>) -> Self {
        Self(tokens)
    }
    
    pub fn slice(&self, start: usize, end: usize) -> &TokenSlice<'a> {
        TokenSlice::from_slice(&self.0[start..end])
    }
    
    pub fn full_slice(&self) -> &TokenSlice<'a> {
        TokenSlice::from_slice(&self.0[..])
    }
    
    pub fn len(&self) -> usize {
        self.0.len()
    }
    
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

// IntoIterator implementation for TokenContainer
impl<'a> IntoIterator for TokenContainer<'a> {
    type Item = Token<'a>;
    type IntoIter = std::vec::IntoIter<Token<'a>>;
    
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> Deref for TokenContainer<'a> {
    type Target = TokenSlice<'a>;
    
    fn deref(&self) -> &Self::Target {
        TokenSlice::from_slice(&self.0)
    }
}

#[repr(transparent)]
pub struct TokenSlice<'a>([Token<'a>]);

impl<'a> TokenSlice<'a> {
    fn from_slice<'b>(tokens: &'b [Token<'a>]) -> &'b Self {
        // SAFETY: TokenSlice is repr(transparent) over [Token]
        // This is private - only TokenContainer can create TokenSlice from raw arrays
        unsafe { &*(tokens as *const [Token<'a>] as *const Self) }
    }
    
    pub fn slice(&self, start: usize, end: usize) -> &Self {
        Self::from_slice(&self.0[start..end])
    }
    
    pub fn slice_from(&self, start: usize) -> &Self {
        Self::from_slice(&self.0[start..])
    }
    
    pub fn slice_to(&self, end: usize) -> &Self {
        Self::from_slice(&self.0[..end])
    }
    
    pub fn get_str_slice(&self) -> &'a str {
        let Some(first_token) = self.0.first() else {
            return ""
        };
        let last_token = self.0.last().unwrap();
        
        let start_ptr = first_token.position.slice.as_ptr();
        let last_slice = last_token.position.slice;
        let end_ptr = unsafe { last_slice.as_ptr().add(last_slice.len()) };
        
        let start_offset = start_ptr as usize;
        let end_offset = end_ptr as usize;
        let slice_len = end_offset - start_offset;
        // Tokens are guaranteed sequential from TokenContainer
        unsafe {
            let slice_ptr = start_ptr;
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(slice_ptr, slice_len))
        }
    }
}

// Deref implementation for convenient slice access
impl<'a> Deref for TokenSlice<'a> {
    type Target = [Token<'a>];
    
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// AsRef implementations for convenient conversion
impl<'a> AsRef<[Token<'a>]> for TokenSlice<'a> {
    fn as_ref(&self) -> &[Token<'a>] {
        &self.0
    }
}

// Display implementation for TokenSlice
impl<'a> fmt::Display for TokenSlice<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TokenSlice[{}]: ", self.len())?;
        for (i, token) in self.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", token.token_type)?;
        }
        Ok(())
    }
}
