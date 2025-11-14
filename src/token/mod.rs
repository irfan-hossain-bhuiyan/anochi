//! Token module for the Anochi programming language lexer.
//! Token module for the Anochi programming language lexer.
#[cfg(test)]
mod tests;
pub mod token_type;
pub use token_type::{Identifier, TokenType};

use std::fmt;
use std::num::NonZeroUsize;
use std::ops::Deref;

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
    
    // Helper methods to reduce boilerplate
    fn make_token(&self, token_type: TokenType, start_line: usize, start_column: usize, start_pos: usize) -> Token<'a> {
        let slice = &self.source[start_pos..self.current];
        Token::new(
            token_type,
            Position::new(start_line, start_column, slice).unwrap(),
        )
    }
    
    fn make_token_at_current(&self, token_type: TokenType) -> Token<'a> {
        self.make_token(token_type, self.line, self.column, self.current)
    }
    
    fn make_single_char_token(&mut self, token_type: TokenType, start_line: usize, start_column: usize, start_pos: usize) -> Token<'a> {
        self.advance();
        self.make_token(token_type, start_line, start_column, start_pos)
    }
    
    fn make_two_char_token(&mut self, token_type: TokenType, start_line: usize, start_column: usize, start_pos: usize) -> Token<'a> {
        self.advance(); // First char
        self.advance(); // Second char
        self.make_token(token_type, start_line, start_column, start_pos)
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

        if !identifier.is_empty() {
            // Check if identifier is a keyword
            let token_type = match Keyword::try_from(identifier.clone()) {
                Ok(keyword) => TokenType::Keyword(keyword),
                Err(_) => TokenType::Identifier(Identifier::new(identifier)),
            };
            self.make_token(token_type, start_line, start_column, start_pos)
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

        if number.is_empty() {
            unreachable!("parse number should be called when digit is accessed.")
        }

        let token_type = if is_float {
            match number.parse::<num_rational::BigRational>() {
                Ok(value) => TokenType::Float(value),
                Err(_) => TokenType::Error(TokenizerError::InvalidFloat),
            }
        } else {
            match number.parse::<num_bigint::BigInt>() {
                Ok(value) => TokenType::Integer(value),
                Err(_) => TokenType::Error(TokenizerError::InvalidInt),
            }
        };
        
        self.make_token(token_type, start_line, start_column, start_pos)
    }

    /// Parses a string literal token
    fn parse_string(&mut self) -> Token<'a> {
        let start_line = self.line;
        let start_column = self.column;
        let start_pos = self.current;
        let mut string_value = String::new();
        
        if Some('"') == self.peek() {
            self.advance();
        } else {
            unreachable!("Should be checked before.")
        }
        
        while let Some(ch) = self.peek() {
            if ch == '"' {
                self.advance();
                return self.make_token(TokenType::String(string_value), start_line, start_column, start_pos);
            } else if ch == '\\' {
                self.advance();
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
                            string_value.push('\\');
                            string_value.push(escaped);
                            self.advance();
                        }
                    }
                } else {
                    string_value.push('\\');
                }
            } else if ch == '\n' {
                return self.make_token(TokenType::Error(TokenizerError::StringInNewLine), start_line, start_column, start_pos);
            } else {
                string_value.push(ch);
                self.advance();
            }
        }

        self.make_token(TokenType::Error(TokenizerError::NoClosingBracket), start_line, start_column, start_pos)
    }

    /// Parses special characters and operators
    fn parse_special_char(&mut self) -> Token<'a> {
        let start_line = self.line;
        let start_column = self.column;
        let start_pos = self.current;

        if let Some(ch) = self.peek() {
            match ch {
                // Two-character tokens
                '!' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.make_two_char_token(TokenType::BangEqual, start_line, start_column, start_pos)
                    } else {
                        self.make_token(TokenType::Bang, start_line, start_column, start_pos)
                    }
                }
                '=' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        self.make_token(TokenType::EqualEqual, start_line, start_column, start_pos)
                    } else {
                        self.make_token(TokenType::Equal, start_line, start_column, start_pos)
                    }
                }
                '>' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        self.make_token(TokenType::GreaterEqual, start_line, start_column, start_pos)
                    } else {
                        self.make_token(TokenType::Greater, start_line, start_column, start_pos)
                    }
                }
                '<' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        self.make_token(TokenType::LessEqual, start_line, start_column, start_pos)
                    } else {
                        self.make_token(TokenType::Less, start_line, start_column, start_pos)
                    }
                }
                '-' => {
                    self.advance();
                    if self.peek() == Some('>') {
                        self.advance();
                        self.make_token(TokenType::Arrow, start_line, start_column, start_pos)
                    } else {
                        self.make_token(TokenType::Minus, start_line, start_column, start_pos)
                    }
                }
                // Single-character tokens
                '(' => self.make_single_char_token(TokenType::LeftParen, start_line, start_column, start_pos),
                ')' => self.make_single_char_token(TokenType::RightParen, start_line, start_column, start_pos),
                '{' => self.make_single_char_token(TokenType::LeftBrace, start_line, start_column, start_pos),
                '}' => self.make_single_char_token(TokenType::RightBrace, start_line, start_column, start_pos),
                ',' => self.make_single_char_token(TokenType::Comma, start_line, start_column, start_pos),
                '.' => self.make_single_char_token(TokenType::Dot, start_line, start_column, start_pos),
                ':' => self.make_single_char_token(TokenType::Colon, start_line, start_column, start_pos),
                '+' => self.make_single_char_token(TokenType::Plus, start_line, start_column, start_pos),
                ';' => self.make_single_char_token(TokenType::Semicolon, start_line, start_column, start_pos),
                '/' => self.make_single_char_token(TokenType::Slash, start_line, start_column, start_pos),
                '*' => self.make_single_char_token(TokenType::Star, start_line, start_column, start_pos),
                '|' => self.make_single_char_token(TokenType::Pipe, start_line, start_column, start_pos),
                '\n' => self.make_single_char_token(TokenType::Newline, start_line, start_column, start_pos),
                _ => self.make_single_char_token(TokenType::Error(TokenizerError::UnknownSpeicalChar), start_line, start_column, start_pos),
            }
        } else {
            self.make_token(TokenType::Error(TokenizerError::NoRightQuote), start_line, start_column, start_pos)
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
            return "";
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
