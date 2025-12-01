use crate::{
    code_error::CodeError,
    token::token_type::{Identifier, Keyword, TokenType, TokenizerErrorType},
};
use derive_more::{ Deref};
use macros::generate_unchecked;
use std::{num::NonZeroUsize, ops::Range};
#[derive(Debug, Clone, PartialEq)]
pub enum CharType {
    Alpha,
    Numeric,
    Special,
    Whitespace,
    Skippable,
}
pub type TokenizerError = CodeError<TokenizerErrorType>;
#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub line: NonZeroUsize,
    pub column: NonZeroUsize,
    pub range: Range<usize>,
}
impl Default for Position {
    fn default() -> Self {
        Self {
            line: NonZeroUsize::new(1).unwrap(),
            column: NonZeroUsize::new(1).unwrap(),
            range: 0..0,
        }
    }
}

impl Position {
    #[generate_unchecked]
    pub fn new_checked(line: usize, column: usize, range: Range<usize>) -> Option<Self> {
        Some(Self {
            line: NonZeroUsize::new(line)?,
            column: NonZeroUsize::new(column)?,
            range,
        })
    }

    pub fn start(&self) -> usize {
        self.range.start
    }
    pub fn end(&self) -> usize {
        self.range.end
    }

    fn new_current(line: usize, column: usize, current: usize) -> Self {
        Self::new(
            line,
            column,
            Range {
                start: current,
                end: current,
            },
        )
    }
    pub fn code_str(&self,code:&str)->String{
        let start_idx = self.start();
        let end_idx = self.end();
        if start_idx >= code.len() || end_idx > code.len() || start_idx > end_idx {
            return "[Error: Invalid CodeSlice range]".to_string();
        }
        let slice = &code[start_idx..end_idx];
        let start_line = self.line;
        let start_col = self.column;
        format!(
            "[Error at {}:{}  ({}..{})]: {}",
            start_line, start_col, start_idx, end_idx, slice
        )
    }
    pub fn is_empty(&self) -> bool {
        *self == Self::default()
    }
    #[generate_unchecked]
    pub fn extend_checked(self, end: Self) -> Option<Self> {
        if end.range.start < self.range.start || end.range.end < self.range.end {
            return None;
        }

        Some(Self {
            range: Range {
                start: self.range.start,
                end: end.range.end,
            },
            ..self
        })
    }
}
pub trait HasPosition{
    fn get_position(&self)->&Position;
}
impl HasPosition for Position{
    fn get_position(&self)->&Position {
        self
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub position: Position,
}

impl Token {
    pub fn new(token_type: TokenType, position: Position) -> Self {
        Self {
            token_type,
            position,
        }
    }
}
pub type TokenizerValue = Result<Token, TokenizerError>;
pub struct Tokenizer<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    errors: Vec<TokenizerError>,
    current: usize,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            errors: Vec::new(),
            current: 0,
            line: 1,
            column: 1,
        }
    }
    fn current_position(&self) -> Position {
        Position::new_current(self.line, self.column, self.current)
    }

    pub fn push_error(&mut self, error: TokenizerError) {
        self.errors.push(error);
    }
    pub fn create_error(&self, error_type: TokenizerErrorType) -> TokenizerError {
        TokenizerError::new(error_type, self.current_position())
    }

    // Helper methods to reduce boilerplate
    fn make_token(
        &self,
        token_type: TokenType,
        start_line: usize,
        start_column: usize,
        start_pos: usize,
    ) -> Token {
        let range = start_pos..self.current;
        Token::new(
            token_type,
            Position::new_checked(start_line, start_column, range).unwrap(),
        )
    }

    fn make_token_at_current(&self, token_type: TokenType) -> Token {
        self.make_token(token_type, self.line, self.column, self.current)
    }

    fn make_single_char_token(
        &mut self,
        token_type: TokenType,
        start_line: usize,
        start_column: usize,
        start_pos: usize,
    ) -> Token {
        self.advance();
        self.make_token(token_type, start_line, start_column, start_pos)
    }

    fn make_two_char_token(
        &mut self,
        token_type: TokenType,
        start_line: usize,
        start_column: usize,
        start_pos: usize,
    ) -> Token {
        self.advance(); // First char
        self.advance(); // Second char
        self.make_token(token_type, start_line, start_column, start_pos)
    }
    fn push_value(&mut self, value: TokenizerValue) {
        match value {
            Ok(x) => self.tokens.push(x),
            Err(x) => self.errors.push(x),
        }
    }

    pub fn tokenize(mut self) -> (TokenContainer,TokenizerErrors) {
        while let Some(value) = self.char_type() {
            let value = match value {
                CharType::Alpha => self.parse_identifier_or_keyword(),
                CharType::Numeric => self.parse_numeric(),
                CharType::Whitespace => {
                    self.skip_whitespace();
                    continue;
                }
                CharType::Special => {
                    if self.peek() == Some('"') {
                        self.parse_string()
                    } else {
                        self.parse_special_char()
                    }
                }
                CharType::Skippable => {
                    self.advance();
                    continue;
                }
            };
            self.push_value(value);
        }
        (TokenContainer::new(self.tokens),TokenizerErrors(self.errors))
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
            } else if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' {
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
    fn parse_identifier_or_keyword(&mut self) -> TokenizerValue {
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
            let token_type = match identifier.parse::<Keyword>() {
                Ok(keyword) => TokenType::Keyword(keyword),
                Err(_) => TokenType::Identifier(Identifier::new(identifier)),
            };
            Ok(self.make_token(token_type, start_line, start_column, start_pos))
        } else {
            unreachable!("Parse identifier should be called on alpha,So it can't be null")
        }
    }

    /// Parses a numeric token (integer or float)
    fn parse_numeric(&mut self) -> Result<Token, TokenizerError> {
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
                Err(_) => return Err(self.create_error(TokenizerErrorType::InvalidFloat)),
            }
        } else {
            match number.parse::<num_bigint::BigInt>() {
                Ok(value) => TokenType::Integer(value),
                Err(_) => return Err(self.create_error(TokenizerErrorType::InvalidInt)),
            }
        };

        Ok(self.make_token(token_type, start_line, start_column, start_pos))
    }

    /// Parses a string literal token
    fn parse_string(&mut self) -> TokenizerValue {
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
                return Ok(self.make_token(
                    TokenType::String(string_value),
                    start_line,
                    start_column,
                    start_pos,
                ));
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
                return Err(self.create_error(TokenizerErrorType::StringInNewLine));
            } else {
                string_value.push(ch);
                self.advance();
            }
        }
        Err(self.create_error(TokenizerErrorType::NoClosingBracket))
    }

    /// Parses special characters and operators
    fn parse_special_char(&mut self) -> Result<Token, TokenizerError> {
        let start_line = self.line;
        let start_column = self.column;
        let start_pos = self.current;

        if let Some(ch) = self.peek() {
            let token = match ch {
                // Two-character tokens
                '!' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.make_two_char_token(
                            TokenType::BangEqual,
                            start_line,
                            start_column,
                            start_pos,
                        )
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
                        self.make_token(
                            TokenType::GreaterEqual,
                            start_line,
                            start_column,
                            start_pos,
                        )
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
                '(' => self.make_single_char_token(
                    TokenType::LeftParen,
                    start_line,
                    start_column,
                    start_pos,
                ),
                ')' => self.make_single_char_token(
                    TokenType::RightParen,
                    start_line,
                    start_column,
                    start_pos,
                ),
                '{' => self.make_single_char_token(
                    TokenType::LeftBrace,
                    start_line,
                    start_column,
                    start_pos,
                ),
                '}' => self.make_single_char_token(
                    TokenType::RightBrace,
                    start_line,
                    start_column,
                    start_pos,
                ),
                ',' => self.make_single_char_token(
                    TokenType::Comma,
                    start_line,
                    start_column,
                    start_pos,
                ),
                '.' => {
                    self.make_single_char_token(TokenType::Dot, start_line, start_column, start_pos)
                }
                ':' => self.make_single_char_token(
                    TokenType::Colon,
                    start_line,
                    start_column,
                    start_pos,
                ),
                '+' => self.make_single_char_token(
                    TokenType::Plus,
                    start_line,
                    start_column,
                    start_pos,
                ),
                ';' => self.make_single_char_token(
                    TokenType::Semicolon,
                    start_line,
                    start_column,
                    start_pos,
                ),
                '/' => self.make_single_char_token(
                    TokenType::Slash,
                    start_line,
                    start_column,
                    start_pos,
                ),
                '*' => self.make_single_char_token(
                    TokenType::Star,
                    start_line,
                    start_column,
                    start_pos,
                ),
                '|' => self.make_single_char_token(
                    TokenType::Pipe,
                    start_line,
                    start_column,
                    start_pos,
                ),
                //'\n' =>{self.advance()}
                _ => {
                    self.advance();
                    return Err(self.create_error(TokenizerErrorType::UnknownSpeicalChar));
                }
            };
            Ok(token)
        } else {
            return Err(self.create_error(TokenizerErrorType::NoRightQuote));
        }
    }

    /// Skips whitespace characters (except newlines which might be significant)
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_ascii_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenContainer(Vec<Token>);
#[derive(Debug,Deref)]
struct TokenizerErrors(Vec<TokenizerError>);
impl TokenizerErrors {
    pub fn err_str(&self,code:&str)->String{
        self.iter().map(|x|x.err_str(code)).collect()
    }
}
impl TokenContainer {
    fn new(tokens: Vec<Token>) -> Self {
        Self(tokens)
    }

    pub fn slice(&self, start: usize, end: usize) -> &TokenSlice {
        TokenSlice::from_slice(&self.0[start..end])
    }

    pub fn full_slice(&self) -> &TokenSlice {
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
impl IntoIterator for TokenContainer {
    type Item = Token;
    type IntoIter = std::vec::IntoIter<Token>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

use crate::token::TokenSlice;
use std::ops::Deref;

impl Deref for TokenContainer {
    type Target = TokenSlice;

    fn deref(&self) -> &Self::Target {
        TokenSlice::from_slice(&self.0)
    }
}
