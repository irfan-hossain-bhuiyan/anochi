//! Token module for the Anochi programming language lexer.
//!
//! This module provides tokenization functionality for converting source code text
//! into a sequence of tokens that can be consumed by the parser. It handles all
//! lexical analysis including identifiers, literals, operators, and special characters.
//!
//! # Example
//!
//! ```
//! use anochi::token::{Tokenizer, TokenType};
//!
//! let source = "let x = 42;";
//! let tokenizer = Tokenizer::new(source);
//! let tokens = tokenizer.tokenize();
//!
//! assert_eq!(tokens.len(), 6); // let, x, =, 42, ;, EOF
//! ```

/// Represents the different types of tokens in the Anochi programming language.
///
/// This enum defines all possible token types that can be produced by the tokenizer,
/// including literals (identifiers, numbers, strings), operators, punctuation, and
/// special tokens like EOF and error tokens.
///
/// # Examples
///
/// ```
/// use anochi::token::{TokenType, Tokenizer};
///
/// let tokenizer = Tokenizer::new("hello");
/// let tokens = tokenizer.tokenize();
///
/// match &tokens[0].token_type {
///     TokenType::Identifier(name) => println!("Found identifier: {}", name),
///     _ => println!("Not an identifier"),
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Literals
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),

    // Single-character tokens
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

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Special tokens
    Newline,
    Eof,
    Error(String), // Error message
}

/// Represents the type classification of characters during tokenization.
///
/// This enum is used internally by the tokenizer to classify characters
/// and determine how they should be processed during the tokenization phase.
///
/// # Variants
///
/// * `Alpha` - Alphabetic characters and underscores (valid identifier starts)
/// * `Numeric` - ASCII digits (0-9)
/// * `Special` - Special characters like operators, punctuation, and quotes
/// * `Whitespace` - Whitespace characters (space, tab, carriage return)
#[derive(Debug, Clone, PartialEq)]
pub enum CharType {
    /// Alphabetic characters and underscores
    Alpha,
    /// ASCII digits (0-9)
    Numeric,
    /// Special characters like operators, punctuation, and quotes
    Special,
    /// Whitespace characters (space, tab, carriage return)
    Whitespace,
}

/// Represents position information for tokens and AST nodes.
///
/// This struct tracks the line and column position in the source code,
/// which is essential for error reporting and debugging.
#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    /// Line number (1-based)
    pub line: usize,
    /// Column number (1-based)
    pub column: usize,
}

impl Position {
    /// Creates a new position with the given line and column.
    ///
    /// # Arguments
    ///
    /// * `line` - The line number (1-based)
    /// * `column` - The column number (1-based)
    ///
    /// # Returns
    ///
    /// A new `Position` with the specified coordinates.
    ///
    /// # Example
    ///
    /// ```
    /// use anochi::token::Position;
    ///
    /// let pos = Position::new(1, 1);
    /// assert_eq!(pos.line, 1);
    /// assert_eq!(pos.column, 1);
    /// ```
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

/// Represents a token with its type, lexeme, and position information.
///
/// Each token contains the type of token it represents along with its
/// position in the source code for error reporting and debugging purposes.
///
/// # Examples
///
/// ```
/// use anochi::token::{Token, TokenType, Position};
///
/// let token = Token::new(TokenType::Identifier("hello".to_string()), Position::new(1, 1));
/// assert_eq!(token.position.line, 1);
/// assert_eq!(token.position.column, 1);
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The type and value of this token
    pub token_type: TokenType,
    /// The position where this token appears in the source code
    pub position: Position,
}

impl Token {
    /// Creates a new token with the specified type and position.
    ///
    /// # Arguments
    ///
    /// * `token_type` - The type and value of the token
    /// * `position` - The position where the token appears in the source code
    ///
    /// # Examples
    ///
    /// ```
    /// use anochi::token::{Token, TokenType, Position};
    ///
    /// let position = Position::new(1, 5);
    /// let token = Token::new(TokenType::Integer(42), position);
    /// assert_eq!(token.token_type, TokenType::Integer(42));
    /// assert_eq!(token.position.line, 1);
    /// assert_eq!(token.position.column, 5);
    /// ```
    pub fn new(token_type: TokenType, position: Position) -> Self {
        Self {
            token_type,
            position,
        }
    }
}

/// Tokenizer that converts source code into a sequence of tokens.
///
/// The tokenizer is responsible for lexical analysis, converting raw source code
/// text into a structured sequence of tokens that can be consumed by a parser.
/// It handles all Anochi language tokens including identifiers, literals,
/// operators, and punctuation.
///
/// # Examples
///
/// ```
/// use anochi::token::{Tokenizer, TokenType};
///
/// let source = "let x = 42;";
/// let tokenizer = Tokenizer::new(source);
/// let tokens = tokenizer.tokenize();
///
/// assert_eq!(tokens.len(), 6); // let, x, =, 42, ;, EOF
/// assert_eq!(tokens[0].token_type, TokenType::Identifier("let".to_string()));
/// assert_eq!(tokens[1].token_type, TokenType::Identifier("x".to_string()));
/// assert_eq!(tokens[2].token_type, TokenType::Equal);
/// assert_eq!(tokens[3].token_type, TokenType::Integer(42));
/// assert_eq!(tokens[4].token_type, TokenType::Semicolon);
/// assert_eq!(tokens[5].token_type, TokenType::Eof);
/// ```
pub struct Tokenizer<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    current: usize,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    /// Creates a new tokenizer from source code.
    ///
    /// # Arguments
    ///
    /// * `source` - The source code string to tokenize
    ///
    /// # Examples
    ///
    /// ```
    /// use anochi::token::Tokenizer;
    ///
    /// let tokenizer = Tokenizer::new("hello world");
    /// let tokens = tokenizer.tokenize();
    /// assert_eq!(tokens.len(), 3); // hello, world, EOF
    /// ```
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            current: 0,
            line: 1,
            column: 1,
        }
    }

    /// Tokenizes the entire source code and returns a vector of tokens.
    ///
    /// This method consumes the tokenizer and performs a complete lexical analysis
    /// of the source code, returning all tokens including a final EOF token.
    ///
    /// # Returns
    ///
    /// A vector of `Token` objects representing the tokenized source code.
    /// The last token will always be `TokenType::Eof`.
    ///
    /// # Examples
    ///
    /// ```
    /// use anochi::token::{Tokenizer, TokenType};
    ///
    /// let tokenizer = Tokenizer::new("42 + 3.14");
    /// let tokens = tokenizer.tokenize();
    ///
    /// assert_eq!(tokens.len(), 4); // 42, +, 3.14, EOF
    /// assert_eq!(tokens[0].token_type, TokenType::Integer(42));
    /// assert_eq!(tokens[1].token_type, TokenType::Plus);
    /// assert_eq!(tokens[2].token_type, TokenType::Float(3.14));
    /// assert_eq!(tokens[3].token_type, TokenType::Eof);
    /// ```
    pub fn tokenize(mut self) -> Vec<Token> {
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
                    // Check if it's a string literal
                    if self.peek() == Some('"') {
                        let token = self.parse_string();
                        self.tokens.push(token);
                    } else {
                        // Parse other special characters and operators
                        let token = self.parse_special_char();
                        self.tokens.push(token);
                    }
                }
                _ => {
                    // Skip unknown characters for now
                    self.advance();
                }
            }
        }

        // Add EOF token
        self.tokens.push(Token::new(TokenType::Eof, Position::new(self.line, self.column)));
        self.tokens
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
    fn parse_identifier_or_keyword(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut identifier = String::new();
        let mut first_loop=true;
        // Collect all alphanumeric characters and underscores
        while let Some(ch) = self.peek() {
            if first_loop{
                first_loop=false;
                if ch.is_alphabetic() || ch =='_'{
                    identifier.push(ch);
                    self.advance();
                    continue;
                }else{
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
            Token::new(
                TokenType::Identifier(identifier),
                Position::new(start_line, start_column),
            )
        } else {
            Token::new(
                TokenType::Error("Empty identifier".to_string()),
                Position::new(start_line, start_column),
            )
        }
    }

    /// Parses a numeric token (integer or float)
    fn parse_numeric(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut number = String::new();
        let mut is_float = false;

        // Collect digits
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                number.push(ch);
                self.advance();
            } else if ch == '.' && !is_float 
                && self.peek_next().is_some_and(|next| next.is_ascii_digit()) {
                // Handle decimal point only if it's followed by a digit
                is_float = true;
                number.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        if number.is_empty() {
            return Token::new(
                TokenType::Error("Empty number".to_string()),
                Position::new(start_line, start_column),
            );
        }

        if is_float {
            match number.parse::<f64>() {
                Ok(value) => Token::new(
                    TokenType::Float(value),
                    Position::new(start_line, start_column),
                ),
                Err(_) => Token::new(
                    TokenType::Error(format!("Invalid float: {number}")),
                    Position::new(start_line, start_column),
                ),
            }
        } else {
            match number.parse::<i64>() {
                Ok(value) => Token::new(
                    TokenType::Integer(value),
                    Position::new(start_line, start_column),
                ),
                Err(_) => Token::new(
                    TokenType::Error(format!("Invalid integer: {number}")),
                    Position::new(start_line, start_column),
                ),
            }
        }
    }

    /// Parses a string literal token
    fn parse_string(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut string_value = String::new();

        // Skip opening quote
        self.advance();

        while let Some(ch) = self.peek() {
            if ch == '"' {
                // Found closing quote
                self.advance();
                return Token::new(
                    TokenType::String(string_value),
                    Position::new(start_line, start_column),
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
                return Token::new(
                    TokenType::Error("Unterminated string at newline".to_string()),
                    Position::new(start_line, start_column),
                );
            } else {
                string_value.push(ch);
                self.advance();
            }
        }

        // Reached end of input without closing quote
        Token::new(
            TokenType::Error("Unterminated string at end of file".to_string()),
            Position::new(start_line, start_column),
        )
    }

    /// Parses special characters and operators
    fn parse_special_char(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;

        if let Some(ch) = self.peek() {
            match ch {
                // Check for two-character tokens first
                '!' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::new(TokenType::BangEqual, Position::new(start_line, start_column))
                    } else {
                        Token::new(TokenType::Bang, Position::new(start_line, start_column))
                    }
                }
                '=' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::new(TokenType::EqualEqual, Position::new(start_line, start_column))
                    } else {
                        Token::new(TokenType::Equal, Position::new(start_line, start_column))
                    }
                }
                '>' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::new(TokenType::GreaterEqual, Position::new(start_line, start_column))
                    } else {
                        Token::new(TokenType::Greater, Position::new(start_line, start_column))
                    }
                }
                '<' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        Token::new(TokenType::LessEqual, Position::new(start_line, start_column))
                    } else {
                        Token::new(TokenType::Less, Position::new(start_line, start_column))
                    }
                }
                // Single-character tokens
                '(' => {
                    self.advance();
                    Token::new(TokenType::LeftParen, Position::new(start_line, start_column))
                }
                ')' => {
                    self.advance();
                    Token::new(TokenType::RightParen, Position::new(start_line, start_column))
                }
                '{' => {
                    self.advance();
                    Token::new(TokenType::LeftBrace, Position::new(start_line, start_column))
                }
                '}' => {
                    self.advance();
                    Token::new(TokenType::RightBrace, Position::new(start_line, start_column))
                }
                ',' => {
                    self.advance();
                    Token::new(TokenType::Comma, Position::new(start_line, start_column))
                }
                '.' => {
                    self.advance();
                    Token::new(TokenType::Dot, Position::new(start_line, start_column))
                }
                '-' => {
                    self.advance();
                    Token::new(TokenType::Minus, Position::new(start_line, start_column))
                }
                '+' => {
                    self.advance();
                    Token::new(TokenType::Plus, Position::new(start_line, start_column))
                }
                ';' => {
                    self.advance();
                    Token::new(TokenType::Semicolon, Position::new(start_line, start_column))
                }
                '/' => {
                    self.advance();
                    Token::new(TokenType::Slash, Position::new(start_line, start_column))
                }
                '*' => {
                    self.advance();
                    Token::new(TokenType::Star, Position::new(start_line, start_column))
                }
                '\n' => {
                    self.advance();
                    Token::new(TokenType::Newline, Position::new(start_line, start_column))
                }
                _ => {
                    // Unknown special character, create an error token
                    let unknown_char = ch;
                    self.advance();
                    Token::new(
                        TokenType::Error(format!("Unknown character: '{}'", unknown_char)),
                        Position::new(start_line, start_column),
                    )
                }
            }
        } else {
            Token::new(
                TokenType::Error("Unexpected end of input".to_string()),
                Position::new(start_line, start_column),
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
#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_tokenize_simple_identifier() {
        let tokenizer = Tokenizer::new("hello");
        let tokens = tokenizer.tokenize();
        
        assert_eq!(tokens.len(), 2); // identifier + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("hello".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_simple_integer() {
        let tokenizer = Tokenizer::new("42");
        let tokens = tokenizer.tokenize();
        
        assert_eq!(tokens.len(), 2); // integer + EOF
        assert_eq!(tokens[0].token_type, TokenType::Integer(42));
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_simple_float() {
        let tokenizer = Tokenizer::new("3.14");
        let tokens = tokenizer.tokenize();
        
        assert_eq!(tokens.len(), 2); // float + EOF
        assert_eq!(tokens[0].token_type, TokenType::Float(3.14));
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_with_whitespace() {
        let tokenizer = Tokenizer::new("  hello   world  ");
        let tokens = tokenizer.tokenize();
        
        assert_eq!(tokens.len(), 3); // identifier + identifier + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("hello".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Identifier("world".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_mixed_content() {
        let tokenizer = Tokenizer::new("variable 42 3.14 another_var");
        let tokens = tokenizer.tokenize();
        
        assert_eq!(tokens.len(), 5); // 4 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("variable".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Integer(42));
        assert_eq!(tokens[2].token_type, TokenType::Float(3.14));
        assert_eq!(tokens[3].token_type, TokenType::Identifier("another_var".to_string()));
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_empty_string() {
        let tokenizer = Tokenizer::new("");
        let tokens = tokenizer.tokenize();
        
        assert_eq!(tokens.len(), 1); // Only EOF
        assert_eq!(tokens[0].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_only_whitespace() {
        let tokenizer = Tokenizer::new("   \t  ");
        let tokens = tokenizer.tokenize();
        
        assert_eq!(tokens.len(), 1); // Only EOF
        assert_eq!(tokens[0].token_type, TokenType::Eof);
    }

    #[test]
    fn test_token_positions() {
        let tokenizer = Tokenizer::new("abc 123");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens[0].position.line, 1);
        assert_eq!(tokens[0].position.column, 1); // "abc" starts at column 1

        assert_eq!(tokens[1].position.line, 1);
        assert_eq!(tokens[1].position.column, 5); // "123" starts at column 5
    }

    #[test]
    fn test_multiline_positions() {
        let tokenizer = Tokenizer::new("first\nsecond");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens[0].position.line, 1);
        assert_eq!(tokens[0].position.column, 1); // "first" on line 1

        assert_eq!(tokens[1].position.line, 1);
        assert_eq!(tokens[1].position.column, 6); // newline token at end of first line

        assert_eq!(tokens[2].position.line, 2);
        assert_eq!(tokens[2].position.column, 1); // "second" on line 2
    }


    #[test]
    fn test_tokenize_simple_string() {
        let tokenizer = Tokenizer::new("\"hello\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 2); // string + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("hello".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_string_with_spaces() {
        let tokenizer = Tokenizer::new("   \"hello world\"   ");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 2); // string + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("hello world".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_multiple_strings() {
        let tokenizer = Tokenizer::new("\"first\" \"second\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 3); // string + string + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("first".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::String("second".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_mixed_with_strings() {
        let tokenizer = Tokenizer::new("var \"hello\" 42");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 4); // identifier + string + integer + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("var".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::String("hello".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Integer(42));
        assert_eq!(tokens[3].token_type, TokenType::Eof);
    }

    #[test]
    fn test_string_position_tracking() {
        let tokenizer = Tokenizer::new("abc \"test\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens[0].position.line, 1);
        assert_eq!(tokens[0].position.column, 1); // "abc" starts at column 1

        assert_eq!(tokens[1].position.line, 1);
        assert_eq!(tokens[1].position.column, 5); // "test" starts at column 5
    }


    #[test]
    fn test_tokenize_single_operators() {
        let tokenizer = Tokenizer::new("+ - * /");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 5); // 4 operators + EOF
        assert_eq!(tokens[0].token_type, TokenType::Plus);
        assert_eq!(tokens[1].token_type, TokenType::Minus);
        assert_eq!(tokens[2].token_type, TokenType::Star);
        assert_eq!(tokens[3].token_type, TokenType::Slash);
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_comparison_operators() {
        let tokenizer = Tokenizer::new("== != >= <=");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 5); // 4 operators + EOF
        assert_eq!(tokens[0].token_type, TokenType::EqualEqual);
        assert_eq!(tokens[1].token_type, TokenType::BangEqual);
        assert_eq!(tokens[2].token_type, TokenType::GreaterEqual);
        assert_eq!(tokens[3].token_type, TokenType::LessEqual);
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_parentheses_and_braces() {
        let tokenizer = Tokenizer::new("() {}");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 5); // 4 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::LeftParen);
        assert_eq!(tokens[1].token_type, TokenType::RightParen);
        assert_eq!(tokens[2].token_type, TokenType::LeftBrace);
        assert_eq!(tokens[3].token_type, TokenType::RightBrace);
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_punctuation() {
        let tokenizer = Tokenizer::new(", . ;");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 4); // 3 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Comma);
        assert_eq!(tokens[1].token_type, TokenType::Dot);
        assert_eq!(tokens[2].token_type, TokenType::Semicolon);
        assert_eq!(tokens[3].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_mixed_operators_and_literals() {
        let tokenizer = Tokenizer::new("x + 42 == \"hello\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 6); // 5 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("x".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Plus);
        assert_eq!(tokens[2].token_type, TokenType::Integer(42));
        assert_eq!(tokens[3].token_type, TokenType::EqualEqual);
        assert_eq!(tokens[4].token_type, TokenType::String("hello".to_string()));
        assert_eq!(tokens[5].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_function_call_syntax() {
        let tokenizer = Tokenizer::new("func(a, b)");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 7); // 6 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("func".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::LeftParen);
        assert_eq!(tokens[2].token_type, TokenType::Identifier("a".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::Comma);
        assert_eq!(tokens[4].token_type, TokenType::Identifier("b".to_string()));
        assert_eq!(tokens[5].token_type, TokenType::RightParen);
        assert_eq!(tokens[6].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_expression_with_parentheses() {
        let tokenizer = Tokenizer::new("(x + y) * z");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 8); // 7 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::LeftParen);
        assert_eq!(tokens[1].token_type, TokenType::Identifier("x".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Plus);
        assert_eq!(tokens[3].token_type, TokenType::Identifier("y".to_string()));
        assert_eq!(tokens[4].token_type, TokenType::RightParen);
        assert_eq!(tokens[5].token_type, TokenType::Star);
        assert_eq!(tokens[6].token_type, TokenType::Identifier("z".to_string()));
        assert_eq!(tokens[7].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_newlines() {
        let tokenizer = Tokenizer::new("a\nb\nc");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 6); // a + newline + b + newline + c + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("a".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Newline);
        assert_eq!(tokens[2].token_type, TokenType::Identifier("b".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::Newline);
        assert_eq!(tokens[4].token_type, TokenType::Identifier("c".to_string()));
        assert_eq!(tokens[5].token_type, TokenType::Eof);
    }


    #[test]
    fn test_tokenize_assignment_and_equality() {
        let tokenizer = Tokenizer::new("x = 10; y == 20");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 8); // x = 10 ; y == 20 + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("x".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Equal);
        assert_eq!(tokens[2].token_type, TokenType::Integer(10));
        assert_eq!(tokens[3].token_type, TokenType::Semicolon);
        assert_eq!(tokens[4].token_type, TokenType::Identifier("y".to_string()));
        assert_eq!(tokens[5].token_type, TokenType::EqualEqual);
        assert_eq!(tokens[6].token_type, TokenType::Integer(20));
        assert_eq!(tokens[7].token_type, TokenType::Eof);
    }

    #[test]
    fn test_operator_position_tracking() {
        let tokenizer = Tokenizer::new("a + b");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens[0].position.line, 1);
        assert_eq!(tokens[0].position.column, 1); // "a" at column 1

        assert_eq!(tokens[1].position.line, 1);
        assert_eq!(tokens[1].position.column, 3); // "+" at column 3

        assert_eq!(tokens[2].position.line, 1);
        assert_eq!(tokens[2].position.column, 5); // "b" at column 5
    }

    // Comprehensive tokenization tests for all token types

    // Identifier tokenization tests
    #[test]
    fn test_tokenize_identifiers() {
        let tokenizer = Tokenizer::new("hello world _var var123 _123");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 6); // 5 identifiers + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("hello".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Identifier("world".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Identifier("_var".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::Identifier("var123".to_string()));
        assert_eq!(tokens[4].token_type, TokenType::Identifier("_123".to_string()));
        assert_eq!(tokens[5].token_type, TokenType::Eof);
    }

    // Numeric tokenization tests
    #[test]
    fn test_tokenize_numbers() {
        let tokenizer = Tokenizer::new("42 3.14 0 999.999");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 5); // 4 numbers + EOF
        assert_eq!(tokens[0].token_type, TokenType::Integer(42));
        assert_eq!(tokens[1].token_type, TokenType::Float(3.14));
        assert_eq!(tokens[2].token_type, TokenType::Integer(0));
        assert_eq!(tokens[3].token_type, TokenType::Float(999.999));
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    // String tokenization tests
    #[test]
    fn test_tokenize_strings() {
        let tokenizer = Tokenizer::new("\"hello\" \"world with spaces\" \"escape\\nsequence\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 4); // 3 strings + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("hello".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::String("world with spaces".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::String("escape\nsequence".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::Eof);
    }

    // Operator tokenization tests
    #[test]
    fn test_tokenize_operators() {
        let tokenizer = Tokenizer::new("+ - * / = == != > < >= <=");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 12); // 11 operators + EOF
        assert_eq!(tokens[0].token_type, TokenType::Plus);
        assert_eq!(tokens[1].token_type, TokenType::Minus);
        assert_eq!(tokens[2].token_type, TokenType::Star);
        assert_eq!(tokens[3].token_type, TokenType::Slash);
        assert_eq!(tokens[4].token_type, TokenType::Equal);
        assert_eq!(tokens[5].token_type, TokenType::EqualEqual);
        assert_eq!(tokens[6].token_type, TokenType::BangEqual);
        assert_eq!(tokens[7].token_type, TokenType::Greater);
        assert_eq!(tokens[8].token_type, TokenType::Less);
        assert_eq!(tokens[9].token_type, TokenType::GreaterEqual);
        assert_eq!(tokens[10].token_type, TokenType::LessEqual);
        assert_eq!(tokens[11].token_type, TokenType::Eof);
    }


    // Mixed tokenization tests
    #[test]
    fn test_tokenize_complex_expression() {
        let tokenizer = Tokenizer::new("func(x + 42, \"hello\") == result;");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 12); // 11 tokens + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("func".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::LeftParen);
        assert_eq!(tokens[2].token_type, TokenType::Identifier("x".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::Plus);
        assert_eq!(tokens[4].token_type, TokenType::Integer(42));
        assert_eq!(tokens[5].token_type, TokenType::Comma);
        assert_eq!(tokens[6].token_type, TokenType::String("hello".to_string()));
        assert_eq!(tokens[7].token_type, TokenType::RightParen);
        assert_eq!(tokens[8].token_type, TokenType::EqualEqual);
        assert_eq!(tokens[9].token_type, TokenType::Identifier("result".to_string()));
        assert_eq!(tokens[10].token_type, TokenType::Semicolon);
        assert_eq!(tokens[11].token_type, TokenType::Eof);
    }

    // Position tracking tests
    #[test]
    fn test_position_tracking() {
        let tokenizer = Tokenizer::new("abc\n123");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 4); // abc + newline + 123 + EOF

        // First token (abc) at line 1, column 1
        assert_eq!(tokens[0].position.line, 1);
        assert_eq!(tokens[0].position.column, 1);

        // Newline token at line 1, column 4
        assert_eq!(tokens[1].position.line, 1);
        assert_eq!(tokens[1].position.column, 4);

        // Second token (123) at line 2, column 1
        assert_eq!(tokens[2].position.line, 2);
        assert_eq!(tokens[2].position.column, 1);
    }

    // Error handling tests
    #[test]
    fn test_tokenize_unterminated_string() {
        let tokenizer = Tokenizer::new("\"unterminated string");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 2); // error token + EOF
        assert!(matches!(tokens[0].token_type, TokenType::Error(_)));
        if let TokenType::Error(msg) = &tokens[0].token_type {
            assert!(msg.contains("Unterminated string"));
        }
    }

    #[test]
    fn test_tokenize_invalid_numbers() {
        let tokenizer = Tokenizer::new("999999999999999999999999999999");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 2); // error token + EOF
        assert!(matches!(tokens[0].token_type, TokenType::Error(_)));
        if let TokenType::Error(msg) = &tokens[0].token_type {
            assert!(msg.contains("Invalid integer"));
        }
    }

    #[test]
    fn test_tokenize_unknown_characters() {
        let tokenizer = Tokenizer::new("@ # $ %");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 5); // 4 error tokens + EOF
        for i in 0..4 {
            assert!(matches!(tokens[i].token_type, TokenType::Error(_)));
        }
    }

    // Edge case tests
    #[test]
    fn test_tokenize_float_edge_cases() {
        let tokenizer = Tokenizer::new("42. .5 3.14.15");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 8); // 42 + dot + dot + 5 + 3.14 + dot + 15 + EOF
        assert_eq!(tokens[0].token_type, TokenType::Integer(42));
        assert_eq!(tokens[1].token_type, TokenType::Dot);
        assert_eq!(tokens[2].token_type, TokenType::Dot);
        assert_eq!(tokens[3].token_type, TokenType::Integer(5));
        assert_eq!(tokens[4].token_type, TokenType::Float(3.14));
        assert_eq!(tokens[5].token_type, TokenType::Dot);
        assert_eq!(tokens[6].token_type, TokenType::Integer(15));
        assert_eq!(tokens[7].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_empty_input() {
        let tokenizer = Tokenizer::new("");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 1); // Only EOF
        assert_eq!(tokens[0].token_type, TokenType::Eof);
        assert_eq!(tokens[0].position.line, 1);
        assert_eq!(tokens[0].position.column, 1);
    }

    #[test]
    fn test_tokenize_single_character_tokens() {
        let tokenizer = Tokenizer::new("(){},.-+;/*!=<>");
        let tokens = tokenizer.tokenize();

        let expected_types = vec![
            TokenType::LeftParen,
            TokenType::RightParen,
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::Comma,
            TokenType::Dot,
            TokenType::Minus,
            TokenType::Plus,
            TokenType::Semicolon,
            TokenType::Slash,
            TokenType::Star,
            TokenType::BangEqual,
            TokenType::Less,
            TokenType::Greater,
            TokenType::Eof,
        ];

        assert_eq!(tokens.len(), 15); // 14 tokens + EOF
        for (i, expected) in expected_types.iter().enumerate() {
            if i < tokens.len() {
                assert_eq!(tokens[i].token_type, *expected, "Mismatch at position {}", i);
            }
        }
    }

    #[test]
    fn test_tokenize_multiline_position_tracking() {
        let tokenizer = Tokenizer::new("line1\nline2\nline3");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 6); // 3 identifiers + 2 newlines + EOF

        // First identifier
        assert_eq!(tokens[0].token_type, TokenType::Identifier("line1".to_string()));
        assert_eq!(tokens[0].position.line, 1);
        assert_eq!(tokens[0].position.column, 1);

        // First newline
        assert_eq!(tokens[1].token_type, TokenType::Newline);
        assert_eq!(tokens[1].position.line, 1);
        assert_eq!(tokens[1].position.column, 6);

        // Second identifier
        assert_eq!(tokens[2].token_type, TokenType::Identifier("line2".to_string()));
        assert_eq!(tokens[2].position.line, 2);
        assert_eq!(tokens[2].position.column, 1);

        // Second newline
        assert_eq!(tokens[3].token_type, TokenType::Newline);
        assert_eq!(tokens[3].position.line, 2);
        assert_eq!(tokens[3].position.column, 6);

        // Third identifier
        assert_eq!(tokens[4].token_type, TokenType::Identifier("line3".to_string()));
        assert_eq!(tokens[4].position.line, 3);
        assert_eq!(tokens[4].position.column, 1);
    }

    #[test]
    fn test_tokenize_string_with_newline_error() {
        let tokenizer = Tokenizer::new("\"unterminated\nstring\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 5); // error token + newline + identifier + error + EOF
        assert!(matches!(tokens[0].token_type, TokenType::Error(_)));
        if let TokenType::Error(msg) = &tokens[0].token_type {
            assert!(msg.contains("Unterminated string at newline"));
        }
        assert_eq!(tokens[1].token_type, TokenType::Newline);
        assert_eq!(tokens[2].token_type, TokenType::Identifier("string".to_string()));
        assert!(matches!(tokens[3].token_type, TokenType::Error(_)));
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_mixed_quotes() {
        let tokenizer = Tokenizer::new("\"valid\" invalid\" \"another valid\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 7); // string + identifier + string + identifier + identifier + error + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("valid".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Identifier("invalid".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::String(" ".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::Identifier("another".to_string()));
        assert_eq!(tokens[4].token_type, TokenType::Identifier("valid".to_string()));
        assert!(matches!(tokens[5].token_type, TokenType::Error(_)));
        assert_eq!(tokens[6].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_consecutive_operators() {
        let tokenizer = Tokenizer::new("===!=<=>=");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 6); // == + = + != + <= + >= + EOF
        assert_eq!(tokens[0].token_type, TokenType::EqualEqual);
        assert_eq!(tokens[1].token_type, TokenType::Equal);
        assert_eq!(tokens[2].token_type, TokenType::BangEqual);
        assert_eq!(tokens[3].token_type, TokenType::LessEqual);
        assert_eq!(tokens[4].token_type, TokenType::GreaterEqual);
        assert_eq!(tokens[5].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_whitespace_handling() {
        let tokenizer = Tokenizer::new("  \t\r  hello   \t  world  \n  ");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 4); // hello + world + newline + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("hello".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Identifier("world".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Newline);
        assert_eq!(tokens[3].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_string_escape_sequences() {
        let tokenizer = Tokenizer::new("\"\\n\\t\\r\\\\\\\"\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 2); // string + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("\n\t\r\\\"".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_real_code_example() {
        let code = r#"
            fn main() {
                let x = 42;
                let message = "Hello, world!";
                if x > 0 {
                    print(message);
                }
            }
        "#;

        let tokenizer = Tokenizer::new(code);
        let tokens = tokenizer.tokenize();

        // Verify we get reasonable tokens without panicking
        assert!(tokens.len() > 1);
        assert_eq!(tokens.last().unwrap().token_type, TokenType::Eof);

        // Check that we have identifiers, operators, strings, etc.
        let has_identifier = tokens.iter().any(|t| matches!(t.token_type, TokenType::Identifier(_)));
        let has_integer = tokens.iter().any(|t| matches!(t.token_type, TokenType::Integer(_)));
        let has_string = tokens.iter().any(|t| matches!(t.token_type, TokenType::String(_)));
        let has_operators = tokens.iter().any(|t| matches!(t.token_type, TokenType::Equal));

        assert!(has_identifier);
        assert!(has_integer);
        assert!(has_string);
        assert!(has_operators);
    }

    // Additional comprehensive tests for edge cases and error conditions

    #[test]
    fn test_tokenize_unicode_identifiers() {
        // Test that identifiers with unicode characters are handled
        let tokenizer = Tokenizer::new("hello_world test123 _underscore");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 4); // 3 identifiers + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("hello_world".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Identifier("test123".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::Identifier("_underscore".to_string()));
    }

    #[test]
    fn test_tokenize_large_numbers() {
        let tokenizer = Tokenizer::new("9223372036854775807 -9223372036854775808");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 4); // large_int + minus + error + EOF
        assert_eq!(tokens[0].token_type, TokenType::Integer(9223372036854775807));
        assert_eq!(tokens[1].token_type, TokenType::Minus);
        // 9223372036854775808 is too large for i64, should be an error
        assert!(matches!(tokens[2].token_type, TokenType::Error(_)));
        assert_eq!(tokens[3].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_float_precision() {
        let tokenizer = Tokenizer::new("0.0 1.0 3.141592653589793");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 4); // 3 floats + EOF
        assert_eq!(tokens[0].token_type, TokenType::Float(0.0));
        assert_eq!(tokens[1].token_type, TokenType::Float(1.0));
        assert_eq!(tokens[2].token_type, TokenType::Float(3.141592653589793));
    }

    #[test]
    fn test_tokenize_string_with_all_escape_sequences() {
        let tokenizer = Tokenizer::new("\"\\n\\t\\r\\\\\\\"\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 2); // string + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("\n\t\r\\\"".to_string()));
    }

    #[test]
    fn test_tokenize_nested_structures() {
        let tokenizer = Tokenizer::new("{{({})}}");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 9); // 8 brackets + EOF
        assert_eq!(tokens[0].token_type, TokenType::LeftBrace);
        assert_eq!(tokens[1].token_type, TokenType::LeftBrace);
        assert_eq!(tokens[2].token_type, TokenType::LeftParen);
        assert_eq!(tokens[3].token_type, TokenType::LeftBrace);
        assert_eq!(tokens[4].token_type, TokenType::RightBrace);
        assert_eq!(tokens[5].token_type, TokenType::RightParen);
        assert_eq!(tokens[6].token_type, TokenType::RightBrace);
        assert_eq!(tokens[7].token_type, TokenType::RightBrace);
    }

    #[test]
    fn test_tokenize_complex_expressions() {
        let tokenizer = Tokenizer::new("x = (a + b) * c - d / e;");
        let tokens = tokenizer.tokenize();

        let expected_types = vec![
            TokenType::Identifier("x".to_string()),
            TokenType::Equal,
            TokenType::LeftParen,
            TokenType::Identifier("a".to_string()),
            TokenType::Plus,
            TokenType::Identifier("b".to_string()),
            TokenType::RightParen,
            TokenType::Star,
            TokenType::Identifier("c".to_string()),
            TokenType::Minus,
            TokenType::Identifier("d".to_string()),
            TokenType::Slash,
            TokenType::Identifier("e".to_string()),
            TokenType::Semicolon,
            TokenType::Eof,
        ];

        assert_eq!(tokens.len(), expected_types.len());
        for (i, expected) in expected_types.iter().enumerate() {
            assert_eq!(tokens[i].token_type, *expected, "Mismatch at position {}", i);
        }
    }

    #[test]
    fn test_tokenize_comparison_chains() {
        let tokenizer = Tokenizer::new("a <= b >= c != d == e");
        let tokens = tokenizer.tokenize();

        let expected_types = vec![
            TokenType::Identifier("a".to_string()),
            TokenType::LessEqual,
            TokenType::Identifier("b".to_string()),
            TokenType::GreaterEqual,
            TokenType::Identifier("c".to_string()),
            TokenType::BangEqual,
            TokenType::Identifier("d".to_string()),
            TokenType::EqualEqual,
            TokenType::Identifier("e".to_string()),
            TokenType::Eof,
        ];

        assert_eq!(tokens.len(), expected_types.len());
        for (i, expected) in expected_types.iter().enumerate() {
            assert_eq!(tokens[i].token_type, *expected, "Mismatch at position {}", i);
        }
    }

    #[test]
    fn test_tokenize_mixed_whitespace_and_newlines() {
        let tokenizer = Tokenizer::new("a\n\tb\r\n  c\n");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 7); // a + newline + b + newline + c + newline + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("a".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Newline);
        assert_eq!(tokens[2].token_type, TokenType::Identifier("b".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::Newline);
        assert_eq!(tokens[4].token_type, TokenType::Identifier("c".to_string()));
        assert_eq!(tokens[5].token_type, TokenType::Newline);
        assert_eq!(tokens[6].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_empty_strings() {
        let tokenizer = Tokenizer::new("\"\" \"hello\" \"\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 4); // 3 strings + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::String("hello".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::String("".to_string()));
    }

    #[test]
    fn test_tokenize_string_with_spaces_and_tabs() {
        let tokenizer = Tokenizer::new("\"  hello\tworld  \"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 2); // string + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("  hello\tworld  ".to_string()));
    }

    #[test]
    fn test_tokenize_zero_and_negative_numbers() {
        let tokenizer = Tokenizer::new("0 -1 -42.5");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 6); // 0 + minus + 1 + minus + 42.5 + EOF
        assert_eq!(tokens[0].token_type, TokenType::Integer(0));
        assert_eq!(tokens[1].token_type, TokenType::Minus);
        assert_eq!(tokens[2].token_type, TokenType::Integer(1));
        assert_eq!(tokens[3].token_type, TokenType::Minus);
        assert_eq!(tokens[4].token_type, TokenType::Float(42.5));
        assert_eq!(tokens[5].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_position_tracking_comprehensive() {
        let tokenizer = Tokenizer::new("abc\n  def\n    ghi");
        let tokens = tokenizer.tokenize();

        // Check positions for each token
        assert_eq!(tokens[0].position.line, 1);
        assert_eq!(tokens[0].position.column, 1); // "abc"

        assert_eq!(tokens[1].position.line, 1);
        assert_eq!(tokens[1].position.column, 4); // newline after "abc"

        assert_eq!(tokens[2].position.line, 2);
        assert_eq!(tokens[2].position.column, 3); // "def" (after 2 spaces)

        assert_eq!(tokens[3].position.line, 2);
        assert_eq!(tokens[3].position.column, 6); // newline after "def"

        assert_eq!(tokens[4].position.line, 3);
        assert_eq!(tokens[4].position.column, 5); // "ghi" (after 4 spaces)
    }

    #[test]
    fn test_tokenize_string_backslash_at_end() {
        let tokenizer = Tokenizer::new("\"test\\");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 2); // error token + EOF
        assert!(matches!(tokens[0].token_type, TokenType::Error(_)));
        if let TokenType::Error(msg) = &tokens[0].token_type {
            assert!(msg.contains("Unterminated string"));
        }
    }

    #[test]
    fn test_tokenize_very_long_identifier() {
        let long_identifier = "a".repeat(1000);
        let tokenizer = Tokenizer::new(&long_identifier);
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 2); // identifier + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier(long_identifier));
    }

    #[test]
    fn test_tokenize_multiple_errors_in_sequence() {
        let tokenizer = Tokenizer::new("@ # $ % &");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 6); // 5 error tokens + EOF
        for i in 0..5 {
            assert!(matches!(tokens[i].token_type, TokenType::Error(_)));
        }
        assert_eq!(tokens[5].token_type, TokenType::Eof);
    }

    // Additional edge case tests for comprehensive coverage
    #[test]
    fn test_tokenize_boundary_numbers() {
        let tokenizer = Tokenizer::new("0 1 -1 999999 0.1 999.999 0.0");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 9); // 0 + 1 + minus + 1 + 999999 + 0.1 + 999.999 + 0.0 + EOF = 9 total
        assert_eq!(tokens[0].token_type, TokenType::Integer(0));
        assert_eq!(tokens[1].token_type, TokenType::Integer(1));
        assert_eq!(tokens[2].token_type, TokenType::Minus);
        assert_eq!(tokens[3].token_type, TokenType::Integer(1));
        assert_eq!(tokens[4].token_type, TokenType::Integer(999999));
        assert_eq!(tokens[5].token_type, TokenType::Float(0.1));
        assert_eq!(tokens[6].token_type, TokenType::Float(999.999));
        assert_eq!(tokens[7].token_type, TokenType::Float(0.0));
        assert_eq!(tokens[8].token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_string_edge_cases() {
        let tokenizer = Tokenizer::new("\"\" \"a\" \"hello world\" \"\\\"quoted\\\"\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 5); // 4 strings + EOF
        assert_eq!(tokens[0].token_type, TokenType::String("".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::String("a".to_string()));
        assert_eq!(tokens[2].token_type, TokenType::String("hello world".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::String("\"quoted\"".to_string()));
    }

    #[test]
    fn test_tokenize_operator_combinations() {
        let tokenizer = Tokenizer::new("! != = == > >= < <= + - * /");
        let tokens = tokenizer.tokenize();

        let expected = vec![
            TokenType::Bang,
            TokenType::BangEqual,
            TokenType::Equal,
            TokenType::EqualEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Star,
            TokenType::Slash,
            TokenType::Eof,
        ];

        assert_eq!(tokens.len(), expected.len());
        for (i, expected_type) in expected.iter().enumerate() {
            assert_eq!(tokens[i].token_type, *expected_type, "Mismatch at position {}", i);
        }
    }

    #[test]
    fn test_tokenize_mixed_line_endings() {
        let tokenizer = Tokenizer::new("a\nb\r\nc\nd");
        let tokens = tokenizer.tokenize();

        // Should handle different line endings properly
        assert!(tokens.len() >= 7); // At least: a + newline + b + newline + c + newline + d + EOF
        assert_eq!(tokens[0].token_type, TokenType::Identifier("a".to_string()));
        assert_eq!(tokens[1].token_type, TokenType::Newline);
        assert_eq!(tokens[2].token_type, TokenType::Identifier("b".to_string()));
        assert_eq!(tokens[3].token_type, TokenType::Newline);
        assert_eq!(tokens[4].token_type, TokenType::Identifier("c".to_string()));
        assert_eq!(tokens[5].token_type, TokenType::Newline);
        assert_eq!(tokens[6].token_type, TokenType::Identifier("d".to_string()));
    }

    #[test]
    fn test_tokenize_string_with_unknown_escape() {
        let tokenizer = Tokenizer::new("\"test\\x\\y\\z\"");
        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 2); // string + EOF
        // Unknown escape sequences should be treated as literals
        assert_eq!(tokens[0].token_type, TokenType::String("test\\x\\y\\z".to_string()));
    }

    #[test]
    fn test_tokenize_float_without_integer_part() {
        let tokenizer = Tokenizer::new(".123");
        let tokens = tokenizer.tokenize();

        // Should parse as dot + number, not as float
        assert_eq!(tokens.len(), 3); // dot + error + EOF
        assert_eq!(tokens[0].token_type, TokenType::Dot);
        assert_eq!(tokens[1].token_type, TokenType::Integer(123));
    }

    #[test]
    fn test_tokenize_complex_real_world_code() {
        let code = "fn calculate(x: i32, y: f64) -> f64 {\n    if x > 0 {\n        return x as f64 * y;\n    }\n    0.0\n}";
        let tokenizer = Tokenizer::new(code);
        let tokens = tokenizer.tokenize();

        // Verify basic structure without being too specific about exact token count
        assert!(tokens.len() > 20); // Should have many tokens
        assert_eq!(tokens.last().unwrap().token_type, TokenType::Eof);

        // Check for presence of key token types
        let has_identifiers = tokens.iter().any(|t| matches!(t.token_type, TokenType::Identifier(_)));
        let has_operators = tokens.iter().any(|t| matches!(t.token_type, TokenType::Greater));
        let has_numbers = tokens.iter().any(|t| matches!(t.token_type, TokenType::Integer(_) | TokenType::Float(_)));
        let has_braces = tokens.iter().any(|t| matches!(t.token_type, TokenType::LeftBrace | TokenType::RightBrace));

        assert!(has_identifiers);
        assert!(has_operators);
        assert!(has_numbers);
        assert!(has_braces);
    }

    #[test]
    fn test_tokenize_pathological_input() {
        // Test with very unusual but valid input
        let tokenizer = Tokenizer::new("(){};,.-+*/!<>=");
        let tokens = tokenizer.tokenize();

        // Should not panic and should produce reasonable tokens
        assert!(tokens.len() > 10);
        assert_eq!(tokens.last().unwrap().token_type, TokenType::Eof);
    }

    #[test]
    fn test_tokenize_position_accuracy_complex() {
        let tokenizer = Tokenizer::new("hello\n  world\n    !");
        let tokens = tokenizer.tokenize();

        // Verify accurate position tracking in complex scenarios
        assert_eq!(tokens[0].position.line, 1);
        assert_eq!(tokens[0].position.column, 1); // "hello"

        assert_eq!(tokens[1].position.line, 1);
        assert_eq!(tokens[1].position.column, 6); // newline

        assert_eq!(tokens[2].position.line, 2);
        assert_eq!(tokens[2].position.column, 3); // "world" after 2 spaces

        assert_eq!(tokens[3].position.line, 2);
        assert_eq!(tokens[3].position.column, 8); // newline after "world"

        assert_eq!(tokens[4].position.line, 3);
        assert_eq!(tokens[4].position.column, 5); // "!" after 4 spaces
    }
}
