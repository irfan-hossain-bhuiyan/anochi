/// Represents the different types of tokens in the Anochi programming language
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

#[derive(Debug, Clone, PartialEq)]
pub enum CharType {
    Alpha,
    Numeric,
    Special,
    Whitespace,
    Other,
}

/// Represents a token with its type, lexeme, and position information
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize, column: usize) -> Self {
        Self {
            token_type,
            line,
            column,
        }
    }
}

/// Tokenizer that converts source code into a sequence of tokens
pub struct Tokenizer<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    current: usize,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    /// Creates a new tokenizer from source code
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            current: 0,
            line: 1,
            column: 1,
        }
    }

    /// Tokenizes the entire source code and returns a vector of tokens
/// Tokenizes the entire source code and returns a vector of tokens
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
                _ => {
                    // Skip unknown characters for now
                    self.advance();
                }
            }
        }
        
        // Add EOF token
        self.tokens.push(Token::new(TokenType::Eof, self.line, self.column));
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
    fn is_alpha(&self) -> bool {
        if let Some(ch) = self.peek() {
            ch.is_ascii_alphabetic() || ch == '_'
        } else {
            false
        }
    }

    fn is_numeric(&self) -> bool {
        if let Some(ch) = self.peek() {
            ch.is_ascii_digit()
        } else {
            false
        }
    }
    
/// Parses an identifier or keyword token
    fn parse_identifier_or_keyword(&mut self) -> Token {
        let start_line = self.line;
        let start_column = self.column;
        let mut identifier = String::new();

        // Collect all alphanumeric characters and underscores
        while let Some(ch) = self.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                identifier.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        if !identifier.is_empty() {
            Token::new(
                TokenType::Identifier(identifier),
                start_line,
                start_column,
            )
        } else {
            Token::new(
                TokenType::Error("Empty identifier".to_string()),
                start_line,
                start_column,
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
            } else if ch == '.' && !is_float && self.peek_next().map_or(false, |next| next.is_ascii_digit()) {
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
                start_line,
                start_column,
            );
        }

        if is_float {
            match number.parse::<f64>() {
                Ok(value) => Token::new(
                    TokenType::Float(value),
                    start_line,
                    start_column,
                ),
                Err(_) => Token::new(
                    TokenType::Error(format!("Invalid float: {}", number)),
                    start_line,
                    start_column,
                ),
            }
        } else {
            match number.parse::<i64>() {
                Ok(value) => Token::new(
                    TokenType::Integer(value),
                    start_line,
                    start_column,
                ),
                Err(_) => Token::new(
                    TokenType::Error(format!("Invalid integer: {}", number)),
                    start_line,
                    start_column,
                ),
            }
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
