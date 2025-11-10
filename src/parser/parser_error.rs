use crate::TokenType;
use thiserror::Error;
#[derive(Debug, Error)]
#[error("Token is invalid.")]
pub struct NoTokenFound {
    pub token_found: Option<TokenType>,
}

impl NoTokenFound {
    pub fn new(token_found: TokenType) -> Self {
        Self { token_found:Some(token_found) }
    }
    pub const NONE_TOKEN:Self=NoTokenFound{token_found:None};

    /// Fast conversion to StatementParseError::NotFound
    pub fn into_statement_error(self) -> StatementParseError {
        StatementParseError::TokenNotFound(self)
    }

    /// Fast direct conversion to ParserError, bypassing StatementParseError
    pub fn into_parser_error(self) -> ParserError {
        ParserError::Stat(self.into_statement_error())
    }

    /// Fast conversion to ExpressionParseError::TokenNotFound
    pub fn into_expression_error(self) -> ExpressionParseError {
        ExpressionParseError::TokenNotFound(self)
    }
}
#[derive(Error, Debug)]
pub enum ExpressionParseError {
    #[error("Unexpected token")]
    UnexpectedToken,
    #[error("Token not found in expression {0:?}")]
    TokenNotFound(#[from] NoTokenFound),
    #[error("No expression there to parse.")]
    NoExpression,
}

impl ExpressionParseError {
    /// Fast conversion to ParserError::Expr
    pub fn into_parser_error(self) -> ParserError {
        ParserError::Expr(self)
    }
}
#[derive(Error, Debug)]
pub enum StatementParseError {
    #[error("Token not found {0:?}")]
    TokenNotFound(#[from] NoTokenFound),
    #[error("No statement detected")]
    NoStatement,
    #[error("Break is outside of loop")]
    BreakOutsideLoop,
    #[error("Continue is outside of loop")]
    ContinueOutsideLoop
}
impl StatementParseError {
    /// Fast conversion to ParserError::Stat
    pub fn into_parser_error(self) -> ParserError {
        ParserError::Stat(self)
    }
}
#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Expression can't be evaluated: {0}")]
    Expr(#[from] ExpressionParseError),
    #[error("Statement can't be evaluated: {0}")]
    Stat(#[from] StatementParseError),
}

impl ParserError {
    pub const NO_STAT_FOUND: Self = Self::Stat(StatementParseError::NoStatement);
    pub const NO_EXPN_FOUND: Self = Self::Expr(ExpressionParseError::NoExpression);
    pub fn expected_token_in_statement(tokentype:TokenType)->Self{
        Self::Stat(StatementParseError::TokenNotFound(NoTokenFound::new(tokentype) ))
    }
    pub fn expected_token_in_expression(tokentype:TokenType)->Self{
        Self::Expr(ExpressionParseError::TokenNotFound(NoTokenFound::new(tokentype)))
    }
}
