use crate::{TokenType, code_error::CodeError, token::Position};
use derive_more::From;
use enum_dispatch::enum_dispatch;
use thiserror::Error;
#[derive(Debug, Error)]
#[error("Token is invalid.")]
pub struct NoTokenFound {
    pub token_found: Option<TokenType>,
}

impl NoTokenFound {
    pub fn new(token_found: TokenType) -> Self {
        Self {
            token_found: Some(token_found),
        }
    }
    pub const NONE_TOKEN: Self = NoTokenFound { token_found: None };

    /// Fast conversion to StatementParseError::NotFound
    pub fn into_statement_error(self) -> StatementParseErrorType{
        StatementParseErrorType::from(self)
    }

    pub fn into_parser_error_type(self) -> ParserErrorType {
        let stat_error:StatementParseErrorType=self.into_statement_error();
        ParserErrorType::from(stat_error)
    }

    pub fn into_parser_error(self, pos: Position) -> ParserError {
        CodeError::new(self.into_parser_error_type(), pos)
    }

    /// Fast conversion to ExpressionParseError::TokenNotFound
    pub fn into_expression_error(self) -> ExpressionParseErrorType{
        ExpressionParseErrorType::from(self)
    }
}
#[derive(Error, Debug)]
pub enum ExpressionParseErrorType {
    #[error("Unexpected token")]
    UnexpectedToken,
    #[error("Token not found in expression {0:?}")]
    TokenNotFound(#[from] NoTokenFound),
    #[error("No expression there to parse.")]
    NoExpression,
}
#[derive(Error, Debug)]
pub enum StatementParseErrorType {
    #[error("Token not found {0:?}")]
    TokenNotFound(#[from] NoTokenFound),
    #[error("No statement detected")]
    NoStatement,
    #[error("Break is outside of loop")]
    BreakOutsideLoop,
    #[error("Continue is outside of loop")]
    ContinueOutsideLoop,
}

impl StatementParseErrorType {
    pub fn with_pos(self, pos: Position) -> ParserError {
        ParserErrorType::from(self).with_pos(pos)
    }
}

#[enum_dispatch(CodeErrorType)]
#[derive(Debug, From)]
pub enum ParserErrorType {
    ExpressionParseErrorType,
    StatementParseErrorType,
}

impl ParserErrorType {
    pub const NO_STAT_FOUND: Self =
        ParserErrorType::StatementParseErrorType(StatementParseErrorType::NoStatement);
    pub const NO_EXPN_FOUND: Self =
        ParserErrorType::ExpressionParseErrorType(ExpressionParseErrorType::NoExpression);
    pub fn expected_token_in_statement(tokentype: TokenType) -> Self {
        ParserErrorType::StatementParseErrorType(StatementParseErrorType::TokenNotFound(
            NoTokenFound::new(tokentype),
        ))
    }
    pub fn expected_token_in_expression(tokentype: TokenType) -> Self {
        ParserErrorType::ExpressionParseErrorType(ExpressionParseErrorType::TokenNotFound(
            NoTokenFound::new(tokentype),
        ))
    }
    pub fn with_pos(self, pos: Position) -> ParserError {
        CodeError::new(self, pos)
    }
}
pub type ParserError = CodeError<ParserErrorType>;
impl crate::code_error::CodeErrorType for ExpressionParseErrorType {}
impl crate::code_error::CodeErrorType for StatementParseErrorType {}
impl crate::code_error::CodeErrorType for NoTokenFound {}
