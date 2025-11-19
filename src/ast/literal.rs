use std::fmt;
use num_bigint::BigInt;
use num_rational::BigRational;
use crate::token::token_type::Identifier;

/// Represents different types of literal values in the AST.
///
/// Literals are the most basic expressions, representing direct values
/// from the source code such as numbers, strings, and identifiers.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// Integer literal (e.g., `42`, `-123`)
    Integer(BigInt),
    /// Floating-point literal (e.g., `3.14`, `-2.5`)
    Float(BigRational),
    /// String literal (e.g., "hello", "world")
    String(String),
    /// Identifier (variable name, function name, etc.)
    Identifier(Identifier),
    Bool(bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{i}"),
            Literal::Float(fl) => write!(f, "{fl}"),
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Identifier(id) => write!(f, "{id}"),
            Literal::Bool(b) => write!(f, "{b}"),
        }
    }
}
