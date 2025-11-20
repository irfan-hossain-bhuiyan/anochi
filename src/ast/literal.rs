use std::fmt;
use num_bigint::BigInt;
use num_rational::BigRational;
use crate::token::token_type::Identifier;

use enum_as_inner::EnumAsInner;
use derive_more::{Display, From};
/// Represents different types of literal values in the AST.
///
/// Literals are the most basic expressions, representing direct values
/// from the source code such as numbers, strings, and identifiers.
#[derive(Debug, Clone, PartialEq, EnumAsInner, Display, From)]
pub enum Literal {
    /// Integer literal (e.g., `42`, `-123`)
    Integer(BigInt),
    /// Floating-point literal (e.g., `3.14`, `-2.5`)
    Float(BigRational),
    /// String literal (e.g., "hello", "world")
    #[display("\"{_0}\"")]
    String(String),
    /// Identifier (variable name, function name, etc.)
    Identifier(Identifier),
    Bool(bool),
}
