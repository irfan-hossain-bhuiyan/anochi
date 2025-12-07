use num_bigint::BigInt;
use num_rational::BigRational;
use crate::token::token_type::Identifier;

use enum_as_inner::EnumAsInner;
use derive_more::Display;
/// Represents different types of literal values in the AST.
///
/// Literals are the most basic expressions, representing direct values
/// from the source code such as numbers, strings, and identifiers.
#[derive(Debug, Clone, PartialEq, EnumAsInner, Display)]
pub enum Literal {
    Integer(BigInt),
    Float(BigRational),
    #[display("\"{_0}\"")]
    String(String),
    Identifier(Identifier),
    Bool(bool),
}
