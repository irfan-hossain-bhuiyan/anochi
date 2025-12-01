//! AST (Abstract Syntax Tree) module for the Anochi programming language.
//!
//! This module provides the data structures and functionality for representing
//! parsed source code as an abstract syntax tree. The AST serves as an intermediate
//! representation between the tokenized source code and the final compiled output.
//!
//! # Design
//!
//! The AST is designed around expressions as the primary building blocks:
//! - **Binary expressions**: Operations between two operands (e.g., `a + b`, `x * y`)
//! - **Unary expressions**: Operations on a single operand (e.g., `-x`, `!flag`)
//! - **Literal expressions**: Direct values (numbers, strings, identifiers)
//! - **Grouping expressions**: Parenthesized expressions for precedence control

use std::{collections::HashMap};

pub use crate::token::token_type::Identifier;
use crate::token::{Position};

pub mod literal;
pub mod operators;
pub mod expression;
pub mod statement;

pub use literal::Literal;
use macros::generate_unchecked;
pub use operators::{BinaryOperator, UnaryOperator};
pub use expression::{Expression, ExprNode};
pub use statement::{Statement, StatementBlock, StatNode};

pub type IdentifierMap<T> = HashMap<Identifier, T>;
pub type IdentifierToExp<T> = IdentifierMap<ExprNode<T>>;

#[derive(Clone, Debug, PartialEq)]
pub struct AstNode<T> {
    pub node: T,
    pub position: Option<Position>,
}
impl<T> AstNode<T> {
    pub fn new(node: T, position: Position) -> Self {
        Self {
            node,
            position: Some(position),
        }
    }
    pub fn new_temp(node: T) -> Self {
        Self {
            node,
            position: None,
        }
    }
}

impl<'a, T> From<T> for AstNode< T> {
    fn from(value: T) -> Self {
        AstNode::new_temp(value)
    }
}

    

pub type StatementNode=StatNode<Position>;
pub type StatmentBlockNode=StatementBlock<Position>;
pub type ExpressionNode=ExprNode<Position>;
