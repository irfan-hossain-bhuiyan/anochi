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

use crate::ast::expression::{ExprNodeGeneric, ExpressionGeneric};
pub use crate::token::token_type::Identifier;
use crate::token::tokenizer::HasPosition;
use crate::token::{Position};
use crate::types::TypeId;

pub mod literal;
pub mod operators;
pub mod expression;
pub mod statement;

pub use literal::Literal;
pub use operators::{BinaryOperator, UnaryOperator};
pub use statement::{StatementGeneric, StatementBlockGeneric, StatNodeGeneric};

#[derive(Clone, Debug, PartialEq)]
pub enum StringTree {
    Leaf(String),
    Node(String, Vec<StringTree>),
}

impl StringTree {
    pub fn leaf(s: impl Into<String>) -> Self {
        Self::Leaf(s.into())
    }

    pub fn node(label: impl Into<String>, children: Vec<StringTree>) -> Self {
        Self::Node(label.into(), children)
    }
}

impl std::fmt::Display for StringTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_prefix(f, "")
    }
}

impl StringTree {
    fn fmt_with_prefix(&self, f: &mut std::fmt::Formatter<'_>, prefix: &str) -> std::fmt::Result {
        match self {
            Self::Leaf(s) => writeln!(f, "{}", s),
            Self::Node(label, children) => {
                writeln!(f, "{}", label)?;
                let count = children.len();
                for (i, child) in children.iter().enumerate() {
                    let is_last = i == count - 1;
                    let connector = if is_last { "└── " } else { "├── " };
                    let child_prefix = if is_last { "    " } else { "│   " };
                    write!(f, "{}{}", prefix, connector)?;
                    child.fmt_with_prefix(f, &(prefix.to_string() + child_prefix))?;
                }
                Ok(())
            }
        }
    }
}

pub trait ToStringTree {
    fn to_string_tree(&self) -> StringTree;
}


pub type IdentifierMap<T> = HashMap<Identifier, T>;
pub type IdentifierToExp<T> = IdentifierMap<ExprNodeGeneric<T>>;

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

    

#[derive(Clone, Debug, PartialEq,Default)]
pub struct CodeMetaData {
    pub pos: Position,
    pub type_data: Option<TypeId>,
}

impl CodeMetaData {
    pub fn new(pos: Position) -> Self {
        Self { 
            pos,
            type_data: None,
        }
    }

    pub fn update_type(&mut self, type_id: TypeId) -> Result<(), String> {
        if let Some(existing_type) = &self.type_data {
            if existing_type == &type_id {
                Ok(())
            } else {
                Err(format!("Type mismatch: expected {:?}, got {:?}", existing_type, type_id))
            }
        } else {
            self.type_data = Some(type_id);
            Ok(())
        }
    }
}

impl HasPosition for CodeMetaData {
    fn get_position(&self) -> &Position {
        &self.pos
    }
}

pub type ExpressionNode=ExprNodeGeneric<CodeMetaData>;
pub type Expression=ExpressionGeneric<CodeMetaData>;
pub type Statement=StatementGeneric<CodeMetaData>;
pub type StatementNode=StatNodeGeneric<CodeMetaData>;
pub type StatmentBlockNode=StatementBlockGeneric<CodeMetaData>;
