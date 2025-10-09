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

use std::fmt;

pub type Identifier = String;

use crate::token::Position;
use crate::token::token_type::Keyword::{False, True};
use crate::token::token_type::TokenType;

/// Represents different types of literal values in the AST.
///
/// Literals are the most basic expressions, representing direct values
/// from the source code such as numbers, strings, and identifiers.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// Integer literal (e.g., `42`, `-123`)
    Integer(i64),
    /// Floating-point literal (e.g., `3.14`, `-2.5`)
    Float(f64),
    /// String literal (e.g., "hello", "world")
    String(String),
    /// Identifier (variable name, function name, etc.)
    Identifier(Identifier),
    Bool(bool),
}

/// Binary operators for expressions that operate on two operands.
///
/// These operators define the various arithmetic, comparison, and logical
/// operations that can be performed between two expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    // Arithmetic operators
    /// Addition operator (`+`)
    Plus,
    /// Subtraction operator (`-`)
    Minus,
    /// Multiplication operator (`*`)
    Multiply,
    /// Division operator (`/`)
    Divide,
    /// Modulo operator (`%`)
    Modulo,

    // Comparison operators
    /// Equal to operator (`==`)
    Equal,
    /// Not equal to operator (`!=`)
    NotEqual,
    /// Less than operator (`<`)
    Less,
    /// Less than or equal operator (`<=`)
    LessEqual,
    /// Greater than operator (`>`)
    Greater,
    /// Greater than or equal operator (`>=`)
    GreaterEqual,

    // Logical operators
    /// Logical AND operator (`&&`)
    And,
    /// Logical OR operator (`||`)
    Or,
}

/// Unary operators for expressions that operate on a single operand.
///
/// These operators define operations that can be applied to a single expression,
/// such as negation or logical NOT.
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    /// Arithmetic negation operator (`-`)
    Minus,
    /// Logical NOT operator (`!`)
    Not,
}

/// The main AST node type representing all possible expressions.
///
/// This enum encompasses all types of expressions that can appear in
/// Anochi source code, from simple literals to complex nested expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'a> {
    /// A literal value (number, string, identifier)
    Literal(Literal),

    /// A binary operation between two expressions
    Binary {
        /// The binary operator
        operator: BinaryOperator,
        /// Left operand of the binary operation
        left: Box<ExpressionNode<'a>>,
        /// Right operand of the binary operation
        right: Box<ExpressionNode<'a>>,
    },

    /// A unary operation on a single expression
    Unary {
        /// The unary operator
        operator: UnaryOperator,
        /// The operand of the unary operation
        operand: Box<ExpressionNode<'a>>,
    },

    /// A grouped expression (parentheses for precedence control)
    Grouping {
        /// The expression inside the parentheses
        expression: Box<ExpressionNode<'a>>,
    },
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{i}"),
            Literal::Float(fl) => write!(f, "{fl}"),
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Identifier(id) => write!(f, "{id}"),
            Literal::Bool(b) => write!(f, "{b}"),
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            BinaryOperator::Plus => "+",
            BinaryOperator::Minus => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Modulo => "%",
            BinaryOperator::Equal => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::Less => "<",
            BinaryOperator::LessEqual => "<=",
            BinaryOperator::Greater => ">",
            BinaryOperator::GreaterEqual => ">=",
            BinaryOperator::And => "&&",
            BinaryOperator::Or => "||",
        };
        write!(f, "{symbol}")
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            UnaryOperator::Minus => "-",
            UnaryOperator::Not => "!",
        };
        write!(f, "{symbol}")
    }
}

impl<'a> Expression<'a> {
    /// Creates a new literal integer expression.
    ///
    /// # Arguments
    ///
    /// * `value` - The integer value for the literal
    ///
    /// # Returns
    ///
    /// An `Expression::Literal` containing the integer value.
    pub fn integer(value: i64) -> Self {
        Expression::Literal(Literal::Integer(value))
    }

    /// Creates a new literal float expression.
    ///
    /// # Arguments
    ///
    /// * `value` - The float value for the literal
    ///
    /// # Returns
    ///
    /// An `Expression::Literal` containing the float value.
    pub fn float(value: f64) -> Self {
        Expression::Literal(Literal::Float(value))
    }
    pub fn bool(value: bool) -> Self {
        Self::Literal(Literal::Bool(value))
    }
    /// Creates a new literal string expression.
    ///
    /// # Arguments
    ///
    /// * `value` - The string value for the literal
    ///
    /// # Returns
    ///
    /// An `Expression::Literal` containing the string value.
    pub fn string(value: String) -> Self {
        Expression::Literal(Literal::String(value))
    }

    /// Creates a new identifier expression.
    ///
    /// # Arguments
    ///
    /// * `name` - The identifier name
    ///
    /// # Returns
    ///
    /// An `Expression::Literal` containing the identifier.
    pub fn identifier(name: Identifier) -> Self {
        Expression::Literal(Literal::Identifier(name))
    }

    /// Creates a new binary expression.
    ///
    /// # Arguments
    ///
    /// * `left` - The left operand expression
    /// * `operator` - The binary operator
    /// * `right` - The right operand expression
    ///
    /// # Returns
    ///
    /// An `Expression::Binary` representing the binary operation.
    pub fn binary(
        left: impl Into<ExpressionNode<'a>>,
        operator: BinaryOperator,
        right: impl Into<ExpressionNode<'a>>,
    ) -> Self {
        Expression::Binary {
            left: Box::new(left.into()),
            operator,
            right: Box::new(right.into()),
        }
    }

    /// Creates a new unary expression.
    ///
    /// # Arguments
    ///
    /// * `operator` - The unary operator
    /// * `operand` - The operand expression
    ///
    /// # Returns
    ///
    /// An `Expression::Unary` representing the unary operation.
    pub fn unary(operator: UnaryOperator, operand: impl Into<ExpressionNode<'a>>) -> Self {
        Self::Unary {
            operator,
            operand: Box::new(operand.into()),
        }
    }

    /// Creates a new grouping expression.
    ///
    /// # Arguments
    ///
    /// * `expression` - The expression to group with parentheses
    ///
    /// # Returns
    ///
    /// An `Expression::Grouping` representing the grouped expression.
    pub fn grouping(expression: impl Into<ExpressionNode<'a>>) -> Self {
        Expression::Grouping {
            expression: Box::new(expression.into()),
        }
    }
    /// Converts a Literal token into an Expression::Literal.
    pub fn from_literal(literal: Literal) -> Self {
        Expression::Literal(literal)
    }

    /// Converts a Token to an Expression if it is a literal token.
    pub fn from_token_type(token_type: TokenType) -> Option<Self> {
        use TokenType::{Float, Identifier, Integer, Keyword, String};
        match token_type {
            Integer(i) => Some(Expression::Literal(Literal::Integer(i))),
            Float(f) => Some(Expression::Literal(Literal::Float(f))),
            String(s) => Some(Expression::Literal(Literal::String(s))),
            Identifier(id) => Some(Expression::Literal(Literal::Identifier(id))),
            Keyword(True) => Some(Expression::Literal(Literal::Bool(true))),
            Keyword(False) => Some(Expression::Literal(Literal::Bool(false))),
            _ => None,
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'a> {
    Assignment {
        identifier: Identifier,
        value: ExpressionNode<'a>,
    },
    StatementBlock {
        statements: Vec<Statement<'a>>,
    },
    CallStatement {
        identifier: Identifier,
        argument: Vec<ExpressionNode<'a>>,
    },
}
pub type ExpressionNode<'a> = AstNode<'a,Expression<'a>>;

pub trait AstElement {}
impl AstElement for Expression<'_> {}
#[derive(Clone, Debug, PartialEq)]
pub struct AstNode<'a,T: AstElement> {
    pub node: T,
    pub position: Option<Position<'a>>,
}

impl<'a,T: AstElement> AstNode<'a,T> {
    pub fn new(node: T, position: Position<'a>) -> Self {
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
impl<'a,T: AstElement> From<T> for AstNode<'a,T> {
    fn from(value: T) -> Self {
        AstNode::new_temp(value)
    }
}
