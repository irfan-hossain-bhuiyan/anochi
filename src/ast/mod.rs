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
/// - **Grouping expressions**: Parenthesized expressions for precedence control
use std::collections::HashMap;
use std::fmt::{self};

use crate::prelude::Mappable;
pub use crate::token::token_type::Identifier;
use crate::vm::tree_walk::Vm;
pub type IdentifierMap<T> = HashMap<Identifier, T>;
//pub type IdentifierToValue = IdentifierMap<VmValue>;
pub type IdentifierToExp<T> = IdentifierMap<ExprNode<T>>;
use num_bigint::BigInt;
use num_rational::BigRational;

use crate::token::{Position, TokenSlice};
use crate::token::token_type::Keyword::{False, True};
use crate::token::token_type::TokenType;
//use crate::vm::tree_walk::VmValue;

/// Represents different types of literal values in the AST.
///
/// Literals are the most basic expressions, representing direct values
/// from the source code such as numbers, strings, and identifiers.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// Integer literal (e.g., `42`, `-123`)
    Integer(num_bigint::BigInt),
    /// Floating-point literal (e.g., `3.14`, `-2.5`)
    Float(num_rational::BigRational),
    /// String literal (e.g., "hello", "world")
    String(String),
    /// Identifier (variable name, function name, etc.)
    Identifier(Identifier),
    Bool(bool),
}

// Mappable implementations for Statement and related types
impl<T,U> Mappable<T,U> for StatNode<T> {
    type Mapped = StatNode<U>;

    fn inner_map<F>(self, f:&mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U {
        Self::Mapped {
            data: f(self.data),
            stat: self.stat.inner_map(f),
        }
    }
}

impl<T,U> Mappable<T,U> for StatementBlock<T> {
    type Mapped = StatementBlock<U>;

    fn inner_map<F>(self, f:&mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U,
    {
        Self::Mapped {
            statements: self.statements.into_iter().map(|x| x.inner_map(f)).collect(),
        }
    }
}

impl<T,U> Mappable<T,U> for Statement<T> {
    type Mapped = Statement<U>;

    fn inner_map<F>(self,f:&mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U {
        match self {
            Self::Assignment { target, r#type, value } => Statement::Assignment {
                target,
                r#type: r#type.map(|t| t.inner_map(f)),
                value: value.inner_map(f),
            },
            Self::Statements(block) => Statement::Statements(block.inner_map(f)),
            Self::MutableAssignment { target, value } => Statement::MutableAssignment {
                target: target.inner_map(f),
                value: value.inner_map(f),
            },
            Self::StatementBlock(block) => Statement::StatementBlock(block.inner_map(f)),
            Self::If { condition, on_true } => Statement::If {
                condition: condition.inner_map(f),
                on_true: Box::new(on_true.inner_map(f)),
            },
            Self::IfElse { condition, on_true, on_false } => Statement::IfElse {
                condition: condition.inner_map(f),
                on_true: Box::new(on_true.inner_map(f)),
                on_false: Box::new(on_false.inner_map(f)),
            },
            Self::Debug { expr_vec } => Statement::Debug {
                expr_vec: expr_vec.into_iter().map(|x| x.inner_map(f)).collect(),
            },
            Self::Loop { statements } => Statement::Loop {
                statements: statements.inner_map(f),
            },
            Self::Break => Statement::Break,
            Self::Continue => Statement::Continue,
        }
    }
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
    And,
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

    Not,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ExprNode<T>{
    data:T,
    pub exp:Expression<T>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct StatNode<T>{
    data:T,
    pub stat:Statement<T>,
}
impl<T,U> Mappable<T,U> for ExprNode<T> {
    type Mapped = ExprNode<U>;

    fn inner_map<F>(self, f:&mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U {
            Self::Mapped {
                data: f(self.data),
                exp: self.exp.inner_map(f),
            }
    }
}
impl<T,U> Mappable<T,U> for Expression<T> {
    type Mapped = Expression<U>;
    fn inner_map<F>(self, f:&mut F) -> Self::Mapped
        where
            F: FnMut(T) -> U {
        match self {
            Self::Literal(literal) => Self::Mapped::Literal(literal),
            Self::Binary { operator, left, right } => {
                let mapped_left = left.inner_map(f);
                let mapped_right = right.inner_map(f);
                Self::Mapped::Binary {
                    operator,
                    left: Box::new(mapped_left),
                    right: Box::new(mapped_right),
                }
            },
            Self::Unary { operator, operand } => {
                let mapped_operand = operand.inner_map(f);
                Self::Mapped::Unary {
                    operator,
                    operand: Box::new(mapped_operand),
                }
            },
            Self::Grouping { expression } => {
                let mapped_expression = expression.inner_map(f);
                Self::Mapped::Grouping {
                    expression: Box::new(mapped_expression),
                }
            },
            Self::MemberAccess { object, member } => {
                let mapped_object = object.inner_map(f);
                Self::Mapped::MemberAccess {
                    object: Box::new(mapped_object),
                    member,
                }
            },
            Self::Product { data } => Self::Mapped::Product {
                data: data.into_iter().map(|(id, expr_node)| (id, expr_node.inner_map(f))).collect(),
            },
            Self::Sum { data } => Self::Mapped::Sum {
                data: data.into_iter().map(|expr_node| expr_node.inner_map(f)).collect(),
            },
            Self::Function { input, output, statements } => {
                let mapped_input = input.inner_map(f);
                let mapped_output = output.map(|out| Box::new(out.inner_map(f)));
                let mapped_statements = statements.inner_map(f);
                Self::Mapped::Function {
                    input: Box::new(mapped_input),
                    output: mapped_output,
                    statements: Box::new(mapped_statements),
                }
            },
        }
    }
}
/// The main AST node type representing all possible expressions.
///
/// This enum encompasses all types of expressions that can appear in
/// Anochi source code, from simple literals to complex nested expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression<T> {
    /// A literal value (number, string, identifier)
    Literal(Literal),

    /// A binary operation between two expressions
    Binary {
        /// The binary operator
        operator: BinaryOperator,
        /// Left operand of the binary operation
        left: Box<ExprNode<T>>,
        /// Right operand of the binary operation
        right: Box<ExprNode<T>>,
    },

    /// A unary operation on a single expression
    Unary {
        /// The unary operator
        operator: UnaryOperator,
        /// The operand of the unary operation
        operand: Box<ExprNode<T>>,
    },

    /// A grouped expression (parentheses for precedence control)
    Grouping {
        /// The expression inside the parentheses
        expression: Box<ExprNode<T>>,
    },

    /// A member access expression (e.g., `a.x`, `{x=10}.y`)
    MemberAccess {
        /// The object being accessed (left side of the dot)
        object: Box<ExprNode<T>>,
        /// The member being accessed (right side of the dot)
        member: Identifier,
    },

    Product {
        data: IdentifierToExp<T>,
    },

    Sum {
        data: Vec<ExprNode<T>>,
    },
    Function {
        input: Box<ExprNode<T>>,
        output: Option<Box<ExprNode<T>>>,
        statements: Box<StatNode<T>>,
    }
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

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self,f:&mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            UnaryOperator::Minus => "-",
            UnaryOperator::Not => "not",
        };
        write!(f, "{symbol}")
    }
}

impl<T> Expression<T> {
    /// Creates a new literal integer expression.
    ///
    /// # Arguments
    ///
    /// * `value` - The integer value for the literal
    ///
    /// # Returns
    ///
    /// An `Expression::Literal` containing the integer value.
    pub fn from_i64(value: i64) -> Self {
        Expression::Literal(Literal::Integer(value.into()))
    }
    pub fn from_f64(value: f64) -> Self {
        Expression::Literal(Literal::Float(BigRational::from_float(value).unwrap()))
    }
    pub fn from_bool(value: bool) -> Self {
        Expression::Literal(Literal::Bool(value))
    }
    pub fn integer(value: BigInt) -> Self {
        Expression::Literal(Literal::Integer(value))
    }
    pub fn product(value: IdentifierToExp<T>) -> Self {
        Expression::Product { data: value }
    }
    pub fn sum(value: Vec<ExprNode<T>>) -> Self {
        Expression::Sum { data: value }
    }
    pub fn float(value: BigRational) -> Self {
        Expression::Literal(Literal::Float(value))
    }
    pub fn bool(value: bool) -> Self {
        Self::Literal(Literal::Bool(value))
    }
    pub fn string(value: String) -> Self {
        Expression::Literal(Literal::String(value))
    }
    pub fn identifier(name: Identifier) -> Self {
        Expression::Literal(Literal::Identifier(name))
    }

    pub fn binary(
        left: impl Into<ExprNode<T>>,
        operator: BinaryOperator,
        right: impl Into<ExprNode<T>>,
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
    pub fn unary(operator: UnaryOperator, operand: impl Into<ExprNode<T>>) -> Self {
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
    pub fn grouping(expression: impl Into<ExprNode<T>>) -> Self {
        Expression::Grouping {
            expression: Box::new(expression.into()),
        }
    }

    /// Creates a new member access expression.
    ///
    /// # Arguments
    ///
    /// * `object` - The object being accessed
    /// * `member` - The member identifier to access
    ///
    /// # Returns
    ///
    /// An `Expression::MemberAccess` representing the member access operation.
    pub fn member_access(object: impl Into<ExprNode<T>>, member: Identifier) -> Self {
        Expression::MemberAccess {
            object: Box::new(object.into()),
            member,
        }
    }

    /// Converts a Literal token into an Expression::Literal.
    pub fn from_literal(literal: Literal) -> Self {
        Expression::Literal(literal)
    }

    /// Returns a Token to an Expression if it is a literal token.
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

    pub fn to_node(self, data: T) -> ExprNode<T> {
        ExprNode {
            data,
            exp: self,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StatementBlock<T> {
    pub statements: Vec<StatNode<T>>,
}

impl<T> StatementBlock<T> {
    pub fn new(statements: Vec<StatNode<T>>) -> Self {
        Self { statements }
    }
}



#[derive(Debug, Clone, PartialEq)]
pub enum Statement<T> {
    /// Assignment for creating new variables with `let` keyword
    Assignment {
        target: Identifier,
        r#type: Option<ExprNode<T>>,
        value: ExprNode<T>,
    },
    /// Assignment for modifying existing objects/members
    MutableAssignment {
        target: ExprNode<T>,
        value: ExprNode<T>,
    },
    Statements(StatementBlock<T>),
    StatementBlock(StatementBlock<T>),
    If {
        condition: ExprNode<T>,
        on_true: Box<StatNode<T>>,
    },
    IfElse {
        condition: ExprNode<T>,
        on_true: Box<StatNode<T>>,
        on_false: Box<StatNode<T>>,
    },
    Debug {
        expr_vec: Vec<ExprNode<T>>,
    },
    Loop {
        statements: StatementBlock<T>,
    },
    Break,
    Continue,
}

impl<T> From<StatementBlock<T>> for Statement<T> {
    fn from(v: StatementBlock<T>) -> Self {
        Self::StatementBlock(v)
    }
}
#[derive(Clone, Debug, PartialEq)]
pub struct AstNode<'a, T> {
    pub node: T,
    pub position: Option<Position<'a>>,
}
impl<'a, T> AstNode<'a, T> {
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

impl<'a, T> From<T> for AstNode<'a, T> {
    fn from(value: T) -> Self {
        AstNode::new_temp(value)
    }
}
impl<T> Statement<T> {
    /// Creates a new variable assignment with let keyword (literal value)
    //pub fn assignment(target: Identifier, value: Literal) -> Self {
    //    Statement::Assignment {
    //        target,
    //        r#type: None,
    //        value,
    //    }
    //}
    /// Creates a new variable assignment with explicit type
    pub fn assignment_with_type(
        target: Identifier,
        r#type: impl Into<ExprNode<T>>,
        value: impl Into<ExprNode<T>>,
    ) -> Self {
        Statement::Assignment {
            target,
            r#type: Some(r#type.into()),
            value: value.into(),
        }
    }
    pub fn assignment(
        target: Identifier,
        r#type: Option<ExprNode<T>>,
        value: impl Into<ExprNode<T>>,
    ) -> Self {
        Statement::Assignment {
            target,
            r#type,
            value: value.into(),
        }
    }
    pub fn mutable_assignment(
        target: impl Into<ExprNode<T>>,
        value: impl Into<ExprNode<T>>,
    ) -> Self {
        Statement::MutableAssignment {
            target: target.into(),
            value: value.into(),
        }
    }

    pub fn assignment_no_type(
        identifier: Identifier,
        value: impl Into<ExprNode<T>>,
    ) -> Self {
        Statement::Assignment {
            target: identifier,
            r#type: None,
            value: value.into(),
        }
    }

    pub fn statement_block(statements: Vec<StatNode<T>>) -> Self {
        Statement::StatementBlock(StatementBlock { statements })
    }

    pub fn if_stmt(
        condition: impl Into<ExprNode<T>>,
        on_true: impl Into<StatNode<T>>,
    ) -> Self {
        Statement::If {
            condition: condition.into(),
            on_true: Box::new(on_true.into()),
        }
    }

    pub fn if_else(
        condition: impl Into<ExprNode<T>>,
        on_true: impl Into<StatNode<T>>,
        on_false: impl Into<StatNode<T>>,
    ) -> Self {
        Statement::IfElse {
            condition: condition.into(),
            on_true: Box::new(on_true.into()),
            on_false: Box::new(on_false.into()),
        }
    }
    pub fn debug(expr_vec: Vec<ExprNode<T>>) -> Self {
        Self::Debug { expr_vec }
    }

    /// Returns `true` if the statement is [`Break`].
    ///
    /// [`Break`]: Statement::Break
    #[must_use]
    pub fn is_break(&self) -> bool {
        matches!(self, Self::Break)
    }

    /// Returns `true` if the statement is [`Continue`].
    ///
    /// [`Continue`]: Statement::Continue
    #[must_use]
    pub fn is_continue(&self) -> bool {
        matches!(self, Self::Continue)
    }

    pub fn to_node(self, data: T) -> StatNode<T> {
        StatNode {
            data,
            stat: self,
        }
    }
}
impl<T> StatNode<T>{
    pub fn to_null(self)->StatNode<()>{
        self.inner_map(&mut |_x|())
    }
}
impl<T> ExprNode<T>{
    pub fn to_null(self)->ExprNode<()>{
        self.inner_map(&mut |_x|())
    }
}
pub type StatementNode<'a>=StatNode<&'a TokenSlice<'a>>;
pub type StatmentBlockNode<'a>=StatementBlock<&'a TokenSlice<'a>>;
pub type ExpressionNode<'a>=ExprNode<&'a TokenSlice<'a>>;
