use num_bigint::BigInt;
use num_rational::BigRational;
use crate::prelude::Mappable;
use crate::token::token_type::{Identifier, TokenType};
use crate::token::token_type::Keyword::{False, True};
use super::literal::Literal;
use super::operators::{BinaryOperator, UnaryOperator};
use super::{IdentifierToExp, StatNodeGeneric};

use derive_more::{Deref, DerefMut, From};

#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
pub struct ExprNodeGeneric<T>{
    data:T,
    #[deref]
    #[deref_mut]
    pub exp:ExpressionGeneric<T>,
}

impl<T,U> Mappable<T,U> for ExprNodeGeneric<T> {
    type Mapped = ExprNodeGeneric<U>;

    fn inner_map<F>(self, f:&mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U {
            Self::Mapped {
                data: f(self.data),
                exp: self.exp.inner_map(f),
            }
    }
}

impl<T> ExprNodeGeneric<T>{
    pub fn to_null(self)->ExprNodeGeneric<()>{
        self.inner_map(&mut |_x|())
    }

    pub fn data(&self) -> &T {
        &self.data
    }
}

use enum_as_inner::EnumAsInner;

/// The main AST node type representing all possible expressions.
///
/// This enum encompasses all types of expressions that can appear in
/// Anochi source code, from simple literals to complex nested expressions.
#[derive(Debug, Clone, PartialEq, EnumAsInner, From)]
pub enum ExpressionGeneric<T> {
    /// A literal value (number, string, identifier)
    Literal(Literal),

    /// A binary operation between two expressions
    Binary {
        /// The binary operator
        operator: BinaryOperator,
        /// Left operand of the binary operation
        left: Box<ExprNodeGeneric<T>>,
        /// Right operand of the binary operation
        right: Box<ExprNodeGeneric<T>>,
    },

    /// A unary operation on a single expression
    Unary {
        /// The unary operator
        operator: UnaryOperator,
        /// The operand of the unary operation
        operand: Box<ExprNodeGeneric<T>>,
    },

    /// A grouped expression (parentheses for precedence control)
    Grouping {
        /// The expression inside the parentheses
        expression: Box<ExprNodeGeneric<T>>,
    },

    /// A member access expression (e.g., `a.x`, `{x=10}.y`)
    MemberAccess {
        /// The object being accessed (left side of the dot)
        object: Box<ExprNodeGeneric<T>>,
        /// The member being accessed (right side of the dot)
        member: Identifier,
    },

    Product {
        data: IdentifierToExp<T>,
    },

    Sum {
        data: Vec<ExprNodeGeneric<T>>,
    },
    Function {
        input: Box<ExprNodeGeneric<T>>,
        output: Option<Box<ExprNodeGeneric<T>>>,
        statements: Box<StatNodeGeneric<T>>,
    },
    FnCall {
        caller:Box<ExprNodeGeneric<T>>,
        callee:Box<ExprNodeGeneric<T>>,
    }
}

impl<T,U> Mappable<T,U> for ExpressionGeneric<T> {
    type Mapped = ExpressionGeneric<U>;
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
            Self::FnCall { caller, callee }=>{
                let mapped_caller = caller.inner_map(f);
                let mapped_callee = callee.inner_map(f);
                Self::Mapped::FnCall {
                    caller: Box::new(mapped_caller),
                    callee: Box::new(mapped_callee),
                }
            }
        }
    }
}

impl<T> ExpressionGeneric<T> {
    /// Creates a new literal integer expression.
    pub fn from_i64(value: i64) -> Self {
        ExpressionGeneric::Literal(Literal::Integer(value.into()))
    }
    pub fn from_f64(value: f64) -> Self {
        ExpressionGeneric::Literal(Literal::Float(BigRational::from_float(value).unwrap()))
    }
    pub fn from_bool(value: bool) -> Self {
        ExpressionGeneric::Literal(Literal::Bool(value))
    }
    pub fn integer(value: BigInt) -> Self {
        ExpressionGeneric::Literal(Literal::Integer(value))
    }
    pub fn product(value: IdentifierToExp<T>) -> Self {
        ExpressionGeneric::Product { data: value }
    }
    pub fn sum(value: Vec<ExprNodeGeneric<T>>) -> Self {
        ExpressionGeneric::Sum { data: value }
    }
    pub fn float(value: BigRational) -> Self {
        ExpressionGeneric::Literal(Literal::Float(value))
    }
    pub fn bool(value: bool) -> Self {
        Self::Literal(Literal::Bool(value))
    }
    pub fn string(value: String) -> Self {
        ExpressionGeneric::Literal(Literal::String(value))
    }
    pub fn identifier(name: Identifier) -> Self {
        ExpressionGeneric::Literal(Literal::Identifier(name))
    }
    pub fn fn_call(caller: ExprNodeGeneric<T>, callee:ExprNodeGeneric<T>) -> Self {
        ExpressionGeneric::FnCall {
            caller: Box::new(caller.into()),
            callee:Box::new(callee.into()),
        }
    }
    pub fn binary(
        left: impl Into<ExprNodeGeneric<T>>,
        operator: BinaryOperator,
        right: impl Into<ExprNodeGeneric<T>>,
    ) -> Self {
        ExpressionGeneric::Binary {
            left: Box::new(left.into()),
            operator,
            right: Box::new(right.into()),
        }
    }

    /// Creates a new unary expression.
    pub fn unary(operator: UnaryOperator, operand: impl Into<ExprNodeGeneric<T>>) -> Self {
        Self::Unary {
            operator,
            operand: Box::new(operand.into()),
        }
    }

    /// Creates a new grouping expression.
    pub fn grouping(expression: impl Into<ExprNodeGeneric<T>>) -> Self {
        ExpressionGeneric::Grouping {
            expression: Box::new(expression.into()),
        }
    }

    /// Creates a new member access expression.
    pub fn member_access(object: impl Into<ExprNodeGeneric<T>>, member: Identifier) -> Self {
        ExpressionGeneric::MemberAccess {
            object: Box::new(object.into()),
            member,
        }
    }

    /// Converts a Literal token into an Expression::Literal.
    pub fn from_literal(literal: Literal) -> Self {
        literal.into()
    }

    /// Returns a Token to an Expression if it is a literal token.
    pub fn from_token_type(token_type: TokenType) -> Option<Self> {
        match token_type {
            TokenType::Integer(i) => Some(ExpressionGeneric::Literal(Literal::Integer(i))),
            TokenType::Float(f) => Some(ExpressionGeneric::Literal(Literal::Float(f))),
            TokenType::String(s) => Some(ExpressionGeneric::Literal(Literal::String(s))),
            TokenType::Identifier(id) => Some(ExpressionGeneric::Literal(Literal::Identifier(id))),
            TokenType::Keyword(True) => Some(ExpressionGeneric::Literal(Literal::Bool(true))),
            TokenType::Keyword(False) => Some(ExpressionGeneric::Literal(Literal::Bool(false))),
            _ => None,
        }
    }

    pub fn to_node(self, data: T) -> ExprNodeGeneric<T> {
        ExprNodeGeneric {
            data,
            exp: self,
        }
    }

}
