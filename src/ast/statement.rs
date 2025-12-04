use crate::prelude::Mappable;
use crate::token::token_type::Identifier;
use super::expression::ExprNode;

use derive_more::{Deref, DerefMut};

#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
pub struct StatNode<T>{
    data:T,
    #[deref]
    #[deref_mut]
    pub stat:Statement<T>,
}

#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
pub struct StatementBlock<T> {
    pub statements: Vec<StatNode<T>>,
}

impl<T> StatementBlock<T> {
    pub fn new(statements: Vec<StatNode<T>>) -> Self {
        Self { statements }
    }
}

use enum_as_inner::EnumAsInner;

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
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
    Return(Option<ExprNode<T>>),
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
            Self::Return(x) =>Statement::Return(x.map(|x|x.inner_map(f)))
        }
    }
}

impl<T> From<StatementBlock<T>> for Statement<T> {
    fn from(v: StatementBlock<T>) -> Self {
        Self::StatementBlock(v)
    }
}

impl<T> Statement<T> {
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

    pub fn data(&self) -> &T {
        &self.data
    }
}
