use crate::prelude::Mappable;
use crate::token::token_type::Identifier;
use super::expression::ExprNodeGeneric;

use derive_more::{Deref, DerefMut};

#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
pub struct StatNodeGeneric<T> {
    pub data: T,
    #[deref]
    #[deref_mut]
    pub stat: StatementGeneric<T>,
}

#[derive(Debug, Clone, PartialEq, Deref, DerefMut)]
pub struct StatementBlockGeneric<T> {
    data: T,
    #[deref]
    #[deref_mut]
    pub statements: Vec<StatNodeGeneric<T>>,
}

impl<T> StatementBlockGeneric<T> {
    pub fn new(statements: Vec<StatNodeGeneric<T>>, data: T) -> Self {
        Self { data, statements }
    }

    pub fn data(&self) -> &T {
        &self.data
    }
}

use enum_as_inner::EnumAsInner;
use super::{ToStringTree, StringTree};

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum StatementGeneric<T> {
    /// Assignment for creating new variables with `let` keyword
    Assignment {
        target: Identifier,
        r#type: Option<ExprNodeGeneric<T>>,
        value: ExprNodeGeneric<T>,
    },
    /// Assignment for modifying existing objects/members
    MutableAssignment {
        target: ExprNodeGeneric<T>,
        value: ExprNodeGeneric<T>,
    },
    Statements(StatementBlockGeneric<T>),
    StatementBlock(StatementBlockGeneric<T>),
    If {
        condition: ExprNodeGeneric<T>,
        on_true: Box<StatNodeGeneric<T>>,
    },
    IfElse {
        condition: ExprNodeGeneric<T>,
        on_true: Box<StatNodeGeneric<T>>,
        on_false: Box<StatNodeGeneric<T>>,
    },
    Debug {
        expr_vec: Vec<ExprNodeGeneric<T>>,
    },
    Loop {
        statements: StatementBlockGeneric<T>,
    },
    Break,
    Continue,
    Return(Option<ExprNodeGeneric<T>>),
    Comptime {
        statements: StatementBlockGeneric<T>,
    },
    ForeignCall(String),
    /// Expression used as a statement (result is discarded)
    Expression(ExprNodeGeneric<T>),
}

// Mappable implementations for Statement and related types
impl<T,U> Mappable<T,U> for StatNodeGeneric<T> {
    type Mapped = StatNodeGeneric<U>;

    fn inner_map<F>(self, f:&mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U {
        Self::Mapped {
            data: f(self.data),
            stat: self.stat.inner_map(f),
        }
    }
}

impl<T,U> Mappable<T,U> for StatementBlockGeneric<T> {
    type Mapped = StatementBlockGeneric<U>;

    fn inner_map<F>(self, f:&mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U,
    {
        Self::Mapped {
            data: f(self.data),
            statements: self.statements.into_iter().map(|x| x.inner_map(f)).collect(),
        }
    }
}

impl<T,U> Mappable<T,U> for StatementGeneric<T> {
    type Mapped = StatementGeneric<U>;

    fn inner_map<F>(self,f:&mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U {
        match self {
            Self::Assignment { target, r#type, value } => StatementGeneric::Assignment {
                target,
                r#type: r#type.map(|t| t.inner_map(f)),
                value: value.inner_map(f),
            },
            Self::Statements(block) => StatementGeneric::Statements(block.inner_map(f)),
            Self::MutableAssignment { target, value } => StatementGeneric::MutableAssignment {
                target: target.inner_map(f),
                value: value.inner_map(f),
            },
            Self::StatementBlock(block) => StatementGeneric::StatementBlock(block.inner_map(f)),
            Self::If { condition, on_true } => StatementGeneric::If {
                condition: condition.inner_map(f),
                on_true: Box::new(on_true.inner_map(f)),
            },
            Self::IfElse { condition, on_true, on_false } => StatementGeneric::IfElse {
                condition: condition.inner_map(f),
                on_true: Box::new(on_true.inner_map(f)),
                on_false: Box::new(on_false.inner_map(f)),
            },
            Self::Debug { expr_vec } => StatementGeneric::Debug {
                expr_vec: expr_vec.into_iter().map(|x| x.inner_map(f)).collect(),
            },
            Self::Loop { statements } => StatementGeneric::Loop {
                statements: statements.inner_map(f),
            },
            Self::Break => StatementGeneric::Break,
            Self::Continue => StatementGeneric::Continue,
            Self::Return(x) =>StatementGeneric::Return(x.map(|x|x.inner_map(f))),
            Self::Comptime { statements } => StatementGeneric::Comptime {
                statements: statements.inner_map(f),
            },
            Self::ForeignCall(s) => StatementGeneric::ForeignCall(s),
            Self::Expression(expr) => StatementGeneric::Expression(expr.inner_map(f)),
        }
    }
}

impl<T> From<StatementBlockGeneric<T>> for StatementGeneric<T> {
    fn from(v: StatementBlockGeneric<T>) -> Self {
        Self::StatementBlock(v)
    }
}

impl<T> StatementGeneric<T> {
    /// Creates a new variable assignment with explicit type
    pub fn assignment_with_type(
        target: Identifier,
        r#type: impl Into<ExprNodeGeneric<T>>,
        value: impl Into<ExprNodeGeneric<T>>,
    ) -> Self {
        StatementGeneric::Assignment {
            target,
            r#type: Some(r#type.into()),
            value: value.into(),
        }
    }
    pub fn assignment(
        target: Identifier,
        r#type: Option<ExprNodeGeneric<T>>,
        value: impl Into<ExprNodeGeneric<T>>,
    ) -> Self {
        StatementGeneric::Assignment {
            target,
            r#type,
            value: value.into(),
        }
    }
    pub fn mutable_assignment(
        target: impl Into<ExprNodeGeneric<T>>,
        value: impl Into<ExprNodeGeneric<T>>,
    ) -> Self {
        StatementGeneric::MutableAssignment {
            target: target.into(),
            value: value.into(),
        }
    }

    pub fn assignment_no_type(
        identifier: Identifier,
        value: impl Into<ExprNodeGeneric<T>>,
    ) -> Self {
        StatementGeneric::Assignment {
            target: identifier,
            r#type: None,
            value: value.into(),
        }
    }

    pub fn statement_block(statements: Vec<StatNodeGeneric<T>>, data: T) -> Self {
        StatementGeneric::StatementBlock(StatementBlockGeneric::new(statements, data))
    }

    pub fn if_stmt(
        condition: impl Into<ExprNodeGeneric<T>>,
        on_true: impl Into<StatNodeGeneric<T>>,
    ) -> Self {
        StatementGeneric::If {
            condition: condition.into(),
            on_true: Box::new(on_true.into()),
        }
    }

    pub fn if_else(
        condition: impl Into<ExprNodeGeneric<T>>,
        on_true: impl Into<StatNodeGeneric<T>>,
        on_false: impl Into<StatNodeGeneric<T>>,
    ) -> Self {
        StatementGeneric::IfElse {
            condition: condition.into(),
            on_true: Box::new(on_true.into()),
            on_false: Box::new(on_false.into()),
        }
    }
    pub fn debug(expr_vec: Vec<ExprNodeGeneric<T>>) -> Self {
        Self::Debug { expr_vec }
    }



    pub fn to_node(self, data: T) -> StatNodeGeneric<T> {
        StatNodeGeneric {
            data,
            stat: self,
        }
    }
}

impl<T> StatNodeGeneric<T>{
    pub fn to_null(self)->StatNodeGeneric<()>{
        self.inner_map(&mut |_x|())
    }

    pub fn data(&self) -> &T {
        &self.data
    }
}

impl<T> ToStringTree for StatNodeGeneric<T> {
    fn to_string_tree(&self) -> StringTree {
        self.stat.to_string_tree()
    }
}

impl<T> ToStringTree for StatementBlockGeneric<T> {
    fn to_string_tree(&self) -> StringTree {
        if self.statements.is_empty() {
             return StringTree::leaf("EmptyBlock");
        }
        let children: Vec<StringTree> = self.statements.iter().map(|s| s.to_string_tree()).collect();
        StringTree::node("Block", children)
    }
}

impl<T> ToStringTree for StatementGeneric<T> {
    fn to_string_tree(&self) -> StringTree {
        match self {
            Self::Assignment { target, r#type, value } => {
                let mut children = Vec::new();
                if let Some(t) = r#type {
                    children.push(StringTree::node("Type:", vec![t.to_string_tree()]));
                }
                children.push(StringTree::node("Value:", vec![value.to_string_tree()]));
                StringTree::node(format!("Assignment({})", target), children)
            }
            Self::MutableAssignment { target, value } => {
                StringTree::node("MutableAssignment", vec![
                    StringTree::node("Target:", vec![target.to_string_tree()]),
                    StringTree::node("Value:", vec![value.to_string_tree()])
                ])
            }
             Self::Statements(block) | Self::StatementBlock(block) => {
                 block.to_string_tree()
             }
             Self::If { condition, on_true } => {
                 StringTree::node("If", vec![
                     StringTree::node("Condition:", vec![condition.to_string_tree()]),
                     StringTree::node("Then:", vec![on_true.to_string_tree()])
                 ])
             }
             Self::IfElse { condition, on_true, on_false } => {
                 StringTree::node("IfElse", vec![
                     StringTree::node("Condition:", vec![condition.to_string_tree()]),
                     StringTree::node("Then:", vec![on_true.to_string_tree()]),
                     StringTree::node("Else:", vec![on_false.to_string_tree()])
                 ])
             }
             Self::Debug { expr_vec } => {
                 let children = expr_vec.iter().map(|e| e.to_string_tree()).collect();
                 StringTree::node("Debug", children)
             }
             Self::Loop { statements } => {
                 StringTree::node("Loop", vec![statements.to_string_tree()])
             }
             Self::Break => StringTree::leaf("Break"),
             Self::Continue => StringTree::leaf("Continue"),
             Self::Return(opt_expr) => {
                 match opt_expr {
                     Some(expr) => StringTree::node("Return", vec![expr.to_string_tree()]),
                     None => StringTree::leaf("Return"),
                 }
             }
             Self::Comptime { statements } => {
                 StringTree::node("Comptime", vec![statements.to_string_tree()])
             }
             Self::ForeignCall(name) => StringTree::leaf(format!("ForeignCall({})", name)),
             Self::Expression(expr) => {
                 StringTree::node("ExpressionStmt", vec![expr.to_string_tree()])
             }
        }
    }
}
