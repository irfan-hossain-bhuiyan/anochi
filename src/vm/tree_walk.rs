//! Virtual Machine module for the Anochi programming language.
//!
//! This module provides the tree-walking interpreter implementation that evaluates
//! AST expressions. The VM walks through the abstract syntax tree and computes
//! the results of expressions using a simple tree-walking approach.
//!
//! # Design
//!
//! The VM is designed to be stateless and simple:
//! - Takes an `Expression` and evaluates it recursively
//! - Handles arithmetic, comparison, and logical operations
//! - Supports type coercion between integers and floats
//! - Provides structured error handling for runtime errors
//!
//! # Example
//!

use std::collections::HashMap;

use crate::{
    ast::{
        BinaryOperator, Expression, ExpressionNode, Identifier, Literal, Statement, StatementNode,
        UnaryOperator,
    },
    vm::backend::{IoBackend, VmBackend},
};

use thiserror::Error;

/// Error types for VM evaluation.
#[derive(Error, Debug, PartialEq)]
pub enum VmError {
    /// Division by zero error
    #[error("Division by zero")]
    DivisionByZero,
    /// Type mismatch error
    #[error("Type mismatch: cannot perform operation on different types")]
    TypeMismatch,
    /// Undefined identifier error
    #[error("Undefined identifier: {0}")]
    UndefinedIdentifier(String),
    /// Invalid operation error
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
}

/// Result type for VM evaluation operations.
pub type VmResult = Result<Literal, VmError>;

/// Virtual Machine for evaluating expressions.
///
/// The VM provides a tree-walking interpreter that evaluates AST expressions
/// by recursively traversing the tree and computing results. It's designed to
/// be simple and stateless for basic expression evaluation.
///
/// # Example
///
///
#[derive(Debug, Default)]
pub struct Vm<Backend: VmBackend = IoBackend> {
    variable: HashMap<Identifier, Literal>,
    backend: Backend,
}
type ExpNode<'a> = ExpressionNode<'a>;
type StmtNode<'a> = StatementNode<'a>;
impl<Backend: VmBackend> Vm<Backend> {
    pub fn new(backend: Backend) -> Self {
        Self {
            variable: HashMap::new(),
            backend,
        }
    }
    /// Creates a new Virtual Machine instance.
    ///
    /// # Returns
    ///
    /// A new `Vm` instance ready to evaluate expressions.
    ///
    /// # Example
    ///
    ///
    /// Evaluates an expression using tree walking.
    ///
    /// This method recursively walks through the AST and evaluates expressions.
    /// Currently supports integer and float arithmetic operations, comparison
    /// operations, and logical operations. String operations and identifier
    /// resolution are not yet implemented.
    ///
    /// # Arguments
    ///
    /// * `expression` - The expression to evaluate
    ///
    /// # Returns
    ///
    /// A `Result` containing the evaluated `Literal` value, or a `VmError`.
    ///
    ///
    ///
    pub fn evaluate_expr(&self, expression_node: &ExpNode) -> VmResult {
        let expression = &expression_node.node;
        match expression {
            Expression::Literal(literal) => match literal {
                Literal::Identifier(x) => self
                    .variable
                    .get(x)
                    .cloned()
                    .ok_or(VmError::UndefinedIdentifier(format!("{x} doesn't exist."))),
                Literal::Bool(_) | Literal::Float(_) | Literal::String(_) | Literal::Integer(_) => {
                    Ok(literal.clone())
                }
            },

            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;

                self.evaluate_binary_operation(&left_val, &operator, &right_val)
            }

            Expression::Unary { operator, operand } => {
                let operand_val = self.evaluate_expr(operand)?;

                self.evaluate_unary_operation(&operator, &operand_val)
            }

            Expression::Grouping { expression } => self.evaluate_expr(expression),
        }
    }
    pub fn execute_statement(&mut self, stat_node: &StmtNode) -> Result<(), VmError> {
        let stmt = &stat_node.node;
        match stmt {
            Statement::Assignment { identifier, value } => {
                self.variable
                    .insert(identifier.clone(), self.evaluate_expr(value)?);
                Ok(())
            }
            Statement::StatementBlock { statements } => {
                for stmt in statements.iter() {
                    self.execute_statement(stmt)?;
                }
                Ok(())
            }
            Statement::If { condition, on_true } => {
                let Literal::Bool(x) = self.evaluate_expr(condition)? else {
                    return Err(VmError::TypeMismatch);
                };
                if x {
                    self.execute_statement(on_true)?;
                }
                Ok(())
            }
            Statement::IfElse {
                condition,
                on_true,
                on_false,
            } => {
                let Literal::Bool(x) = self.evaluate_expr(condition)? else {
                    return Err(VmError::TypeMismatch);
                };
                if x {
                    self.execute_statement(on_true)?;
                } else {
                    self.execute_statement(on_false)?;
                }
                Ok(())
            }
        }
    }

    /// Evaluates a binary operation between two literals.
    ///
    /// Handles arithmetic, comparison, and logical operations with type coercion
    /// between integers and floats when necessary.
    ///
    /// # Arguments
    ///
    /// * `left` - The left operand literal
    /// * `operator` - The binary operator
    /// * `right` - The right operand literal
    ///
    /// # Returns
    ///
    /// A `VmResult` containing the computed result or an error.
    fn evaluate_binary_operation(
        &self,
        left: &Literal,
        operator: &BinaryOperator,
        right: &Literal,
    ) -> VmResult {
        match (left, right) {
            // Bool operations
            (Literal::Bool(l), Literal::Bool(r)) => match operator {
                BinaryOperator::Equal => Ok(Literal::Bool(l == r)),
                BinaryOperator::NotEqual => Ok(Literal::Bool(l != r)),
                BinaryOperator::And => Ok(Literal::Bool(*l && *r)),
                BinaryOperator::Or => Ok(Literal::Bool(*l || *r)),
                BinaryOperator::Less => Ok(Literal::Bool(!*l && *r)), // false < true
                BinaryOperator::LessEqual => Ok(Literal::Bool(!*l || *r)), // false <= true, true <= true
                BinaryOperator::Greater => Ok(Literal::Bool(*l && !*r)),   // true > false
                BinaryOperator::GreaterEqual => Ok(Literal::Bool(*l || !*r)), // true >= false, true >= true
                _ => Err(VmError::InvalidOperation(format!(
                    "Cannot apply {operator:?} to Bool"
                ))),
            },
            // Integer operations
            (Literal::Integer(l), Literal::Integer(r)) => match operator {
                BinaryOperator::Plus => Ok(Literal::Integer(l + r)),
                BinaryOperator::Minus => Ok(Literal::Integer(l - r)),
                BinaryOperator::Multiply => Ok(Literal::Integer(l * r)),
                BinaryOperator::Divide => {
                    if *r == 0 {
                        Err(VmError::DivisionByZero)
                    } else {
                        Ok(Literal::Integer(l / r))
                    }
                }
                BinaryOperator::Modulo => {
                    if *r == 0 {
                        Err(VmError::DivisionByZero)
                    } else {
                        Ok(Literal::Integer(l % r))
                    }
                }
                BinaryOperator::Equal => Ok(Literal::Bool(l == r)),
                BinaryOperator::NotEqual => Ok(Literal::Bool(l != r)),
                BinaryOperator::Less => Ok(Literal::Bool(l < r)),
                BinaryOperator::LessEqual => Ok(Literal::Bool(l <= r)),
                BinaryOperator::Greater => Ok(Literal::Bool(l > r)),
                BinaryOperator::GreaterEqual => Ok(Literal::Bool(l >= r)),
                _ => Err(VmError::InvalidOperation(format!(
                    "Cannot apply {operator:?} to integer"
                ))),
            },

            // Float operations
            (Literal::Float(l), Literal::Float(r)) => match operator {
                BinaryOperator::Plus => Ok(Literal::Float(l + r)),
                BinaryOperator::Minus => Ok(Literal::Float(l - r)),
                BinaryOperator::Multiply => Ok(Literal::Float(l * r)),
                BinaryOperator::Divide => {
                    if *r == 0.0 {
                        Err(VmError::DivisionByZero)
                    } else {
                        Ok(Literal::Float(l / r))
                    }
                }
                BinaryOperator::Modulo => {
                    if *r == 0.0 {
                        Err(VmError::DivisionByZero)
                    } else {
                        Ok(Literal::Float(l % r))
                    }
                }
                BinaryOperator::Equal => Ok(Literal::Bool((l - r).abs() < f64::EPSILON)),
                BinaryOperator::NotEqual => Ok(Literal::Bool((l - r).abs() >= f64::EPSILON)),
                BinaryOperator::Less => Ok(Literal::Bool(l < r)),
                BinaryOperator::LessEqual => Ok(Literal::Bool(l <= r)),
                BinaryOperator::Greater => Ok(Literal::Bool(l > r)),
                BinaryOperator::GreaterEqual => Ok(Literal::Bool(l >= r)),
                _ => Err(VmError::InvalidOperation(format!(
                    "Cannot apply {operator:?} to integer"
                ))),
            },

            // Type mismatch for other combinations
            _ => Err(VmError::TypeMismatch),
        }
    }

    /// Evaluates a unary operation on a literal.
    ///
    /// Handles negation and logical NOT operations on numeric values.
    ///
    /// # Arguments
    ///
    /// * `operator` - The unary operator
    /// * `operand` - The operand literal
    ///
    /// # Returns
    ///
    /// A `VmResult` containing the computed result or an error.
    fn evaluate_unary_operation(&self, operator: &UnaryOperator, operand: &Literal) -> VmResult {
        match (operator, operand) {
            (UnaryOperator::Minus, Literal::Integer(i)) => Ok(Literal::Integer(-i)),
            (UnaryOperator::Minus, Literal::Float(f)) => Ok(Literal::Float(-f)),
            (UnaryOperator::Not, Literal::Bool(b)) => Ok(Literal::Bool(!b)),
            _ => Err(VmError::InvalidOperation(format!(
                "Cannot apply {operator:?} to {operand:?}",
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AstNode, BinaryOperator, Expression, UnaryOperator};

    #[test]
    fn test_vm_comprehensive_operations() {
        let vm: Vm = Vm::default();

        // Test integer literal
        let integer = AstNode::new_temp(Expression::integer(42));
        let result = vm.evaluate_expr(&integer).unwrap();
        assert_eq!(result, Literal::Integer(42));

        // Test binary expression: 5 + 3
        let expr = Expression::binary(Expression::integer(5), BinaryOperator::Plus, Expression::integer(3));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate_expr(&expr_node).unwrap();
        assert_eq!(result, Literal::Integer(8));

        // Test unary expression: -42
        let unary_expr = Expression::unary(UnaryOperator::Minus, Expression::integer(42));
        let unary_expr_node = AstNode::new_temp(unary_expr);
        let result = vm.evaluate_expr(&unary_expr_node).unwrap();
        assert_eq!(result, Literal::Integer(-42));
        
        // Test boolean operations: true && false
        let bool_expr = Expression::binary(Expression::bool(true), BinaryOperator::And, Expression::bool(false));
        let bool_node = AstNode::new_temp(bool_expr);
        let result = vm.evaluate_expr(&bool_node).unwrap();
        assert_eq!(result, Literal::Bool(false));
        
        // Test NOT operation: !true = false
        let not_expr = Expression::unary(UnaryOperator::Not, Expression::bool(true));
        let not_node = AstNode::new_temp(not_expr);
        let result = vm.evaluate_expr(&not_node).unwrap();
        assert_eq!(result, Literal::Bool(false));
        
        // Test comparison: 5 > 3
        let comp_expr = Expression::binary(Expression::integer(5), BinaryOperator::Greater, Expression::integer(3));
        let comp_node = AstNode::new_temp(comp_expr);
        let result = vm.evaluate_expr(&comp_node).unwrap();
        assert_eq!(result, Literal::Bool(true));

        // Test complex expression: (2 + 3) * 4 - 1 = 19
        let inner = Expression::binary(Expression::integer(2), BinaryOperator::Plus, Expression::integer(3));
        let grouped = Expression::grouping(inner);
        let multiplied = Expression::binary(grouped, BinaryOperator::Multiply, Expression::integer(4));
        let final_expr = Expression::binary(multiplied, BinaryOperator::Minus, Expression::integer(1));
        let final_expr_node = AstNode::new_temp(final_expr);
        let result = vm.evaluate_expr(&final_expr_node).unwrap();
        assert_eq!(result, Literal::Integer(19));
    }

    #[test]
    fn test_vm_error_handling() {
        let vm: Vm = Vm::default();

        // Test division by zero
        let expr = Expression::binary(Expression::integer(5), BinaryOperator::Divide, Expression::integer(0));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate_expr(&expr_node);
        assert_eq!(result, Err(VmError::DivisionByZero));

        // Test float division by zero
        let float_expr = Expression::binary(Expression::float(5.0), BinaryOperator::Divide, Expression::float(0.0));
        let float_expr_node = AstNode::new_temp(float_expr);
        let float_result = vm.evaluate_expr(&float_expr_node);
        assert_eq!(float_result, Err(VmError::DivisionByZero));
        
        // Test type mismatch: integer + float
        let type_mismatch_expr = Expression::binary(Expression::integer(5), BinaryOperator::Plus, Expression::float(2.5));
        let type_mismatch_node = AstNode::new_temp(type_mismatch_expr);
        let type_result = vm.evaluate_expr(&type_mismatch_node);
        assert_eq!(type_result, Err(VmError::TypeMismatch));
    }
}
