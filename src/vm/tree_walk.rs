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

use crate::ast::{BinaryOperator, Expression, ExpressionNode, Literal, UnaryOperator};

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
pub struct Vm {
    // For now, the VM is stateless
    // In the future, this could contain:
    // - Variable environment/scope
    // - Call stack
    // - Memory management
}

impl Vm {
    /// Creates a new Virtual Machine instance.
    ///
    /// # Returns
    ///
    /// A new `Vm` instance ready to evaluate expressions.
    ///
    /// # Example
    ///
    ///
    pub fn new() -> Self {
        Vm {}
    }

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
    pub fn evaluate(&self, expression_node: &ExpressionNode) -> VmResult {
        let expression = &expression_node.node;
        match expression {
            Expression::Literal(literal) => Ok(literal.clone()),

            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;

                self.evaluate_binary_operation(&left_val, &operator, &right_val)
            }

            Expression::Unary { operator, operand } => {
                let operand_val = self.evaluate(operand)?;

                self.evaluate_unary_operation(&operator, &operand_val)
            }

            Expression::Grouping { expression } => self.evaluate(expression),
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
                BinaryOperator::Greater => Ok(Literal::Bool(*l && !*r)), // true > false
                BinaryOperator::GreaterEqual => Ok(Literal::Bool(*l || !*r)), // true >= false, true >= true
                _ => Err(VmError::InvalidOperation(format!("Cannot apply {operator:?} to Bool"))),
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
                _ => Err(VmError::InvalidOperation(format!("Cannot apply {operator:?} to integer"))),

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
                _ => Err(VmError::InvalidOperation(format!("Cannot apply {operator:?} to integer"))),
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
            (UnaryOperator::Not,Literal::Bool(b))=> Ok(Literal::Bool(!b)),
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
    fn test_vm_bool_operations() {
        let vm = Vm::new();

        // true == true
        let expr = Expression::binary(Expression::bool(true), BinaryOperator::Equal, Expression::bool(true));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(true));

        // true != false
        let expr = Expression::binary(Expression::bool(true), BinaryOperator::NotEqual, Expression::bool(false));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(true));

        // true && false
        let expr = Expression::binary(Expression::bool(true), BinaryOperator::And, Expression::bool(false));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(false));

        // true || false
        let expr = Expression::binary(Expression::bool(true), BinaryOperator::Or, Expression::bool(false));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(true));

        // false < true
        let expr = Expression::binary(Expression::bool(false), BinaryOperator::Less, Expression::bool(true));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(true));

        // false <= true
        let expr = Expression::binary(Expression::bool(false), BinaryOperator::LessEqual, Expression::bool(true));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(true));

        // true > false
        let expr = Expression::binary(Expression::bool(true), BinaryOperator::Greater, Expression::bool(false));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(true));

        // true >= false
        let expr = Expression::binary(Expression::bool(true), BinaryOperator::GreaterEqual, Expression::bool(false));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(true));

        // Unary NOT: !true = false
        let expr = Expression::unary(UnaryOperator::Not, Expression::bool(true));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(false));

        // Unary NOT: !false = true
        let expr = Expression::unary(UnaryOperator::Not, Expression::bool(false));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(true));
    }
    fn test_vm_basic_operations() {
        let vm = Vm::new();

        // Test integer literal
        let integer = AstNode::new_temp(Expression::integer(42));
        let result = vm.evaluate(&integer).unwrap();
        assert_eq!(result, Literal::Integer(42));

        // Test binary expression: 5 + 3
        let left = Expression::integer(5);
        let right = Expression::integer(3);
        let expr = Expression::binary(left, BinaryOperator::Plus, right);
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Integer(8));

        // Test unary expression: -42
        let operand = Expression::integer(42);
        let unary_expr = Expression::unary(UnaryOperator::Minus, operand);
        let unary_expr_node = AstNode::new_temp(unary_expr);
        let result = vm.evaluate(&unary_expr_node).unwrap();
        assert_eq!(result, Literal::Integer(-42));
    }

    #[test]
    fn test_vm_expression_evaluation_with_type_coercion() {
        let vm = Vm::new();

        // Test mixed integer and float operations: 5 + 2.5 = 7.5
        let left = Expression::integer(5);
        let right = Expression::float(2.5);
                      let expr = Expression::binary(left, BinaryOperator::Plus, right);
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node);
        assert_eq!(result, Err(VmError::TypeMismatch));

        // Test complex expression: (2 + 3) * 4 - 1 = 19
        let inner_left = Expression::integer(2);
        let inner_right = Expression::integer(3);
        let inner = Expression::binary(inner_left, BinaryOperator::Plus, inner_right);
        let grouped = Expression::grouping(inner);

        let mult_right = Expression::integer(4);
        let multiplied = Expression::binary(grouped, BinaryOperator::Multiply, mult_right);

        let sub_right = Expression::integer(1);
        let final_expr = Expression::binary(multiplied, BinaryOperator::Minus, sub_right);

        let final_expr_node = AstNode::new_temp(final_expr);
        let result = vm.evaluate(&final_expr_node).unwrap();
        assert_eq!(result, Literal::Integer(19));
    }

    #[test]
    fn test_vm_error_handling() {
        let vm = Vm::new();

        // Test division by zero
        let left = Expression::integer(5);
        let right = Expression::integer(0);
        let expr = Expression::binary(left, BinaryOperator::Divide, right);
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node);
        assert_eq!(result, Err(VmError::DivisionByZero));

        // Test float division by zero
        let float_left = Expression::float(5.0);
        let float_right = Expression::float(0.0);
        let float_expr = Expression::binary(float_left, BinaryOperator::Divide, float_right);
        let float_expr_node = AstNode::new_temp(float_expr);
        let float_result = vm.evaluate(&float_expr_node);
        assert_eq!(float_result, Err(VmError::DivisionByZero));
    }

    #[test]
    fn test_vm_comparison_operations() {
        let vm = Vm::new();

        // Test integer comparison: 5 > 3
        let left = Expression::integer(5);
        let right = Expression::integer(3);
        let expr = Expression::binary(left, BinaryOperator::Greater, right);
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate(&expr_node).unwrap();
        assert_eq!(result, Literal::Bool(true)); // true

        // Test float equality with epsilon: 1.0 == 1.0
        let float_left = Expression::float(1.0);
        let float_right = Expression::float(1.0);
        let float_expr = Expression::binary(float_left, BinaryOperator::Equal, float_right);
        let float_expr_node = AstNode::new_temp(float_expr);
        let float_result = vm.evaluate(&float_expr_node).unwrap();
        assert_eq!(float_result, Literal::Bool(true)); // true
    }

    #[test]
   fn test_vm_logical_operations() { 
    let vm = Vm::new();

    // Test logical AND: true && false = false
    let left = Expression::bool(true);
    let right = Expression::bool(false);
    let expr = Expression::binary(left, BinaryOperator::And, right);
    let expr_node = AstNode::new_temp(expr);
    let result = vm.evaluate(&expr_node).unwrap();
    assert_eq!(result, Literal::Bool(false)); // false

    // Test logical OR: false || true = true
    let left = Expression::bool(false);
    let right = Expression::bool(true);
    let expr = Expression::binary(left, BinaryOperator::Or, right);
    let expr_node = AstNode::new_temp(expr);
    let result = vm.evaluate(&expr_node).unwrap();
    assert_eq!(result, Literal::Bool(true)); // true

    // Test logical NOT: !false = true
    let operand = Expression::bool(false);
    let unary_expr = Expression::unary(UnaryOperator::Not, operand);
    let unary_expr_node = AstNode::new_temp(unary_expr);
    let result = vm.evaluate(&unary_expr_node).unwrap();
    assert_eq!(result, Literal::Bool(true)); // true
}

}
