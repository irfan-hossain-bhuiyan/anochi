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

use std::{collections::HashMap, fmt::Display};
mod vm_value_operation;
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
#[derive(Debug,Clone,PartialEq)]
pub enum VmValue{
    Literal(Literal),
    Product(HashMap<Identifier,VmValue>),
}
impl Display for VmValue{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            Self::Literal(x)=>Display::fmt(x, f),
            Self::Product(x)=>write!(f,"Product{x:?}"),
        }
    }
}

impl From<Literal> for VmValue {
    fn from(v: Literal) -> Self {
        Self::Literal(v)
    }
}
/// Result type for VM evaluation operations.
pub type VmResult = Result<VmValue, VmError>;
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
pub struct Vm<Backend= IoBackend> {
    variable: HashMap<Identifier, VmValue>,
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
                    Ok(literal.clone().into())
                }
            },

            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;

                vm_value_operation::evaluate_binary_op(&left_val, operator, &right_val)
            }

            Expression::Unary { operator, operand } => {
                let operand_val = self.evaluate_expr(operand)?;
                vm_value_operation::evaluate_unary_op(operator, &operand_val)
            }

            Expression::Grouping { expression } => self.evaluate_expr(expression),
            Expression::Product { data } => {
                let mut product=HashMap::new();
                for (key,value) in data.iter(){
                    product.insert(key.to_string(),self.evaluate_expr(value)?);
                }
                Ok(VmValue::Product(product))
            },
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
                let VmValue::Literal(Literal::Bool(x)) = self.evaluate_expr(condition)? else {
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
                let VmValue::Literal(Literal::Bool(x)) = self.evaluate_expr(condition)? else {
                    return Err(VmError::TypeMismatch);
                };
                if x {
                    self.execute_statement(on_true)?;
                } else {
                    self.execute_statement(on_false)?;
                }
                Ok(())
            }
            Statement::Debug { expr_vec }=>{
                for expr in expr_vec.iter(){
                    let expr=self.evaluate_expr(expr)?;
                    self.backend.debug_print(&expr.to_string()).unwrap();
                }
                Ok(())
            }   
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AstNode, BinaryOperator, Expression, UnaryOperator, Statement};

    #[test]
    fn test_vm_comprehensive_operations() {
        let vm: Vm = Vm::default();

        // Test integer literal
        let integer = AstNode::new_temp(Expression::integer(42));
        let result = vm.evaluate_expr(&integer).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Integer(42)));

        // Test binary expression: 5 + 3
        let expr = Expression::binary(Expression::integer(5), BinaryOperator::Plus, Expression::integer(3));
        let expr_node = AstNode::new_temp(expr);
        let result = vm.evaluate_expr(&expr_node).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Integer(8)));

        // Test unary expression: -42
        let unary_expr = Expression::unary(UnaryOperator::Minus, Expression::integer(42));
        let unary_expr_node = AstNode::new_temp(unary_expr);
        let result = vm.evaluate_expr(&unary_expr_node).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Integer(-42)));
        
        // Test boolean operations: true && false
        let bool_expr = Expression::binary(Expression::bool(true), BinaryOperator::And, Expression::bool(false));
        let bool_node = AstNode::new_temp(bool_expr);
        let result = vm.evaluate_expr(&bool_node).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Bool(false)));
        
        // Test NOT operation: !true = false
        let not_expr = Expression::unary(UnaryOperator::Not, Expression::bool(true));
        let not_node = AstNode::new_temp(not_expr);
        let result = vm.evaluate_expr(&not_node).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Bool(false)));
        
        // Test comparison: 5 > 3
        let comp_expr = Expression::binary(Expression::integer(5), BinaryOperator::Greater, Expression::integer(3));
        let comp_node = AstNode::new_temp(comp_expr);
        let result = vm.evaluate_expr(&comp_node).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Bool(true)));

        // Test complex expression: (2 + 3) * 4 - 1 = 19
        let inner = Expression::binary(Expression::integer(2), BinaryOperator::Plus, Expression::integer(3));
        let grouped = Expression::grouping(inner);
        let multiplied = Expression::binary(grouped, BinaryOperator::Multiply, Expression::integer(4));
        let final_expr = Expression::binary(multiplied, BinaryOperator::Minus, Expression::integer(1));
        let final_expr_node = AstNode::new_temp(final_expr);
        let result = vm.evaluate_expr(&final_expr_node).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Integer(19)));
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

    #[test]
    fn test_debug_statement_single_value() {
         use crate::vm::backend::TestBackend;
         let backend = TestBackend::new();
         let mut vm: Vm<TestBackend> = Vm::new(backend);
         
         // Create a debug statement with a single integer expression
         let expr = AstNode::new_temp(Expression::integer(42));
         let debug_stmt = AstNode::new_temp(Statement::debug(vec![expr]));
         
         // Execute the debug statement
         vm.execute_statement(&debug_stmt).unwrap();
         
         // Verify the debug output
         let debug_output = vm.backend.get_debug_output();
         assert_eq!(debug_output, "42");
     }

     #[test]
     fn test_debug_statement_multiple_values() {
         use crate::vm::backend::TestBackend;
         let backend = TestBackend::new();
         let mut vm: Vm<TestBackend> = Vm::new(backend);
         
         // Create a debug statement with multiple expressions
         let expr1 = AstNode::new_temp(Expression::integer(100));
         let expr2 = AstNode::new_temp(Expression::float(3.4));
         let expr3 = AstNode::new_temp(Expression::string("hello".to_string()));
         
         let debug_stmt = AstNode::new_temp(Statement::debug(vec![expr1, expr2, expr3]));
         
         // Execute the debug statement
         vm.execute_statement(&debug_stmt).unwrap();
         
         // Verify the debug output
         let debug_output = vm.backend.get_debug_output();
         assert_eq!(debug_output, "100\n3.4\n\"hello\"");
     }

     #[test]
     fn test_debug_statement_with_expressions() {
         use crate::vm::backend::TestBackend;
         let backend = TestBackend::new();
         let mut vm: Vm<TestBackend> = Vm::new(backend);
         
         // Create a debug statement with arithmetic expression: 5 + 3
         let expr = Expression::binary(Expression::integer(5), BinaryOperator::Plus, Expression::integer(3));
         let expr_node = AstNode::new_temp(expr);
         let debug_stmt = AstNode::new_temp(Statement::debug(vec![expr_node]));
         
         // Execute the debug statement
         vm.execute_statement(&debug_stmt).unwrap();
         
         // Verify the debug output shows the evaluated result
         let debug_output = vm.backend.get_debug_output();
         assert_eq!(debug_output, "8");
     }

     #[test]
     fn test_debug_statement_with_variables() {
         use crate::vm::backend::TestBackend;
         let backend = TestBackend::new();
         let mut vm: Vm<TestBackend> = Vm::new(backend);
         
         // First, assign a value to a variable
         let assign_stmt = AstNode::new_temp(Statement::assignment(
             "x".to_string(),
             Expression::integer(42),
         ));
         vm.execute_statement(&assign_stmt).unwrap();
         
         // Then debug print the variable
         let var_expr = AstNode::new_temp(Expression::identifier("x".to_string()));
         let debug_stmt = AstNode::new_temp(Statement::debug(vec![var_expr]));
         vm.execute_statement(&debug_stmt).unwrap();
         
         // Verify the debug output shows the variable value
         let debug_output = vm.backend.get_debug_output();
         assert_eq!(debug_output, "42");
     }

     #[test]
     fn test_debug_statement_boolean_values() {
         use crate::vm::backend::TestBackend;
         let backend = TestBackend::new();
         let mut vm: Vm<TestBackend> = Vm::new(backend);
         
         // Create a debug statement with boolean expressions
         let true_expr = AstNode::new_temp(Expression::bool(true));
         let false_expr = AstNode::new_temp(Expression::bool(false));
         let comparison = Expression::binary(Expression::integer(5), BinaryOperator::Greater, Expression::integer(3));
         let comp_expr = AstNode::new_temp(comparison);
         
         let debug_stmt = AstNode::new_temp(Statement::debug(vec![true_expr, false_expr, comp_expr]));
         
         // Execute the debug statement
         vm.execute_statement(&debug_stmt).unwrap();
         
         // Verify the debug output
         let debug_output = vm.backend.get_debug_output();
         assert_eq!(debug_output, "true\nfalse\ntrue");
     }
}
