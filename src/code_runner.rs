//! Code Runner module for the Anochi programming language.
//!
//! This module provides the `CodeRunner` struct that ties together the tokenizer,
//! parser, and VM components to execute Anochi source code from a string input
//! and return the result.

use crate::{
    ast::Literal,
    parser::Parser,
    token::{token_type::TokenizerError, Tokenizer},
    vm::{backend::VmBackend, tree_walk::Vm},
};
use std::fmt;
use thiserror::Error;

/// Errors that can occur during code execution.
#[derive(Error, Debug)]
pub enum CodeRunnerError {
    /// Error occurred during tokenization
    #[error("Tokenization error: {0}")]
    TokenizationError(#[from] TokenizerError),

    /// Error occurred during parsing
    #[error("Parse error: could not parse the input")]
    ParseError,

    /// Error occurred during VM execution
    #[error("Runtime error: {0}")]
    RuntimeError(#[from] crate::vm::tree_walk::VmError),
}
/// Main code runner that orchestrates tokenization, parsing, and execution.
///
/// The `CodeRunner` takes a string of source code and processes it through
/// the complete pipeline: tokenization -> parsing -> execution, returning
/// the final result or any errors that occur along the way.
///
pub struct CodeRunner<Backend: VmBackend = crate::vm::backend::IoBackend> {
    vm: Vm<Backend>,
}

impl<Backend: VmBackend> CodeRunner<Backend> {
    /// Creates a new CodeRunner with the specified VM backend.
    ///
    /// # Arguments
    ///
    /// * `backend` - The VM backend to use for execution
    ///
    /// # Returns
    ///
    /// A new `CodeRunner` instance.
    pub fn new(backend: Backend) -> Self {
        Self {
            vm: Vm::new(backend),
        }
    }

    /// Executes the given source code and returns the result.
    ///
    /// This method processes the complete pipeline:
    /// 1. Tokenizes the input string
    /// 2. Parses tokens into an AST
    /// 3. Executes the AST using the VM
    ///
    /// # Arguments
    ///
    /// * `source` - The source code string to execute
    ///
    /// # Returns
    ///
    /// A `CodeRunnerResult` containing the execution result or an error.
    pub fn run_statement(&mut self, source: &str) -> Result<(), CodeRunnerError> {
        // Step 1: Tokenize the source code
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();

        // Check for tokenization errors
        for token in &tokens {
            if let crate::token::TokenType::Error(err) = &token.token_type {
                return Err(CodeRunnerError::TokenizationError(err.clone()));
            }
        }
        // Step 2: Parse tokens into AST
        let mut parser = Parser::new(&tokens);
        if let Ok(stmt_node) = parser.parse_statement() {
            // Step 3: Execute the statement
            self.vm.execute_statement(&stmt_node)?;
            return Ok(()); // Return unit type for statements
        }

        // If both fail, return parse error
        Err(CodeRunnerError::ParseError)
    }
    pub fn evaluate_expr(&mut self, source: &str) -> Result<Literal, CodeRunnerError> {
        // Step 1: Tokenize the source code
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();

        // Check for tokenization errors
        for token in &tokens {
            if let crate::token::TokenType::Error(err) = &token.token_type {
                return Err(CodeRunnerError::TokenizationError(err.clone()));
            }
        }
        // Step 2: Parse tokens into AST
        let mut parser = Parser::new(&tokens);
        if let Ok(stmt_node) = parser.parse_expression() {
            // Step 3: Execute the statement
            let value=self.vm.evaluate_expr(&stmt_node)?;
            return Ok(value); // Return unit type for statements
        }

        // If both fail, return parse error
        Err(CodeRunnerError::ParseError)
    }

    pub fn vm(&self) -> &Vm<Backend> {
        &self.vm
    }
}

impl Default for CodeRunner<crate::vm::backend::IoBackend> {
    /// Creates a new CodeRunner with the default IoBackend.
    fn default() -> Self {
        Self::new(crate::vm::backend::IoBackend::default())
    }
}

impl<Backend: VmBackend> fmt::Debug for CodeRunner<Backend> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CodeRunner")
            .field("vm", &"Vm { ... }")
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_arithmetic_and_float_and_bool() {
        let mut runner = CodeRunner::default();
        // Integer arithmetic
        assert_eq!(runner.evaluate_expr("2 + 3").unwrap(), Literal::Integer(5));
        assert_eq!(runner.evaluate_expr("2 + 3 * 4").unwrap(), Literal::Integer(14));
        assert_eq!(runner.evaluate_expr("(2 + 3) * 4").unwrap(), Literal::Integer(20));
        assert_eq!(runner.evaluate_expr("10 - 4").unwrap(), Literal::Integer(6));
        assert_eq!(runner.evaluate_expr("15 / 3").unwrap(), Literal::Integer(5));
        // Float arithmetic
        assert_eq!(runner.evaluate_expr("2.5 + 1.5").unwrap(), Literal::Float(4.0));
        // Boolean
        assert_eq!(runner.evaluate_expr("true").unwrap(), Literal::Bool(true));
        assert_eq!(runner.evaluate_expr("false").unwrap(), Literal::Bool(false));
        assert_eq!(runner.evaluate_expr("true and false").unwrap(), Literal::Bool(false));
        assert_eq!(runner.evaluate_expr("true or false").unwrap(), Literal::Bool(true));
        assert_eq!(runner.evaluate_expr("not true").unwrap(), Literal::Bool(false));
        // Comparison
        assert_eq!(runner.evaluate_expr("5 > 3").unwrap(), Literal::Bool(true));
        assert_eq!(runner.evaluate_expr("2 < 1").unwrap(), Literal::Bool(false));
        assert_eq!(runner.evaluate_expr("4 == 4").unwrap(), Literal::Bool(true));
        assert_eq!(runner.evaluate_expr("3 != 5").unwrap(), Literal::Bool(true));
        // Unary minus
        assert_eq!(runner.evaluate_expr("-5").unwrap(), Literal::Integer(-5));
        assert_eq!(runner.evaluate_expr("-(2 + 3)").unwrap(), Literal::Integer(-5));
        // Complex nested expression
        assert_eq!(runner.evaluate_expr("((2 + 3) * 4) - (10 / 2)").unwrap(), Literal::Integer(15));
        // Mixed arithmetic and logical
        assert_eq!(runner.evaluate_expr("(5 > 3) and (2 < 4)").unwrap(), Literal::Bool(true));
    }

    #[test]
    fn test_error_handling() {
        let mut runner = CodeRunner::default();
        // Test division by zero
        let result = runner.evaluate_expr("5 / 0");
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), CodeRunnerError::RuntimeError(_)));
        // Test invalid syntax (should be parse error)
        let result = runner.evaluate_expr("2 + + 3");
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), CodeRunnerError::ParseError));
    }
}

