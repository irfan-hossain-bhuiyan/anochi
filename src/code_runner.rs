//! Code Runner module for the Anochi programming language.
//!
//! This module provides the `CodeRunner` struct that ties together the tokenizer,
//! parser, and VM components to execute Anochi source code from a string input
//! and return the result.

use crate::{
    parser::Parser,
    token::Tokenizer,
    vm::{backend::VmBackend, tree_walk::Vm},
};
use std::fmt;
use thiserror::Error;

/// Errors that can occur during code execution.
#[derive(Error, Debug)]
pub enum CodeRunnerError {
    /// Error occurred during tokenization
    #[error("Tokenization error: {0}")]
    TokenizationError(String),
    
    /// Error occurred during parsing
    #[error("Parse error: could not parse the input")]
    ParseError,
    
    /// Error occurred during VM execution
    #[error("Runtime error: {0}")]
    RuntimeError(#[from] crate::vm::tree_walk::VmError),
}

/// Result type for code execution operations.
pub type CodeRunnerResult<T> = Result<T, CodeRunnerError>;

/// Main code runner that orchestrates tokenization, parsing, and execution.
///
/// The `CodeRunner` takes a string of source code and processes it through
/// the complete pipeline: tokenization -> parsing -> execution, returning
/// the final result or any errors that occur along the way.
///
/// # Design
///
/// - Simple and stateless for basic expression evaluation
/// - Minimal interface: string input, result output
/// - Proper error handling and propagation
/// - Modular design allowing different VM backends
///
/// # Example
///
/// ```rust
/// use anochi::CodeRunner;
/// use anochi::vm::backend::IoBackend;
/// 
/// let mut runner = CodeRunner::new(IoBackend::default());
/// let result = runner.run("2 + 3 * 4").unwrap();
/// println!("Result: {}", result); // Result: 14
/// ```
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
    ///
    /// # Example
    ///
    /// ```rust
    /// use anochi::CodeRunner;
    /// 
    /// let mut runner = CodeRunner::default();
    /// let result = runner.run("10 - 4")?;
    /// assert_eq!(result.to_string(), "6");
    /// # Ok::<(), anochi::code_runner::CodeRunnerError>(())
    /// ```
    pub fn run(&mut self, source: &str) -> CodeRunnerResult<String> {
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
        
        // Try to parse as an expression first
        if let Some(expr_node) = parser.parse_expression() {
            // Step 3: Execute the expression
            let result = self.vm.evaluate_expr(&expr_node)?;
            return Ok(result.to_string());
        }

        // If expression parsing fails, try statement parsing
        let mut parser = Parser::new(&tokens);
        if let Some(stmt_node) = parser.parse_statement() {
            // Step 3: Execute the statement
            self.vm.execute_statement(&stmt_node)?;
            return Ok("()".to_string()); // Return unit type for statements
        }

        // If both fail, return parse error
        Err(CodeRunnerError::ParseError)
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
    fn test_basic_arithmetic() {
        let mut runner = CodeRunner::default();
        
        // Test addition
        assert_eq!(runner.run("2 + 3").unwrap(), "5");
        
        // Test multiplication with precedence
        assert_eq!(runner.run("2 + 3 * 4").unwrap(), "14");
        
        // Test parentheses
        assert_eq!(runner.run("(2 + 3) * 4").unwrap(), "20");
        
        // Test subtraction
        assert_eq!(runner.run("10 - 4").unwrap(), "6");
        
        // Test division
        assert_eq!(runner.run("15 / 3").unwrap(), "5");
    }

    #[test]
    fn test_float_operations() {
        let mut runner = CodeRunner::default();
        
        // Test float literal
        assert_eq!(runner.run("3.14").unwrap(), "3.14");
        
        // Test float arithmetic
        assert_eq!(runner.run("2.5 + 1.5").unwrap(), "4");
    }

    #[test]
    fn test_boolean_operations() {
        let mut runner = CodeRunner::default();
        
        // Test boolean literals
        assert_eq!(runner.run("true").unwrap(), "true");
        assert_eq!(runner.run("false").unwrap(), "false");
        
        // Test logical operations
        assert_eq!(runner.run("true and false").unwrap(), "false");
        assert_eq!(runner.run("true or false").unwrap(), "true");
        assert_eq!(runner.run("not true").unwrap(), "false");
    }

    #[test]
    fn test_comparison_operations() {
        let mut runner = CodeRunner::default();
        
        // Test comparisons
        assert_eq!(runner.run("5 > 3").unwrap(), "true");
        assert_eq!(runner.run("2 < 1").unwrap(), "false");
        assert_eq!(runner.run("4 == 4").unwrap(), "true");
        assert_eq!(runner.run("3 != 5").unwrap(), "true");
    }

    #[test]
    fn test_unary_operations() {
        let mut runner = CodeRunner::default();
        
        // Test unary minus
        assert_eq!(runner.run("-5").unwrap(), "-5");
        assert_eq!(runner.run("-(2 + 3)").unwrap(), "-5");
    }

    #[test]
    fn test_error_handling() {
        let mut runner = CodeRunner::default();
        
        // Test division by zero
        let result = runner.run("5 / 0");
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), CodeRunnerError::RuntimeError(_)));
        
        // Test invalid syntax (should be parse error)
        let result = runner.run("2 + + 3");
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), CodeRunnerError::ParseError));
    }

    #[test]
    fn test_complex_expressions() {
        let mut runner = CodeRunner::default();
        
        // Test complex nested expression
        assert_eq!(runner.run("((2 + 3) * 4) - (10 / 2)").unwrap(), "15");
        
        // Test mixed arithmetic and logical
        assert_eq!(runner.run("(5 > 3) and (2 < 4)").unwrap(), "true");
    }
}
