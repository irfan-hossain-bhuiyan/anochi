//! Code Runner module for the Anochi programming language.
//!
//! This module provides the `CodeRunner` struct that ties together the tokenizer,
//! parser, and VM components to execute Anochi source code from a string input
//! and return the result.

use crate::{
    parser::Parser,
    token::{token_type::TokenizerError, Tokenizer},
    vm::{backend::VmBackend, tree_walk::{Vm, VmValue}},
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
    pub fn evaluate_expr(&mut self, source: &str) -> Result<VmValue, CodeRunnerError> {
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
            return Ok(value)
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

