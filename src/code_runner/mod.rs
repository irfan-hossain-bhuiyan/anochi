//! Code Runner module for the Anochi programming language.
//!
//! This module provides the `CodeRunner` struct that ties together the tokenizer,
//! parser, and VM components to execute Anochi source code from a string input
//! and return the result.

use crate::{
    code_error::CodeError, parser::{Parser, ParserErrorType}, prelude::Mappable, token::{Tokenizer, token_type::TokenizerErrorType}, vm::{
        backend::VmBackend,
        tree_walk::{Vm, VmErrorType, VmValue},
    }
};
use std::fmt;
use derive_more::From;
use enum_dispatch::enum_dispatch;
#[enum_dispatch(CodeErrorType)]
#[derive(Debug, From)]
pub enum CodeRunnerErrorType {
    TokenizerErrorType,
    ParserErrorType,
    VmErrorType,
}

pub type CodeRunnerError=CodeError<CodeRunnerErrorType>;

impl<T:Into<CodeRunnerErrorType>> CodeError<T>{
    fn into_code_error(self)->CodeRunnerError{
        self.inner_map(&mut |x|Into::into(x))
    }
}
impl CodeRunnerErrorType {
    pub fn as_tokenization_error(&self) -> Option<&TokenizerErrorType> {
        if let Self::TokenizerErrorType(v) = self {
            Some(v)
        } else {
            None
        }
    }
    pub fn error_type_str(&self)->&'static str{
        match self {
            Self::TokenizerErrorType(_)=>"tokenization error",
            Self::ParserErrorType(_)=>"parser error",
            Self::VmErrorType(_)=>"runtime error",
        }
    }
    pub fn as_parse_error(&self) -> Option<&ParserErrorType> {
        if let Self::ParserErrorType(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_runtime_error(&self) -> Option<&VmErrorType> {
        if let Self::VmErrorType(v) = self {
            Some(v)
        } else {
            None
        }
    }
    pub fn is_runtime_error_and(&self,f:impl Fn (&VmErrorType)->bool)->bool{
        self.as_runtime_error().is_some_and(f)
    }
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
    pub fn run_statements(&mut self, source: &str) -> Result<(), CodeRunnerError> {
        // Step 1: Tokenize the source code
        let tokenizer = Tokenizer::new(source);
        let (tokens,errors) = tokenizer.tokenize();

        // Check for tokenization errors
        println!("{}",errors.err_str(source));
        // Step 2: Parse tokens into AST
        let mut parser = Parser::new(&tokens);
let stmt_node = parser.parse_statements().map_err(|x|x.into_code_error())?;
        // Step 3: Execute the statement
        self.vm.execute_statement(&stmt_node).map_err(|x|x.into_code_error())?;
        Ok(())// Return unit type for statements
        // If both fail, return parse error
    }
    pub fn evaluate_expr(&mut self, source: &str) -> Result<VmValue, CodeRunnerError> {
        let tokenizer = Tokenizer::new(source);
        let (tokens,errors) = tokenizer.tokenize();

        println!("{}",errors.err_str(source));
        let mut parser = Parser::new(&tokens);
        let stmt_node = parser.parse_expression().map_err(|x| x.into_code_error())?;
        let value = self.vm.evaluate_expr(&stmt_node).map_err(|x| x.into_code_error())?;
        Ok(value)
    }
    

    pub fn vm(&self) -> &Vm<Backend> {
        &self.vm
    }

    fn stack_trace(&self) {
        self.vm.print_stack();
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
mod code_runner_tests;
