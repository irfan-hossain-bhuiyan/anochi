//! Anochi Programming Language
//!
//! A compiled programming language implemented in Rust, designed to be productive
//! while maintaining simplicity and performance.
//!
//! This crate provides the core functionality for the Anochi programming language,
//! including lexical analysis, parsing, and compilation.
pub mod token;
pub mod ast;
pub mod parser;
pub mod types;
pub mod vm;
pub mod code_runner;
pub mod prelude;

pub use token::{Token, TokenType, Tokenizer};
//pub use code_runner::CodeRunner;

