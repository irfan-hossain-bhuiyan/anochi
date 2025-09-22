//! Anochi Programming Language
//!
//! A compiled programming language implemented in Rust, designed to be productive
//! while maintaining simplicity and performance.
//!
//! This crate provides the core functionality for the Anochi programming language,
//! including lexical analysis, parsing, and compilation.
pub mod tree_walk_vm;
pub mod token;
pub mod ast;
pub use token::{Token, TokenType, Tokenizer};

