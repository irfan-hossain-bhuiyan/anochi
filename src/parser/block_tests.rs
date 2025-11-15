use crate::ast::{Expression, Identifier, StatNode, Statement, StatementBlock};
use crate::parser::Parser;
use crate::prelude::Mappable;
use crate::token::{Position, TokenContainer, Tokenizer};
use std::num::NonZeroUsize;

/// Helper function to create tokens from source code
fn create_tokens_from_source(source: &str) -> TokenContainer {
    Tokenizer::new(source).tokenize()
}

/// Helper function to create a simple position for testing
fn create_test_position(slice: &str) -> Position {
    Position {
        line: NonZeroUsize::new(1).unwrap(),
        column: NonZeroUsize::new(1).unwrap(),
        slice,
    }
}

/// Helper function to parse AST from source
fn parse_ast_from_source(source: &str) -> StatNode<()> {
    let tokens = create_tokens_from_source(source);
    let mut parser = Parser::new(&tokens);
    parser.parse_statement().unwrap().inner_map(&mut |_x| ())
}

/// Test parsing simple empty block statement
#[test]
fn test_parse_empty_block() {
    let source = "{}";
    let statement = parse_ast_from_source(source);
    let expected = Statement::StatementBlock(StatementBlock {
        statements: Vec::new(),
    })
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing block with single assignment statement
#[test]
fn test_parse_block_with_single_assignment() {
    let source = "{ let x = 42; }";
    let statement = parse_ast_from_source(source);
    let expected = Statement::StatementBlock(StatementBlock {
        statements: vec![
            Statement::assignment(
                Identifier::new("x"),
                None,
                Expression::from_i64(42).to_node(()),
            )
            .to_node(()),
        ],
    })
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing block with multiple statements
#[test]
fn test_parse_block_with_multiple_statements() {
    let source = "{ let x = 42; let y = true; }";
    let statement = parse_ast_from_source(source);
    let expected = Statement::StatementBlock(StatementBlock {
        statements: vec![
            Statement::assignment(
                Identifier::new("x"),
                None,
                Expression::from_i64(42).to_node(()),
            )
            .to_node(()),
            Statement::assignment(
                Identifier::new("y"),
                None,
                Expression::from_bool(true).to_node(()),
            )
            .to_node(()),
        ],
    })
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing nested blocks
#[test]
fn test_parse_nested_blocks() {
    let source = "{ let x = 42; { let y = true; } }";
    let statement = parse_ast_from_source(source);
    let expected = Statement::StatementBlock(StatementBlock {
        statements: vec![
            Statement::assignment(
                Identifier::new("x"),
                None,
                Expression::from_i64(42).to_node(()),
            )
            .to_node(()),
            Statement::StatementBlock(StatementBlock {
                statements: vec![
                    Statement::assignment(
                        Identifier::new("y"),
                        None,
                        Expression::from_bool(true).to_node(()),
                    )
                    .to_node(()),
                ],
            })
            .to_node(()),
        ],
    })
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing block with mutable assignment
#[test]
fn test_parse_block_with_mutable_assignment() {
    let source = "{ x = 100; }";
    let statement = parse_ast_from_source(source);
    let expected = Statement::StatementBlock(StatementBlock {
        statements: vec![
            Statement::mutable_assignment(
                Expression::identifier(Identifier::new("x")).to_node(()),
                Expression::from_i64(100).to_node(()),
            )
            .to_node(()),
        ],
    })
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing block with if statement
#[test]
fn test_parse_block_with_if_statement() {
    let source = "{ if true { let x = 42; } }";
    let statement = parse_ast_from_source(source);
    let expected = Statement::StatementBlock(StatementBlock {
        statements: vec![
            Statement::if_stmt(
                Expression::from_bool(true).to_node(()),
                Statement::StatementBlock(StatementBlock {
                    statements: vec![
                        Statement::assignment(
                            Identifier::new("x"),
                            None,
                            Expression::from_i64(42).to_node(()),
                        )
                        .to_node(()),
                    ],
                })
                .to_node(()),
            )
            .to_node(()),
        ],
    })
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing block with mixed statement types
#[test]
fn test_parse_block_with_mixed_statements() {
    let source = "{ let x = 42; x = 100; if x { let y = true; } }";
    let statement = parse_ast_from_source(source);
    let expected = Statement::StatementBlock(StatementBlock {
        statements: vec![
            Statement::assignment(
                Identifier::new("x"),
                None,
                Expression::from_i64(42).to_node(()),
            )
            .to_node(()),
            Statement::mutable_assignment(
                Expression::identifier(Identifier::new("x")).to_node(()),
                Expression::from_i64(100).to_node(()),
            )
            .to_node(()),
            Statement::if_stmt(
                Expression::identifier(Identifier::new("x")).to_node(()),
                Statement::StatementBlock(StatementBlock {
                    statements: vec![
                        Statement::assignment(
                            Identifier::new("y"),
                            None,
                            Expression::from_bool(true).to_node(()),
                        )
                        .to_node(()),
                    ],
                })
                .to_node(()),
            )
            .to_node(()),
        ],
    })
    .to_node(());
    assert_eq!(statement, expected);
}
#[test]
fn parse_loop() {
    let source = "loop {break; }";
    let statement = parse_ast_from_source(source);

    let expected =Statement::Loop {
                statements: StatementBlock {
                    statements: vec![Statement::Break.to_node(())],
                },
            }
    .to_node(());

    assert_eq!(statement, expected);
}
