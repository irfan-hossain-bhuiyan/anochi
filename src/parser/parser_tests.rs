use crate::ast::{Identifier,  StatNodeGeneric, StatementBlockGeneric, StatementGeneric};
use crate::ast::expression::ExpressionGeneric;
use crate::parser::Parser;
use crate::prelude::Mappable;
use crate::token::{TokenContainer, Tokenizer};

/// Helper function to create tokens from source code
fn create_tokens_from_source(source: &str) -> TokenContainer {
    let (tokens,errors)=Tokenizer::new(source).tokenize();
    if !errors.is_empty(){panic!("Tokenizer has error")}
    tokens
}



/// Helper function to parse AST from source
fn parse_ast_from_source(source: &str) -> StatNodeGeneric<()> {
    let tokens = create_tokens_from_source(source);
    let mut parser = Parser::new(&tokens);
    parser.parse_statement().unwrap().inner_map(&mut |_x| ())
}

/// Test parsing simple empty block statement
#[test]
fn test_parse_empty_block() {
    let source = "{}";
    let statement = parse_ast_from_source(source);
    let expected = StatementGeneric::StatementBlock(StatementBlockGeneric::new(
        Vec::new(), ()
    ))
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing block with single assignment statement
#[test]
fn test_parse_block_with_single_assignment() {
    let source = "{ let x = 42; }";
    let statement = parse_ast_from_source(source);
    let expected = StatementGeneric::StatementBlock(StatementBlockGeneric::new(
        vec![
            StatementGeneric::assignment(
                Identifier::new("x"),
                None,
                ExpressionGeneric::from_i64(42).to_node(()),
            )
            .to_node(()),
        ], ()
    ))
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing block with multiple statements
#[test]
fn test_parse_block_with_multiple_statements() {
    let source = "{ let x = 42; let y = true; }";
    let statement = parse_ast_from_source(source);
    let expected = StatementGeneric::StatementBlock(StatementBlockGeneric::new(
        vec![
            StatementGeneric::assignment(
                Identifier::new("x"),
                None,
                ExpressionGeneric::from_i64(42).to_node(()),
            )
            .to_node(()),
            StatementGeneric::assignment(
                Identifier::new("y"),
                None,
                ExpressionGeneric::from_bool(true).to_node(()),
            )
            .to_node(()),
        ], ()
    ))
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing nested blocks
#[test]
fn test_parse_nested_blocks() {
    let source = "{ let x = 42; { let y = true; } }";
    let statement = parse_ast_from_source(source);
    let expected = StatementGeneric::StatementBlock(StatementBlockGeneric::new(
        vec![
            StatementGeneric::assignment(
                Identifier::new("x"),
                None,
                ExpressionGeneric::from_i64(42).to_node(()),
            )
            .to_node(()),
            StatementGeneric::StatementBlock(StatementBlockGeneric::new(
                vec![
                    StatementGeneric::assignment(
                        Identifier::new("y"),
                        None,
                        ExpressionGeneric::from_bool(true).to_node(()),
                    )
                    .to_node(()),
                ], ()
            ))
            .to_node(()),
        ], ()
    ))
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing block with mutable assignment
#[test]
fn test_parse_block_with_mutable_assignment() {
    let source = "{ x = 100; }";
    let statement = parse_ast_from_source(source);
    let expected = StatementGeneric::StatementBlock(StatementBlockGeneric::new(
        vec![
            StatementGeneric::mutable_assignment(
                ExpressionGeneric::identifier(Identifier::new("x")).to_node(()),
                ExpressionGeneric::from_i64(100).to_node(()),
            )
            .to_node(()),
        ], ()
    ))
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing block with if statement
#[test]
fn test_parse_block_with_if_statement() {
    let source = "{ if true { let x = 42; } }";
    let statement = parse_ast_from_source(source);
    let expected = StatementGeneric::StatementBlock(StatementBlockGeneric::new(
        vec![
            StatementGeneric::if_stmt(
                ExpressionGeneric::from_bool(true).to_node(()),
                StatementGeneric::StatementBlock(StatementBlockGeneric::new(
                    vec![
                        StatementGeneric::assignment(
                            Identifier::new("x"),
                            None,
                            ExpressionGeneric::from_i64(42).to_node(()),
                        )
                        .to_node(()),
                    ], ()
                ))
                .to_node(()),
            )
            .to_node(()),
        ], ()
    ))
    .to_node(());
    assert_eq!(statement, expected);
}

/// Test parsing block with mixed statement types
#[test]
fn test_parse_block_with_mixed_statements() {
    let source = "{ let x = 42; x = 100; if x { let y = true; } }";
    let statement = parse_ast_from_source(source);
    let expected = StatementGeneric::StatementBlock(StatementBlockGeneric::new(
        vec![
            StatementGeneric::assignment(
                Identifier::new("x"),
                None,
                ExpressionGeneric::from_i64(42).to_node(()),
            )
            .to_node(()),
            StatementGeneric::mutable_assignment(
                ExpressionGeneric::identifier(Identifier::new("x")).to_node(()),
                ExpressionGeneric::from_i64(100).to_node(()),
            )
            .to_node(()),
            StatementGeneric::if_stmt(
                ExpressionGeneric::identifier(Identifier::new("x")).to_node(()),
                StatementGeneric::StatementBlock(StatementBlockGeneric::new(
                    vec![
                        StatementGeneric::assignment(
                            Identifier::new("y"),
                            None,
                            ExpressionGeneric::from_bool(true).to_node(()),
                        )
                        .to_node(()),
                    ], ()
                ))
                .to_node(()),
            )
            .to_node(()),
        ], ()
    ))
    .to_node(());
    assert_eq!(statement,expected)
}
    
#[test]
fn parse_loop() {
    let source = "loop {break; }";
    let statement = parse_ast_from_source(source);

    let expected = StatementGeneric::Loop {
        statements: StatementBlockGeneric::new(
            vec![StatementGeneric::Break.to_node(())], ()
        ),
    }
    .to_node(());

    assert_eq!(statement, expected);

    assert_eq!(statement, expected);
}

#[test]
fn test_parse_reference_and_dereference() {
    let source = "{ let r = &x; let v = *r; *r = 20; }";
    let statement = parse_ast_from_source(source);
    
    let expected = StatementGeneric::StatementBlock(StatementBlockGeneric::new(
        vec![
            StatementGeneric::assignment(
                Identifier::new("r"),
                None,
                ExpressionGeneric::unary(
                    crate::ast::UnaryOperator::Ref,
                    ExpressionGeneric::identifier(Identifier::new("x")).to_node(())
                ).to_node(())
            ).to_node(()),
            StatementGeneric::assignment(
                Identifier::new("v"),
                None,
                ExpressionGeneric::unary(
                    crate::ast::UnaryOperator::Deref,
                    ExpressionGeneric::identifier(Identifier::new("r")).to_node(())
                ).to_node(())
            ).to_node(()),
            StatementGeneric::mutable_assignment(
                ExpressionGeneric::unary(
                    crate::ast::UnaryOperator::Deref,
                    ExpressionGeneric::identifier(Identifier::new("r")).to_node(())
                ).to_node(()),
                ExpressionGeneric::from_i64(20).to_node(())
            ).to_node(())
        ], ()
    )).to_node(());
    
    assert_eq!(statement, expected);
}
