#[cfg(test)]
mod parser_block_tests {
    use crate::ast::{Identifier, StatNode, Statement, StatementBlock};
    use crate::parser::Parser;
    use crate::prelude::Mappable;
    use crate::token::{Position, Token, TokenContainer, TokenType, Tokenizer};
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
        parser.parse_statement().unwrap().inner_map(&mut |_x|())
    }

    /// Test parsing simple empty block statement
    #[test]
    fn test_parse_empty_block() {
        let source = "{}";
        let statement = parse_ast_from_source(source);
        let expected = Statement::StatementBlock(StatementBlock { statements: Vec::new() }).to_node(());
        assert_eq!(statement,expected);
    }

    /// Test parsing block with single assignment statement
    #[test]
    fn test_parse_block_with_single_assignment() {
        let source = "{ let x = 42; }";
        let parsed_ast = parse_ast_from_source(source);
        let statement = parsed_ast;
        match &statement.stat {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 1);
                match &statements[0].stat {
                    Statement::Assignment { target, value, .. } => {
                        assert_eq!(target, &Identifier::new("x"));
                    }
                    _ => panic!("Expected Assignment statement"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.stat),
        }
    }

    /// Test parsing block with multiple statements
    #[test]
    fn test_parse_block_with_multiple_statements() {
        let source = "{ let x = 42; let y = true; }";
        let parsed_ast = parse_ast_from_source(source);
        let statement = parsed_ast;
        match &statement.stat {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 2);

                // Check first statement
                match &statements[0].stat {
                    Statement::Assignment { target, .. } => {
                        assert_eq!(target, &Identifier::new("x".to_string()));
                    }
                    _ => panic!("Expected first Assignment statement"),
                }

                // Check second statement
                match &statements[1].stat {
                    Statement::Assignment { target, .. } => {
                        assert_eq!(target, &Identifier::new("y".to_string()));
                    }
                    _ => panic!("Expected second Assignment statement"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.stat),
        }
    }

    /// Test parsing nested blocks
    #[test]
    fn test_parse_nested_blocks() {
        let source = "{ let x = 42; { let y = true; } }";
        let parsed_ast = parse_ast_from_source(source);
        let statement = parsed_ast;
        match &statement.stat {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 2);

                // Check first statement is assignment
                match &statements[0].stat {
                    Statement::Assignment { target, .. } => {
                        assert_eq!(target, &Identifier::new("x".to_string()));
                    }
                    _ => panic!("Expected Assignment statement"),
                }

                // Check second statement is nested block
                match &statements[1].stat {
                    Statement::StatementBlock(StatementBlock {
                        statements: nested_statements,
                    }) => {
                        assert_eq!(nested_statements.len(), 1);
                        match &nested_statements[0].stat {
                            Statement::Assignment { target, .. } => {
                                assert_eq!(target, &Identifier::new("y".to_string()));
                            }
                            _ => panic!("Expected nested Assignment statement"),
                        }
                    }
                    _ => panic!("Expected nested StatementBlock"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.stat),
        }
    }

    /// Test parsing block with mutable assignment
    #[test]
    fn test_parse_block_with_mutable_assignment() {
        let source = "{ x = 100; }";
        let parsed_ast = parse_ast_from_source(source);
        let statement = parsed_ast;
        match &statement.stat {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 1);
                match &statements[0].stat {
                    Statement::MutableAssignment { target, .. } => {
                        // Target should be an identifier expression
                        match &target.exp {
                            crate::ast::Expression::Literal(crate::ast::Literal::Identifier(
                                id,
                            )) => {
                                assert_eq!(id, &Identifier::new("x".to_string()));
                            }
                            _ => panic!("Expected identifier in mutable assignment target"),
                        }
                    }
                    _ => panic!("Expected MutableAssignment statement"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.stat),
        }
    }

    /// Test parsing block with if statement
    #[test]
    fn test_parse_block_with_if_statement() {
        let source = "{ if true { let x = 42; } }";
        let parsed_ast = parse_ast_from_source(source);
        let statement = parsed_ast;
        match &statement.stat {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 1);
                match &statements[0].stat {
                    Statement::If { condition, on_true } => {
                        // Verify on_true is also a block
                        match &on_true.stat {
                            Statement::StatementBlock(StatementBlock {
                                statements: if_statements,
                            }) => {
                                assert_eq!(if_statements.len(), 1);
                            }
                            _ => panic!("Expected StatementBlock in if body"),
                        }
                    }
                    _ => panic!("Expected If statement"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.stat),
        }
    }

    /// Test parsing block with mixed statement types
    #[test]
    fn test_parse_block_with_mixed_statements() {
        let source = "{ let x = 42; x = 100; if x { let y = true; } }";
        let parsed_ast = parse_ast_from_source(source);
        let statement = parsed_ast;
        match &statement.stat {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 3);

                // First: let assignment
                match &statements[0].stat {
                    Statement::Assignment { target, .. } => {
                        assert_eq!(target, &Identifier::new("x".to_string()));
                    }
                    _ => panic!("Expected Assignment statement at index 0"),
                }

                // Second: mutable assignment
                match &statements[1].stat {
                    Statement::MutableAssignment { .. } => {} // Just verify it's mutable assignment
                    _ => panic!("Expected MutableAssignment statement at index 1"),
                }

                // Third: if statement
                match &statements[2].stat {
                    Statement::If { .. } => {} // Just verify it's if statement
                    _ => panic!("Expected If statement at index 2"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.stat),
        }
    }
}
