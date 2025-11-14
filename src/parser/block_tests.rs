#[cfg(test)]
mod parser_block_tests {
    use super::*;
    use crate::ast::{Identifier, Statement, StatementBlock};
    use crate::parser::Parser;
    use crate::token::{Tokenizer, Token, Position, TokenType};
    use crate::token::token_type::Keyword;
    use num_bigint::BigInt;
    use std::num::NonZeroUsize;

    /// Helper function to create tokens from source code
    fn create_tokens_from_source(source: &str) -> Vec<Token> {
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

    /// Test parsing simple empty block statement
    #[test]
    fn test_parse_empty_block() {
        let source = "{}";
        let tokens = create_tokens_from_source(source);
        let mut parser = Parser::new(&tokens);
        
        let result = parser.parse_statement();
        let statement = result.unwrap();
        match &statement.node {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert!(statements.is_empty());
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.node),
        }
    }

    /// Test parsing block with single assignment statement
    #[test]
    fn test_parse_block_with_single_assignment() {
        let source = "{ let x = 42; }";
        let tokens = create_tokens_from_source(source);
        let mut parser = Parser::new(&tokens);
        
        let result = parser.parse_statement();
        assert!(result.is_ok());
        
        let statement = result.unwrap();
        match &statement.node {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 1);
                match &statements[0].node {
                    Statement::Assignment { target, value, .. } => {
                        assert_eq!(target, &Identifier::new("x"));
                    }
                    _ => panic!("Expected Assignment statement"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.node),
        }
    }

    /// Test parsing block with multiple statements
    #[test]
    fn test_parse_block_with_multiple_statements() {
        let source = "{ let x = 42; let y = true; }";
        let tokens = create_tokens_from_source(source);
        let mut parser = Parser::new(&tokens);
        
        let result = parser.parse_statement();
        assert!(result.is_ok());
        
        let statement = result.unwrap();
        match &statement.node {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 2);
                
                // Check first statement
                match &statements[0].node {
                    Statement::Assignment { target, .. } => {
                        assert_eq!(target, &Identifier::new("x".to_string()));
                    }
                    _ => panic!("Expected first Assignment statement"),
                }
                
                // Check second statement
                match &statements[1].node {
                    Statement::Assignment { target, .. } => {
                        assert_eq!(target, &Identifier::new("y".to_string()));
                    }
                    _ => panic!("Expected second Assignment statement"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.node),
        }
    }

    /// Test parsing nested blocks
    #[test]
    fn test_parse_nested_blocks() {
        let source = "{ let x = 42; { let y = true; } }";
        let tokens = create_tokens_from_source(source);
        let mut parser = Parser::new(&tokens);
        
        let result = parser.parse_statement();
        assert!(result.is_ok());
        
        let statement = result.unwrap();
        match &statement.node {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 2);
                
                // Check first statement is assignment
                match &statements[0].node {
                    Statement::Assignment { target, .. } => {
                        assert_eq!(target, &Identifier::new("x".to_string()));
                    }
                    _ => panic!("Expected Assignment statement"),
                }
                
                // Check second statement is nested block
                match &statements[1].node {
                    Statement::StatementBlock(StatementBlock { statements: nested_statements }) => {
                        assert_eq!(nested_statements.len(), 1);
                        match &nested_statements[0].node {
                            Statement::Assignment { target, .. } => {
                                assert_eq!(target, &Identifier::new("y".to_string()));
                            }
                            _ => panic!("Expected nested Assignment statement"),
                        }
                    }
                    _ => panic!("Expected nested StatementBlock"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.node),
        }
    }

    /// Test parsing block with mutable assignment
    #[test]
    fn test_parse_block_with_mutable_assignment() {
        let source = "{ x = 100; }";
        let tokens = create_tokens_from_source(source);
        let mut parser = Parser::new(&tokens);
        
        let result = parser.parse_statement();
        assert!(result.is_ok());
        
        let statement = result.unwrap();
        match &statement.node {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 1);
                match &statements[0].node {
                    Statement::MutableAssignment { target, .. } => {
                        // Target should be an identifier expression
                        match &target.node {
                            crate::ast::Expression::Literal(crate::ast::Literal::Identifier(id)) => {
                                assert_eq!(id, &Identifier::new("x".to_string()));
                            }
                            _ => panic!("Expected identifier in mutable assignment target"),
                        }
                    }
                    _ => panic!("Expected MutableAssignment statement"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.node),
        }
    }

    /// Test parsing block with if statement
    #[test]
    fn test_parse_block_with_if_statement() {
        let source = "{ if true { let x = 42; } }";
        let tokens = create_tokens_from_source(source);
        let mut parser = Parser::new(&tokens);
        
        let result = parser.parse_statement();
        assert!(result.is_ok());
        
        let statement = result.unwrap();
        match &statement.node {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 1);
                match &statements[0].node {
                    Statement::If { condition, on_true } => {
                        // Verify on_true is also a block
                        match &on_true.node {
                            Statement::StatementBlock(StatementBlock { statements: if_statements }) => {
                                assert_eq!(if_statements.len(), 1);
                            }
                            _ => panic!("Expected StatementBlock in if body"),
                        }
                    }
                    _ => panic!("Expected If statement"),
                }k
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.node),
        }
    }

    /// Test parsing block with mixed statement types
    #[test]
    fn test_parse_block_with_mixed_statements() {
        let source = "{ let x = 42; x = 100; if x { let y = true; } }";
        let tokens = create_tokens_from_source(source);
        let mut parser = Parser::new(&tokens);
        
        let result = parser.parse_statement();
        assert!(result.is_ok());
        
        let statement = result.unwrap();
        match &statement.node {
            Statement::StatementBlock(StatementBlock { statements }) => {
                assert_eq!(statements.len(), 3);
                
                // First: let assignment
                match &statements[0].node {
                    Statement::Assignment { target, .. } => {
                        assert_eq!(target, &Identifier::new("x".to_string()));
                    }
                    _ => panic!("Expected Assignment statement at index 0"),
                }
                
                // Second: mutable assignment
                match &statements[1].node {
                    Statement::MutableAssignment { .. } => {} // Just verify it's mutable assignment
                    _ => panic!("Expected MutableAssignment statement at index 1"),
                }
                
                // Third: if statement
                match &statements[2].node {
                    Statement::If { .. } => {} // Just verify it's if statement
                    _ => panic!("Expected If statement at index 2"),
                }
            }
            _ => panic!("Expected StatementBlock, got: {:?}", statement.node),
        }
    }
}
