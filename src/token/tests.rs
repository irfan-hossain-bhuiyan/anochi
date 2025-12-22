// Tests for token parsing and keyword detection

use crate::ast::Identifier;
use crate::token::Tokenizer;
use crate::token::token_type::{Keyword, TokenType};
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::FromPrimitive;

#[test]
fn test_basic_tokenization() {
    // Test basic tokenization with combined input
    let source = "42;\ntrue\nx = 1;";
   use Keyword::True; 
    let expected_type = vec![
        TokenType::Integer(BigInt::from(42)),
        TokenType::Semicolon,
        TokenType::Keyword(True),
        TokenType::Identifier(Identifier::new("x")),
        TokenType::Equal,
        TokenType::Integer(BigInt::from(1)),
        TokenType::Semicolon,
    ];
    
    let (tokens, _errors) = Tokenizer::new(source).tokenize();
    let actual: Vec<TokenType> = tokens.into_iter().map(|t| t.token_type.clone()).collect();
    
    assert_eq!(actual, expected_type);
}

#[test]
fn test_tokenizer_for_expression_statement_with_params() {
    let source = "{ add!{x=5, y=10.0}; }";
    let (tokens, _errors) = Tokenizer::new(source).tokenize();
    let expected = vec![
        TokenType::LeftBrace,
        TokenType::Identifier(Identifier::new("add")),
        TokenType::Bang,
        TokenType::LeftBrace,
        TokenType::Identifier(Identifier::new("x")),
        TokenType::Equal,
        TokenType::Integer(BigInt::from(5)),
        TokenType::Comma,
        TokenType::Identifier(Identifier::new("y")),
        TokenType::Equal,
        TokenType::Float(BigRational::from_f64(10.0).unwrap()),
        TokenType::RightBrace,
        TokenType::Semicolon,
        TokenType::RightBrace,
    ];
    let actual: Vec<TokenType> = tokens.into_iter().map(|t| t.token_type.clone()).collect();
    assert_eq!(actual, expected);
}


