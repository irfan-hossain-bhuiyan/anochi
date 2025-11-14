// Tests for token parsing and keyword detection

use crate::ast::Identifier;
use crate::token::Tokenizer;
use crate::token::token_type::{Keyword, TokenType};
use num_bigint::BigInt;

#[test]
fn test_basic_tokenization() {
    // Test basic tokenization with combined input
    let source = "42;\ntrue\nx = 1;";
   use Keyword::True; 
    let expected_type = vec![
        TokenType::Integer(BigInt::from(42)),
        TokenType::Semicolon,
        TokenType::Newline,
        TokenType::Keyword(True),
        TokenType::Newline,
        TokenType::Identifier(Identifier::new("x")),
        TokenType::Equal,
        TokenType::Integer(BigInt::from(1)),
        TokenType::Semicolon,
    ];
    
    let tokens = Tokenizer::new(source).tokenize();
    let actual: Vec<TokenType> = tokens.into_iter().map(|t| t.token_type.clone()).collect();
    
    assert_eq!(actual, expected_type);
}
