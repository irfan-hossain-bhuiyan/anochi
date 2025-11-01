// Tests for token parsing and keyword detection

use crate::token::Tokenizer;
use crate::token::token_type::TokenType;
use num_bigint::BigInt;

#[test]
fn test_basic_tokenization() {
    // Just test that basic tokenization works with BigInt
    let test_cases = vec![
        "42",           // integer
        "true",         // keyword
        "x = 1;",       // simple statement
    ];

    for source in test_cases {
        let tokens = Tokenizer::new(source).tokenize();
        // Just verify it tokenizes without error (no Error tokens)
        for token in &tokens {
            if let TokenType::Error(_) = token.token_type {
                panic!("Tokenization failed for: {source}");
            }
        }
        
        // Verify we get some tokens
        assert!(!tokens.is_empty(), "No tokens generated for: {source}");
    }

    // Test that big integers work
    let tokens = Tokenizer::new("42").tokenize();
    if let TokenType::Integer(big_int) = &tokens[0].token_type {
        assert_eq!(*big_int, BigInt::from(42));
    } else {
        panic!("Expected integer token");
    }
}
