// Tests for token parsing and keyword detection

use crate::Token;
use crate::token::Tokenizer;
use crate::token::token_type::{Keyword, TokenType};

fn extract_token_types(tokens: &[Token]) -> Vec<TokenType> {
    tokens.iter().map(|t| t.token_type.clone()).collect()
}

#[test]
fn test_parse_boolean_expressions() {
    let cases = [
        ("not true", vec![TokenType::Keyword(Keyword::Not), TokenType::Keyword(Keyword::True), TokenType::Eof]),
        ("true and false", vec![TokenType::Keyword(Keyword::True), TokenType::Keyword(Keyword::And), TokenType::Keyword(Keyword::False), TokenType::Eof]),
        ("true or false", vec![TokenType::Keyword(Keyword::True), TokenType::Keyword(Keyword::Or), TokenType::Keyword(Keyword::False), TokenType::Eof]),
        ("not (a or b)", vec![TokenType::Keyword(Keyword::Not), TokenType::LeftParen, TokenType::Identifier("a".to_string()), TokenType::Keyword(Keyword::Or), TokenType::Identifier("b".to_string()), TokenType::RightParen, TokenType::Eof]),
    ];
    for (src, expected) in cases.iter() {
        let tokens = Tokenizer::new(src).tokenize();
        let actual = extract_token_types(&tokens);
        assert_eq!(actual, *expected);
    }
}

#[test]
fn test_parse_statements() {
    let cases = [
        ("x = 1;", vec![TokenType::Identifier("x".to_string()), TokenType::Equal, TokenType::Integer(1), TokenType::Semicolon, TokenType::Eof]),
        ("debug(\"msg\");", vec![TokenType::Keyword(Keyword::Debug), TokenType::LeftParen, TokenType::String("msg".to_string()), TokenType::RightParen, TokenType::Semicolon, TokenType::Eof]),
        ("input(x, \"prompt\");", vec![TokenType::Keyword(Keyword::Input), TokenType::LeftParen, TokenType::Identifier("x".to_string()), TokenType::Comma, TokenType::String("prompt".to_string()), TokenType::RightParen, TokenType::Semicolon, TokenType::Eof]),
        ("{ x = 1; debug(\"ok\"); }", vec![TokenType::LeftBrace, TokenType::Identifier("x".to_string()), TokenType::Equal, TokenType::Integer(1), TokenType::Semicolon, TokenType::Keyword(Keyword::Debug), TokenType::LeftParen, TokenType::String("ok".to_string()), TokenType::RightParen, TokenType::Semicolon, TokenType::RightBrace, TokenType::Eof]),
    ];
    for (src, expected) in cases.iter() {
        let tokens = Tokenizer::new(src).tokenize();
        let actual = extract_token_types(&tokens);
        assert_eq!(actual, *expected);
    }
}

#[test]
fn test_parse_keywords() {
    let keywords = [
        "not", "while", "for", "or", "and", "true", "false", "let", "if", "else", "use", "debug",
        "input",
    ];
    for &kw in &keywords {
        let src = kw;
        let tokens = Tokenizer::new(src).tokenize();
        assert_eq!(
            tokens[0].token_type,
            TokenType::Keyword(Keyword::try_from(kw.to_string()).unwrap())
        );
    }
}

#[test]
fn test_parse_operators() {
    let src = "! != = == > >= < <=";
    let tokens = Tokenizer::new(src).tokenize();
    let expected = vec![
        TokenType::Bang,
        TokenType::BangEqual,
        TokenType::Equal,
        TokenType::EqualEqual,
        TokenType::Greater,
        TokenType::GreaterEqual,
        TokenType::Less,
        TokenType::LessEqual,
        TokenType::Eof,
    ];
    let actual = extract_token_types(&tokens);
    assert_eq!(actual, expected);
}

#[test]
fn test_parse_code_snippet_keywords() {
    let src = r#"let x = 42; if not x { debug input = "hello"; }"#;
    let tokens = Tokenizer::new(src).tokenize();
    let expected_keywords = vec![
        TokenType::Keyword(Keyword::Let),
        TokenType::Identifier("x".to_string()),
        TokenType::Equal,
        TokenType::Integer(42),
        TokenType::Semicolon,
        TokenType::Keyword(Keyword::If),
        TokenType::Keyword(Keyword::Not),
        TokenType::Identifier("x".to_string()),
        TokenType::LeftBrace,
        TokenType::Keyword(Keyword::Debug),
        TokenType::Keyword(Keyword::Input),
        TokenType::Equal,
        TokenType::String("hello".to_string()),
        TokenType::Semicolon,
        TokenType::RightBrace,
        TokenType::Eof,
    ];
    let actual = extract_token_types(&tokens);
    assert_eq!(actual, expected_keywords);
}
