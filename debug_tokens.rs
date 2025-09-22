use anochi::token::{Tokenizer, TokenType};

fn main() {
    // Test 1: Mixed quotes - "valid" invalid" "another valid"
    println!("=== Test 1: Mixed Quotes ===");
    let tokenizer1 = Tokenizer::new("\"valid\" invalid\" \"another valid\"");
    let tokens1 = tokenizer1.tokenize();

    println!("Input: \"valid\" invalid\" \"another valid\"");
    println!("Tokens produced:");
    for (i, token) in tokens1.iter().enumerate() {
        println!("  {}: {:?}", i, token.token_type);
    }

    // Test 2: String with newline - "unterminated\nstring"
    println!("\n=== Test 2: String with Newline ===");
    let tokenizer2 = Tokenizer::new("\"unterminated\nstring\"");
    let tokens2 = tokenizer2.tokenize();

    println!("Input: \"unterminated\\nstring\"");
    println!("Tokens produced:");
    for (i, token) in tokens2.iter().enumerate() {
        println!("  {}: {:?}", i, token.token_type);
    }
}
