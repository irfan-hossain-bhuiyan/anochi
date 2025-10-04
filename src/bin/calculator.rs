//! Simple CLI calculator using the Anochi parser and VM.
use std::io::{self, Write};
use anochi::token::Tokenizer;
use anochi::parser::Parser;
use anochi::vm::tree_walk::Vm;

fn main() {
    println!("Anochi Calculator. Enter expressions, Ctrl+D to exit.");
    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        let bytes = stdin.read_line(&mut input).unwrap();
        if bytes == 0 { break; }
        let input = input.trim();
        if input.is_empty() { continue; }
        let tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let vm = Vm::new();
        match parser.parse_expression() {
            Some(expr_node) => match vm.evaluate(&expr_node) {
                Ok(result) => println!("= {result:?}"),
                Err(e) => println!("Error: {e}"),
            },
            None => println!("Parse error."),
        }
    }
}
