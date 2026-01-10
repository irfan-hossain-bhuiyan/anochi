use anochi::parser::Parser;
use anochi::token::tokenizer::Tokenizer;
use anochi::ast::ToStringTree;

fn main() {
    let source = r#"
    let x = 10 + 20 * 3;
    if x > 50 {
        debug(x);
    } else {
        debug(0);
    }
    "#;

    println!("Source Code:\n{}\n", source);

    let mut tokenizer = Tokenizer::new(source);
    let (tokens, errors) = tokenizer.tokenize();
    if !errors.is_empty() {
        eprintln!("Tokenization Errors: {:?}", errors);
        return;
    }
    let mut parser = Parser::new(&tokens);
    match parser.parse_statements() {
        Ok(ast) => {
            println!("AST Tree:\n{}", ast.to_string_tree());
        }
        Err(e) => {
            eprintln!("Parse Error: {:?}", e);
        }
    }
}
