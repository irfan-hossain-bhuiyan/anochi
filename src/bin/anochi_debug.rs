use std::env;
use std::fs;
use std::process;

use anochi::parser::Parser;
use anochi::token::tokenizer::Tokenizer;
use anochi::ast::ToStringTree;
use anochi::vm::tree_walk::Vm;
use anochi::vm::backend::IoBackend;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <source_file>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let source = fs::read_to_string(filename).unwrap_or_else(|err| {
        eprintln!("Error reading file {}: {}", filename, err);
        process::exit(1);
    });

    println!("--- Source Code ---");
    println!("{}", source);

    println!("\n--- Tokenization ---");
    let mut tokenizer = Tokenizer::new(&source);
    let (tokens, errors) = tokenizer.tokenize();
    
    if !errors.is_empty() {
         eprintln!("Tokenization Failed:");
         println!("{}", errors.err_str(&source));
         process::exit(1);
    }

    for (i, token) in tokens.iter().enumerate() {
        println!("{}: {:?}", i, token);
    }

    println!("\n--- Parsing ---");
    let mut parser = Parser::new(&tokens);
    let ast_result = parser.parse_statements();
    
    match ast_result {
        Ok(ast) => {
            println!("AST Tree:\n{}", ast.to_string_tree());
            
            println!("\n--- Execution ---");
            let mut vm = Vm::new(IoBackend::default());
            if let Err(e) = vm.initialize() {
                eprintln!("VM Initialization Error: {:?}", e);
                process::exit(1);
            }
            
            // execute_statement returns Result<StatementEvent, VmError>
            match vm.execute_statement(&ast) {
                Ok(_) => println!("\nExecution Completed Successfully."),
                Err(e) => {
                     eprintln!("Runtime Error: {:?}", e);
                }
            }
        }
        Err(e) => {
            eprintln!("Parse Error: {:?}", e);
            process::exit(1);
        }
    }
}
