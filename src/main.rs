use anochi::code_runner::CodeRunner;
use anochi::vm::tree_walk::raylib_backend::RayLibBackend;
use rustyline::{Editor, DefaultEditor};
use rustyline::error::ReadlineError;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        run_repl();
    } else {
        run_file(&args[1]);
    }
}

fn run_file(filename: &str) {
    let code = if let Ok(content) = fs::read_to_string(filename) {
        content
    } else {
        eprintln!("Failed to read file: {}", filename);
        process::exit(1);
    };
    
    let mut runner = CodeRunner::new(RayLibBackend::new());
    
    runner.initialize().expect("Failed to initialize RayLib backend");
    
    runner.run_statements(&code).expect("Failed to run code");
}

fn run_repl() {
    println!("🎯 Anochi REPL v0.1.0");
    println!("📝 Multi-line mode: Type your code, then press Ctrl+D to execute");
    println!("🧹 Press Ctrl+C to clear current input");
    println!("🔄 Type 'reset' to reset the runtime state");
    println!("❓ Type 'help' for more commands");
    println!("🚪 Type 'exit' or 'quit' to exit\n");

    let mut rl: DefaultEditor = Editor::new().expect("Failed to create editor");
    let mut code_runner = CodeRunner::default();
    let mut accumulated_input = String::new();
    let mut line_number = 1;

    loop {
        let prompt = if accumulated_input.is_empty() {
            ">>> ".to_string()
        } else {
            format!("{line_number}| ")
        };

        match rl.readline(&prompt) {
            Ok(line) => {
                let trimmed = line.trim();
                match trimmed {
                    "exit" | "quit" => {
                        println!("👋 Goodbye!");
                        break;
                    }
                    "help" => {
                        print_help();
                        continue;
                    }
                    "reset" => {
                        code_runner = CodeRunner::default();
                        accumulated_input.clear();
                        line_number = 1;
                        println!("🔄 Runtime state reset");
                        continue;
                    }
                    "clear" => {
                        accumulated_input.clear();
                        line_number = 1;
                        println!("🧹 Input cleared");
                        continue;
                    }
                    _ => {}
                }

                if !accumulated_input.is_empty() {
                    accumulated_input.push('\n');
                }
                accumulated_input.push_str(&line);
                line_number += 1;
            }
            Err(ReadlineError::Interrupted) => {
                if !accumulated_input.is_empty() {
                    println!("🧹 Input cleared");
                    accumulated_input.clear();
                    line_number = 1;
                } else {
                    println!("👋 Goodbye!");
                    break;
                }
            }
            Err(ReadlineError::Eof) => {
                if !accumulated_input.trim().is_empty() {
                    execute_code(&mut code_runner, &accumulated_input);
                    let _ = rl.add_history_entry(&accumulated_input);
                    accumulated_input.clear();
                    line_number = 1;
                    println!();
                }
            }
            Err(err) => {
                println!("❌ Error reading input: {err}");
                break;
            }
        }
    }
}

fn execute_code(code_runner: &mut CodeRunner, input: &str) {
    println!("🔄 Executing:\n{input}");
    println!("{}", "─".repeat(50));

    let input = input.trim();
    
    match code_runner.run_statements(input) {
        Ok(_) => {
            println!("✅ Statement executed successfully");
        }
        Err(statement_err) => {
            match code_runner.evaluate_expr(input) {
                Ok(result) => {
                    println!("✅ Result: {result:?}");
                }
                Err(expr_err) => {
                    println!("❌ Statement error: {statement_err:?}");
                    println!("❌ Expression error: {expr_err:?}");
                }
            }
        }
    }
    
    println!("{}", "─".repeat(50));
}

fn print_help() {
    println!("{}", "═".repeat(60));
    println!("🎯 Anochi REPL Help");
    println!("{}", "═".repeat(60));
    println!("📝 Multi-line input:");
    println!("   - Type code across multiple lines");
    println!("   - Press Ctrl+D to execute");
    println!();
    println!("🔧 Commands:");
    println!("   exit, quit    - Exit the REPL");
    println!("   reset         - Reset runtime state (clear all variables)");
    println!("   clear         - Clear current input buffer");
    println!("   help          - Show this help");
    println!();
    println!("⌨️  Shortcuts:");
    println!("   Ctrl+D        - Execute current input");
    println!("   Ctrl+C        - Clear current input (or exit if empty)");
    println!();
    println!("💡 Examples:");
    println!("   let x = 42;");
    println!("   let y = x * 2;");
    println!("   x + y");
    println!("   if (x > 40) {{ y = 100; }}");
    println!("{}", "═".repeat(60));
}

