use std::env;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    
    let mut runner = CodeRunner::new();

    match args.len() {
        // Interactive mode
        1 => {
            
        }
        
        // File execution mode
        2 => {
            let filename = &args[1];
            println!("📁 Running file: {filename}");
            
            let source = fs::read_to_string(filename)?;
            match runner.run(&source) {
                Ok(()) => println!("✅ Execution completed successfully."),
                Err(e) => {
                    eprintln!("❌ Execution failed: {e}");
                    std::process::exit(1);
                }
            }
        }
        
        // Invalid usage
        _ => {
            eprintln!("Usage:");
            eprintln!("  {} [file.anochi]     # Run a file", args[0]);
            eprintln!("  {}                   # Interactive mode", args[0]);
            std::process::exit(1);
        }
    }
    
    Ok(())
}
