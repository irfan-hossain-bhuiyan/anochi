//! Backend abstraction for VM I/O operations.
//!
//! This module provides a flexible backend system that allows the VM to perform
//! I/O operations (debug output, user input, etc.) through different implementations
//! without being tied to specific I/O mechanisms.

use std::io::{self, Write};
use std::fs::OpenOptions;
use std::path::Path;

/// Result type for backend operations
pub type BackendResult<T> = Result<T, BackendError>;

/// Errors that can occur in backend operations
#[derive(Debug, thiserror::Error)]
pub enum BackendError {
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    #[error("Backend not available: {0}")]
    NotAvailable(String),
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
}

/// Trait defining the backend interface for VM I/O operations
pub trait VmBackend: std::fmt::Debug {
    /// Print debug output
    fn debug_print(&mut self, message: &str) -> BackendResult<()>;
    
    /// Read user input (returns None if input not supported)
    fn read_input(&mut self, prompt: Option<&str>) -> BackendResult<Option<String>>;
    
    /// Print regular output
    fn print(&mut self, message: &str) -> BackendResult<()>;
    
    /// Print error messages
    fn print_error(&mut self, message: &str) -> BackendResult<()>;
    
    /// Flush any pending output
    fn flush(&mut self) -> BackendResult<()>;
    
    /// Check if input is available
    fn has_input(&self) -> bool;
    
    /// Check if output is available
    fn has_output(&self) -> bool;
}


/// Unified backend for file or console I/O
#[derive(Debug,Default)]
pub struct IoBackend {
    output_file: Option<std::fs::File>,
    input_file: Option<std::fs::File>,
    input_buffer: Vec<String>,
}

impl IoBackend {
    pub fn new() -> Self {
        Self {
            output_file: None,
            input_file: None,
            input_buffer: Vec::new(),
        }
    }

    /// Set output file
    pub fn with_output_file<P: AsRef<Path>>(mut self, output: P) -> BackendResult<Self> {
        let output_file = OpenOptions::new().create(true).append(true).open(output)?;
        self.output_file = Some(output_file);
        Ok(self)
    }

    /// Set input file
    pub fn with_input_file<P: AsRef<Path>>(mut self, input: P) -> BackendResult<Self> {
        let input_file = OpenOptions::new().read(true).open(input)?;
        self.input_file = Some(input_file);
        Ok(self)
    }
}

impl VmBackend for IoBackend {
    fn debug_print(&mut self, message: &str) -> BackendResult<()> {
        self.print(message)
    }

    fn read_input(&mut self, prompt: Option<&str>) -> BackendResult<Option<String>> {
        if let Some(ref mut input_file) = self.input_file {
            // If buffer is empty, fill it with next chunk of tokens
            if self.input_buffer.is_empty() {
                use std::io::Read;
                let mut buf = String::new();
                let n = input_file.read_to_string(&mut buf)?;
                if n == 0 {
                    return Ok(None);
                }
                self.input_buffer = buf.split_whitespace().map(|s| s.to_string()).collect();
            }
            // Pop next token
            if let Some(token) = self.input_buffer.first() {
                let token = token.clone();
                self.input_buffer.remove(0);
                Ok(Some(token))
            } else {
                Ok(None)
            }
        } else {
            if let Some(prompt) = prompt {
                print!("{prompt}");
                io::stdout().flush()?;
            }
            let stdin = io::stdin();
            let mut line = String::new();
            match stdin.read_line(&mut line) {
                Ok(_) => {
                    if line.ends_with('\n') { line.pop(); }
                    if line.ends_with('\r') { line.pop(); }
                    Ok(Some(line))
                }
                Err(e) => Err(BackendError::Io(e)),
            }
        }
    }

    fn print(&mut self, message: &str) -> BackendResult<()> {
        if let Some(ref mut file) = self.output_file {
            writeln!(file, "{message}")?;
            file.flush()?;
        } else {
            println!("{message}");
        }
        Ok(())
    }

    fn print_error(&mut self, message: &str) -> BackendResult<()> {
        if let Some(ref mut file) = self.output_file {
            writeln!(file, "{message}")?;
            file.flush()?;
        } else {
            eprintln!("{message}");
        }
        Ok(())
    }

    fn flush(&mut self) -> BackendResult<()> {
        if let Some(ref mut file) = self.output_file {
            file.flush()?;
        } else {
            io::stdout().flush()?;
            io::stderr().flush()?;
        }
        Ok(())
    }

    fn has_input(&self) -> bool {
        self.input_file.is_some()
    }

    fn has_output(&self) -> bool {
        self.output_file.is_some()
    }
}

/// Test backend that captures output for testing
#[derive(Debug, Default)]
pub struct TestBackend {
    pub debug_output: Vec<String>,
    pub output: Vec<String>,
    pub error_output: Vec<String>,
    pub input_queue: Vec<String>,
    pub debug_enabled: bool,
}

impl TestBackend {
    pub fn new() -> Self {
        Self {
            debug_output: Vec::new(),
            output: Vec::new(),
            error_output: Vec::new(),
            input_queue: Vec::new(),
            debug_enabled: true,
        }
    }
    
    /// Add input to be returned by read_input calls
    pub fn add_input(&mut self, input: String) {
        self.input_queue.push(input);
    }
    
    /// Get all debug output as a single string
    pub fn get_debug_output(&self) -> String {
        self.debug_output.join("\n")
    }
    
    /// Get all regular output as a single string
    pub fn get_output(&self) -> String {
        self.output.join("\n")
    }
    
    /// Get all error output as a single string
    pub fn get_error_output(&self) -> String {
        self.error_output.join("\n")
    }
    
    /// Clear all captured output
    pub fn clear(&mut self) {
        self.debug_output.clear();
        self.output.clear();
        self.error_output.clear();
    }
    
    /// Get the number of inputs in the queue
    pub fn input_queue_len(&self) -> usize {
        self.input_queue.len()
    }
}

impl VmBackend for TestBackend {
    fn debug_print(&mut self, message: &str) -> BackendResult<()> {
        if self.debug_enabled {
            self.debug_output.push(message.to_string());
        }
        Ok(())
    }
    
    fn read_input(&mut self, _prompt: Option<&str>) -> BackendResult<Option<String>> {
        Ok(self.input_queue.pop())
    }
    
    fn print(&mut self, message: &str) -> BackendResult<()> {
        self.output.push(message.to_string());
        Ok(())
    }
    
    fn print_error(&mut self, message: &str) -> BackendResult<()> {
        self.error_output.push(message.to_string());
        Ok(())
    }
    
    fn flush(&mut self) -> BackendResult<()> {
        // Nothing to flush for test backend
        Ok(())
    }
    
    fn has_input(&self) -> bool {
        !self.input_queue.is_empty()
    }
    
    fn has_output(&self) -> bool {
        true
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    
    #[test]
    fn test_io_backend_console() {
        let mut backend = IoBackend::new();
        backend.print("test output").unwrap();
        backend.print_error("test error").unwrap();
        backend.flush().unwrap();
        assert!(!backend.has_input());
        assert!(!backend.has_output());
    }
    
    #[test]
    fn test_test_backend() {
        let mut backend = TestBackend::new();
        
        backend.debug_print("debug1").unwrap();
        backend.debug_print("debug2").unwrap();
        backend.print("output1").unwrap();
        backend.print_error("error1").unwrap();
        
        assert_eq!(backend.get_debug_output(), "debug1\ndebug2");
        assert_eq!(backend.get_output(), "output1");
        assert_eq!(backend.get_error_output(), "error1");
        
        backend.add_input("test input".to_string());
        let input = backend.read_input(Some("prompt: ")).unwrap();
        assert_eq!(input, Some("test input".to_string()));
        
        // No more input
        let input = backend.read_input(None).unwrap();
        assert_eq!(input, None);
    }
    
    #[test]
    fn test_io_backend_file() -> Result<(), Box<dyn std::error::Error>> {
        let temp_dir = std::env::temp_dir();
        let output_file = temp_dir.join("test_output.log");
        let input_file = temp_dir.join("test_input.txt");

        let _ = fs::remove_file(&output_file);
        let _ = fs::remove_file(&input_file);

        fs::write(&input_file, b"foo bar baz 123\nhello world")?;

        let mut backend = IoBackend::new().with_output_file(&output_file)?;
        backend = backend.with_input_file(&input_file)?;
        backend.print("test output message").unwrap();
        backend.print_error("test error message").unwrap();
        backend.flush().unwrap();

        let output_content = fs::read_to_string(&output_file)?;
        assert!(output_content.contains("test output message"));
        assert!(output_content.contains("test error message"));

        // Input file token reading
        assert_eq!(backend.read_input(None).unwrap(), Some("foo".to_string()));
        assert_eq!(backend.read_input(None).unwrap(), Some("bar".to_string()));
        assert_eq!(backend.read_input(None).unwrap(), Some("baz".to_string()));
        assert_eq!(backend.read_input(None).unwrap(), Some("123".to_string()));
        assert_eq!(backend.read_input(None).unwrap(), Some("hello".to_string()));
        assert_eq!(backend.read_input(None).unwrap(), Some("world".to_string()));
        assert_eq!(backend.read_input(None).unwrap(), None);
        assert!(backend.has_input());
        assert!(backend.has_output());

        let _ = fs::remove_file(&output_file);
        let _ = fs::remove_file(&input_file);
        Ok(())
    }
    
}
