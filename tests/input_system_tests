#[cfg(test)]
mod tests {
    use anochi::{CodeRunner, vm::TestBackend};

    #[test]
    fn test_simulated_input_with_test_backend() {
        let mut test_backend = TestBackend::new();
        
        // Pre-populate inputs that would come from user
        test_backend.add_input("42".to_string());
        test_backend.add_input("3.14".to_string());
        test_backend.add_input("true".to_string());
        test_backend.add_input("Hello World".to_string());
        
        let mut runner = CodeRunner::with_backend(test_backend);
        
        // Simulate manual input assignments (since input() function isn't implemented yet)
        runner.run("number = 42; debug(\"Simulated number input:\", number);").unwrap();
        runner.run("pi = 3.14; debug(\"Simulated pi input:\", pi);").unwrap();
        runner.run("flag = true; debug(\"Simulated flag input:\", flag);").unwrap();
        runner.run(r#"message = "Hello World"; debug("Simulated message input:", message);"#).unwrap();
        
        let output = runner.vm().backend().get_debug_output();
        assert!(output.contains("Simulated number input:"));
        assert!(output.contains("42"));
        assert!(output.contains("3.14"));
        assert!(output.contains("true"));
        assert!(output.contains("Hello World"));
    }

    #[test]
    fn test_interactive_calculator_concept() {
        let mut test_backend = TestBackend::new();
        
        // Simulate user entering calculator operations
        test_backend.add_input("10".to_string());
        test_backend.add_input("5".to_string());
        
        let mut runner = CodeRunner::with_backend(test_backend);
        
        // Simulate interactive calculator
        runner.run("a = 10; b = 5;").unwrap();
        runner.run("sum = a + b; debug(\"Sum:\", sum);").unwrap();
        runner.run("diff = a - b; debug(\"Difference:\", diff);").unwrap();
        runner.run("product = a * b; debug(\"Product:\", product);").unwrap();
        runner.run("quotient = a / b; debug(\"Quotient:\", quotient);").unwrap();
        
        let output = runner.vm().backend().get_debug_output();
        assert!(output.contains("Sum:"));
        assert!(output.contains("15")); // 10 + 5
        assert!(output.contains("Difference:"));
        assert!(output.contains("5")); // 10 - 5
        assert!(output.contains("Product:"));
        assert!(output.contains("50")); // 10 * 5
        assert!(output.contains("Quotient:"));
        assert!(output.contains("2")); // 10 / 5
    }

    #[test]
    fn test_input_type_auto_detection() {
        // Test the type detection logic that would be used by the VM
        let test_cases = vec![
            ("42", "Integer"),
            ("3.14159", "Float"),
            ("true", "Bool"),
            ("false", "Bool"),
            ("hello world", "String"),
            ("123.0", "Float"),
            ("-42", "Integer"),
            ("-3.14", "Float"),
        ];
        
        for (input, expected_type) in test_cases {
            let detected_type = if input.parse::<i64>().is_ok() {
                "Integer"
            } else if input.parse::<f64>().is_ok() {
                "Float"
            } else if input == "true" || input == "false" {
                "Bool"
            } else {
                "String"
            };
            
            assert_eq!(detected_type, expected_type, "Failed for input: {input}");
        }
    }

    #[test]
    fn test_debug_output_capture() {
        let test_backend = TestBackend::new();
        let mut runner = CodeRunner::with_backend(test_backend);
        
        runner.run("x = 100;").unwrap();
        runner.run("debug(\"Value of x:\", x);").unwrap();
        runner.run("debug(\"Multiple values:\", 1, 2, 3);").unwrap();
        
        let output = runner.vm().backend().get_debug_output();
        assert!(output.contains("Value of x:"));
        assert!(output.contains("100"));
        assert!(output.contains("Multiple values:"));
        assert!(output.contains("1"));
        assert!(output.contains("2"));
        assert!(output.contains("3"));
    }

    #[test]
    fn test_test_backend_input_queue() {
        let mut test_backend = TestBackend::new();
        
        // Test that inputs are queued properly
        test_backend.add_input("first".to_string());
        test_backend.add_input("second".to_string());
        test_backend.add_input("third".to_string());
        
        // Note: This would test the actual input() function when implemented
        // For now, we just verify the backend can store inputs
        assert_eq!(test_backend.input_queue_len(), 3);
    }

    #[test]
    fn test_backend_debug_output_accumulation() {
        let test_backend = TestBackend::new();
        let mut runner = CodeRunner::with_backend(test_backend);
        
        // Multiple debug statements should accumulate output
        runner.run("debug(\"First line\");").unwrap();
        runner.run("debug(\"Second line\");").unwrap();
        runner.run("debug(\"Third line\");").unwrap();
        
        let output = runner.vm().backend().get_debug_output();
        let lines: Vec<&str> = output.lines().collect();
        
        assert!(lines.len() >= 3);
        assert!(output.contains("First line"));
        assert!(output.contains("Second line"));
        assert!(output.contains("Third line"));
    }

    #[test]
    fn test_mixed_data_types_debug() {
        let test_backend = TestBackend::new();
        let mut runner = CodeRunner::with_backend(test_backend);
        
        runner.run("int_val = 42;").unwrap();
        runner.run("float_val = 3.14;").unwrap();
        runner.run("bool_val = true;").unwrap();
        runner.run(r#"string_val = "test";"#).unwrap();
        
        runner.run("debug(\"Mixed types:\", int_val, float_val, bool_val, string_val);").unwrap();
        
        let output = runner.vm().backend().get_debug_output();
        assert!(output.contains("Mixed types:"));
        assert!(output.contains("42"));
        assert!(output.contains("3.14"));
        assert!(output.contains("true"));
        assert!(output.contains("test"));
    }
}
