#[cfg(test)]
mod tests {
    use anochi::{CodeRunner, ast::Literal};

    #[test]
    fn test_comprehensive_operations() {
        let mut runner = CodeRunner::default();
        // Test arithmetic, variables, and complex expressions in one test
        runner.run_statement("a = 15;").unwrap();
        runner.run_statement("b = 3;").unwrap();
        let result = runner.evaluate_expr("(a + b) * 2 - 5").unwrap();
        assert_eq!(result, Literal::Integer(31));
        
        // Test boolean and comparison operations
        let result = runner.evaluate_expr("a > b and b < 10").unwrap();
        assert_eq!(result, Literal::Bool(true));
    }

    #[test]
    fn test_mixed_data_types() {
        let mut runner = CodeRunner::default();
        // Test float, string, and integer operations together
        let result = runner.evaluate_expr("3.14 + 2.86").unwrap();
        assert_eq!(result, Literal::Float(6.0));
        
        runner.run_statement(r#"message = "Hello World";"#).unwrap();
        let result = runner.evaluate_expr(r#""test string""#).unwrap();
        assert_eq!(result, Literal::String("test string".to_string()));
        
        // Test operator precedence
        let result = runner.evaluate_expr("(2 + 3) * (4 - 1)").unwrap();
        assert_eq!(result, Literal::Integer(15));
    }

    #[test]
    fn test_if_statement() {
        let mut runner = CodeRunner::default();
        runner.run_statement("x = 0;").unwrap();
        runner.run_statement("if (2 > 1) { x = 42; }").unwrap();
        let result = runner.evaluate_expr("x").unwrap();
        assert_eq!(result, Literal::Integer(42));
    }

    #[test]
    fn test_if_else_and_nested_statements() {
        let mut runner = CodeRunner::default();
        // Test if-else
        runner.run_statement("y = 0;").unwrap();
        runner.run_statement("if (1 > 2) { y = 100; } else { y = 200; }").unwrap();
        let result = runner.evaluate_expr("y").unwrap();
        assert_eq!(result, Literal::Integer(200));
        
        // Test nested if statements
        runner.run_statement("result = 0;").unwrap();
        let nested_code = "if (true) { if (10 > 5) { result = 999; } }";
        runner.run_statement(nested_code).unwrap();
        let result = runner.evaluate_expr("result").unwrap();
        assert_eq!(result, Literal::Integer(999));
    }
}
