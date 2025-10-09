#[cfg(test)]
mod tests {
    use anochi::{CodeRunner, ast::Literal};

    #[test]
    fn test_simple_arithmetic() {
        let runner = CodeRunner::new();
        let result = runner.eval_expression("2 + 3 * 4").unwrap();
        assert_eq!(result, Literal::Integer(14));
    }

    #[test]
    fn test_variable_assignment_and_usage() {
        let mut runner = CodeRunner::new();
        
        runner.run("x = 10;").unwrap();
        runner.run("y = 5;").unwrap();
        let result = runner.eval_expression("x + y * 2").unwrap();
        assert_eq!(result, Literal::Integer(20));
    }

    #[test]
    fn test_boolean_operations() {
        let runner = CodeRunner::new();
        
        let result = runner.eval_expression("true and false").unwrap();
        assert_eq!(result, Literal::Bool(false));
        
        let result = runner.eval_expression("true or false").unwrap();
        assert_eq!(result, Literal::Bool(true));
        
        let result = runner.eval_expression("not false").unwrap();
        assert_eq!(result, Literal::Bool(true));
    }

    #[test]
    fn test_comparison_operations() {
        let runner = CodeRunner::new();
        
        let result = runner.eval_expression("10 > 5").unwrap();
        assert_eq!(result, Literal::Bool(true));
        
        let result = runner.eval_expression("3 == 3").unwrap();
        assert_eq!(result, Literal::Bool(true));
        
        let result = runner.eval_expression("7 <= 10").unwrap();
        assert_eq!(result, Literal::Bool(true));
    }

    #[test]
    fn test_complex_expressions_with_variables() {
        let mut runner = CodeRunner::new();
        
        runner.run("a = 15;").unwrap();
        runner.run("b = 3;").unwrap();
        let result = runner.eval_expression("(a + b) * 2 - 5").unwrap();
        assert_eq!(result, Literal::Integer(31));
    }

    #[test]
    fn test_debug_statements() {
        let mut runner = CodeRunner::new();
        
        // Debug statements should not fail
        runner.run("debug(42, 3.14, true, \"hello\");").unwrap();
    }

    #[test]
    fn test_statement_blocks() {
        let mut runner = CodeRunner::new();
        
        runner.run("{ local_var = 100; result = local_var + 50; debug(result); }").unwrap();
        
        // Variables from blocks should be accessible
        let result = runner.eval_expression("result").unwrap();
        assert_eq!(result, Literal::Integer(150));
    }

    #[test]
    fn test_float_operations() {
        let runner = CodeRunner::new();
        
        let result = runner.eval_expression("3.14 + 2.86").unwrap();
        assert_eq!(result, Literal::Float(6.0));
        
        let result = runner.eval_expression("10.5 / 2.5").unwrap();
        assert_eq!(result, Literal::Float(4.2));
    }

    #[test]
    fn test_multiple_variable_operations() {
        let mut runner = CodeRunner::new();
        
        runner.run("x = 42;").unwrap();
        runner.run("y = 8;").unwrap();
        runner.run("z = x + y;").unwrap();
        
        let result = runner.eval_expression("z").unwrap();
        assert_eq!(result, Literal::Integer(50));
        
        let result = runner.eval_expression("x * y / 2").unwrap();
        assert_eq!(result, Literal::Integer(168));
    }

    #[test]
    fn test_string_literals() {
        let mut runner = CodeRunner::new();
        
        runner.run(r#"message = "Hello World";"#).unwrap();
        runner.run(r#"debug("Message:", message);"#).unwrap();
        
        // Debug should work with string literals
        let result = runner.eval_expression(r#""test string""#).unwrap();
        assert_eq!(result, Literal::String("test string".to_string()));
    }

    #[test]
    fn test_complex_arithmetic_with_precedence() {
        let runner = CodeRunner::new();
        
        // Test operator precedence
        let result = runner.eval_expression("2 + 3 * 4 - 1").unwrap();
        assert_eq!(result, Literal::Integer(13));
        
        let result = runner.eval_expression("(2 + 3) * (4 - 1)").unwrap();
        assert_eq!(result, Literal::Integer(15));
        
        let result = runner.eval_expression("10 / 2 + 3 * 2").unwrap();
        assert_eq!(result, Literal::Integer(11));
    }

    #[test]
    fn test_nested_expressions() {
        let mut runner = CodeRunner::new();
        
        runner.run("a = 2;").unwrap();
        runner.run("b = 3;").unwrap();
        runner.run("c = 4;").unwrap();
        
        let result = runner.eval_expression("a + b * c - (a + b)").unwrap();
        assert_eq!(result, Literal::Integer(9)); // 2 + 12 - 5 = 9
    }

    #[test]
    fn test_if_statement() {
        let mut runner = CodeRunner::new();
        runner.run("x = 0;").unwrap();
        runner.run("if (2 > 1) { x = 42; }").unwrap();
        let result = runner.eval_expression("x").unwrap();
        assert_eq!(result, Literal::Integer(42));
    }

    #[test]
    fn test_if_else_statement() {
        let mut runner = CodeRunner::new();
        runner.run("y = 0;").unwrap();
        runner.run("if (1 > 2) { y = 100; } else { y = 200; }").unwrap();
        let result = runner.eval_expression("y").unwrap();
        assert_eq!(result, Literal::Integer(200));
    }

    #[test]
    fn test_multiline_if_else_repl_simulation() {
        let mut runner = CodeRunner::new();
        
        // Simulate REPL multiline input for if-else
        runner.run("z = 0;").unwrap();
        
        // Test that multiline if-else works as expected
        let multiline_code = "if (5 > 3) { z = 42; debug(\"condition true\"); } else { z = 24; debug(\"condition false\"); }";
        
        runner.run(multiline_code).unwrap();
        let result = runner.eval_expression("z").unwrap();
        assert_eq!(result, Literal::Integer(42));
    }

    #[test]
    fn test_nested_if_statements() {
        let mut runner = CodeRunner::new();
        runner.run("result = 0;").unwrap();
        
        let nested_code = "if (true) { if (10 > 5) { result = 999; } }";
        
        runner.run(nested_code).unwrap();
        let result = runner.eval_expression("result").unwrap();
        assert_eq!(result, Literal::Integer(999));
    }
}
