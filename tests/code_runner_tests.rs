#[cfg(test)]
mod tests {
    use anochi::{CodeRunner, ast::Literal};

    #[test]
    fn test_simple_arithmetic() {
        let mut runner = CodeRunner::default();
        let result = runner.run("2 + 3 * 4").unwrap();
        assert_eq!(result, "14");
    }

    #[test]
    fn test_variable_assignment_and_usage() {
        let mut runner = CodeRunner::default();
        runner.run("x = 10;").unwrap();
        runner.run("y = 5;").unwrap();
        let result = runner.run("x + y * 2").unwrap();
        assert_eq!(result, "20");
    }

    #[test]
    fn test_boolean_operations() {
        let mut runner = CodeRunner::default();
        let result = runner.run("true and false").unwrap();
        assert_eq!(result, "false");
        let result = runner.run("true or false").unwrap();
        assert_eq!(result, "true");
        let result = runner.run("not false").unwrap();
        assert_eq!(result, "true");
    }

    #[test]
    fn test_comparison_operations() {
        let mut runner = CodeRunner::default();
        let result = runner.run("10 > 5").unwrap();
        assert_eq!(result, "true");
        let result = runner.run("3 == 3").unwrap();
        assert_eq!(result, "true");
        let result = runner.run("7 <= 10").unwrap();
        assert_eq!(result, "true");
    }

    #[test]
    fn test_complex_expressions_with_variables() {
        let mut runner = CodeRunner::default();
        runner.run("a = 15;").unwrap();
        runner.run("b = 3;").unwrap();
        let result = runner.run("(a + b) * 2 - 5").unwrap();
        assert_eq!(result, "31");
    }

    #[test]
    fn test_debug_statements() {
        let mut runner = CodeRunner::default();
        // Debug statements should not fail
        let result = runner.run("debug(42, 3.14, true, \"hello\");").unwrap();
        assert_eq!(result, "()");
    }

    #[test]
    fn test_statement_blocks() {
        let mut runner = CodeRunner::default();
        runner.run("{ local_var = 100; result = local_var + 50; debug(result); }").unwrap();
        // Variables from blocks should be accessible
        let result = runner.run("result").unwrap();
        assert_eq!(result, "150");
    }

    #[test]
    fn test_float_operations() {
        let mut runner = CodeRunner::default();
        let result = runner.run("3.14 + 2.86").unwrap();
        assert_eq!(result, "6");
        let result = runner.run("10.5 / 2.5").unwrap();
        assert_eq!(result, "4.2");
    }

    #[test]
    fn test_multiple_variable_operations() {
        let mut runner = CodeRunner::default();
        runner.run("x = 42;").unwrap();
        runner.run("y = 8;").unwrap();
        runner.run("z = x + y;").unwrap();
        let result = runner.run("z").unwrap();
        assert_eq!(result, "50");
        let result = runner.run("x * y / 2").unwrap();
        assert_eq!(result, "168");
    }

    #[test]
    fn test_string_literals() {
        let mut runner = CodeRunner::default();
        runner.run(r#"message = "Hello World";"#).unwrap();
        runner.run(r#"debug("Message:", message);"#).unwrap();
        // Debug should work with string literals
        let result = runner.run(r#""test string""#).unwrap();
        assert_eq!(result, "test string");
    }

    #[test]
    fn test_complex_arithmetic_with_precedence() {
        let mut runner = CodeRunner::default();
        // Test operator precedence
        let result = runner.run("2 + 3 * 4 - 1").unwrap();
        assert_eq!(result, "13");
        let result = runner.run("(2 + 3) * (4 - 1)").unwrap();
        assert_eq!(result, "15");
        let result = runner.run("10 / 2 + 3 * 2").unwrap();
        assert_eq!(result, "11");
    }

    #[test]
    fn test_nested_expressions() {
        let mut runner = CodeRunner::default();
        runner.run("a = 2;").unwrap();
        runner.run("b = 3;").unwrap();
        runner.run("c = 4;").unwrap();
        let result = runner.run("a + b * c - (a + b)").unwrap();
        assert_eq!(result, "9"); // 2 + 12 - 5 = 9
    }

    #[test]
    fn test_if_statement() {
        let mut runner = CodeRunner::default();
        runner.run("x = 0;").unwrap();
        runner.run("if (2 > 1) { x = 42; }").unwrap();
        let result = runner.run("x").unwrap();
        assert_eq!(result, "42");
    }

    #[test]
    fn test_if_else_statement() {
        let mut runner = CodeRunner::default();
        runner.run("y = 0;").unwrap();
        runner.run("if (1 > 2) { y = 100; } else { y = 200; }").unwrap();
        let result = runner.run("y").unwrap();
        assert_eq!(result, "200");
    }

    #[test]
    fn test_multiline_if_else_repl_simulation() {
        let mut runner = CodeRunner::default();
        // Simulate REPL multiline input for if-else
        runner.run("z = 0;").unwrap();
        // Test that multiline if-else works as expected
        let multiline_code = "if (5 > 3) { z = 42; debug(\"condition true\"); } else { z = 24; debug(\"condition false\"); }";
        runner.run(multiline_code).unwrap();
        let result = runner.run("z").unwrap();
        assert_eq!(result, "42");
    }

    #[test]
    fn test_nested_if_statements() {
        let mut runner = CodeRunner::default();
        runner.run("result = 0;").unwrap();
        let nested_code = "if (true) { if (10 > 5) { result = 999; } }";
        runner.run(nested_code).unwrap();
        let result = runner.run("result").unwrap();
        assert_eq!(result, "999");
    }
}
