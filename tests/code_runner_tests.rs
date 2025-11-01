#[cfg(test)]
mod tests {
    use anochi::CodeRunner;

    #[test]
    fn test_basic_functionality() {
        let mut runner = CodeRunner::default();
        
        // Test basic arithmetic and variables
        runner.run_statement("a = 15;").unwrap();
        runner.run_statement("b = 3;").unwrap();
        let result = runner.evaluate_expr("(a + b) * 2 - 5").unwrap();
        assert_eq!(result, anochi::vm::tree_walk::VmValue::from_i64(31));
        
        // Test boolean operations
        let result = runner.evaluate_expr("a > b").unwrap();
        assert_eq!(result, anochi::vm::tree_walk::VmValue::from_bool(true));

    }

    #[test]
    fn test_control_flow() {
        let mut runner = CodeRunner::default();
        
        // Test if statement
        runner.run_statement("x = 0;").unwrap();
        runner.run_statement("if (2 > 1) { x = 42; }").unwrap();
        let result = runner.evaluate_expr("x").unwrap();
        assert_eq!(result, anochi::vm::tree_walk::VmValue::from_i64(42));
    }
}
