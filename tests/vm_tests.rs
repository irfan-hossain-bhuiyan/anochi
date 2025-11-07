#[cfg(test)]
mod tests {
    use anochi::ast::{AstNode, Expression, Statement, BinaryOperator};
    use anochi::token::Identifier;
    use anochi::vm::tree_walk::{Vm, VmValue};
    use anochi::vm::backend::IoBackend;

    #[test]
    fn test_vm_basic_operations() {
        let mut vm = Vm::new(IoBackend::new());

        // Test assignment and variable lookup
        let assignment = Statement::assignment(
            Identifier::new("x"),
            Expression::from_i64(42)
        );
        vm.execute_statement(&AstNode::new_temp(assignment)).unwrap();
        
        let var_expr = Expression::identifier(Identifier::new("x".to_string()));
        let result = vm.evaluate_expr(&AstNode::new_temp(var_expr)).unwrap();
        assert_eq!(result, VmValue::from_i64(42));

        // Test boolean operations
        let undefined_expr = Expression::identifier(Identifier::new("undefined_variable".to_string()));
        let undefined_result = vm.evaluate_expr(&AstNode::new_temp(undefined_expr));
        assert!(undefined_result.is_err()); // Should error for undefined variable
        
        let and_expr = Expression::binary(
            Expression::from_bool(true),
            BinaryOperator::And,
            Expression::from_bool(false)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(and_expr)).unwrap();
        assert_eq!(result, VmValue::from_bool(false));


    }

    #[test]
    fn test_vm_error_handling() {
        let mut vm = Vm::new(IoBackend::new());

        // Test undefined variable error
        let undefined_expr = Expression::identifier(Identifier::new("undefined_variable".to_string()));
        let result = vm.evaluate_expr(&AstNode::new_temp(undefined_expr));
        assert!(result.is_err());
    }
}
