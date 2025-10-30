#[cfg(test)]
mod tests {
    use anochi::ast::{AstNode, Expression, Statement, BinaryOperator, Literal};
    use anochi::vm::tree_walk::{Vm, VmValue};
    use anochi::vm::backend::IoBackend;

    #[test]
    fn test_vm_core_operations() {
        let mut vm = Vm::new(IoBackend::new());

        // Create multiple assignments
        let assignments = vec![
            ("a", 10),
            ("b", 20),
            ("c", 30),
        ];

        for (name, value) in assignments {
            let assignment = Statement::Assignment {
                target: AstNode::new_temp(Expression::identifier(name.to_string())),
                value: AstNode::new_temp(Expression::integer(value)),
            };
            vm.execute_statement(&AstNode::new_temp(assignment)).unwrap();
        }

        // Test complex arithmetic expression: (a + b) * c / 10
        let expr = Expression::binary(
            Expression::binary(
                Expression::binary(
                    Expression::identifier("a".to_string()),
                    BinaryOperator::Plus,
                    Expression::identifier("b".to_string())
                ),
                BinaryOperator::Multiply,
                Expression::identifier("c".to_string())
            ),
            BinaryOperator::Divide,
            Expression::integer(10)
        );

        let result = vm.evaluate_expr(&AstNode::new_temp(expr)).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Integer(90))); // (10 + 20) * 30 / 10 = 90
    }

    #[test]
    fn test_vm_data_types_and_operations() {
        let vm = Vm::new(IoBackend::new());

        // Test boolean operations
        let and_expr = Expression::binary(
            Expression::bool(true),
            BinaryOperator::And,
            Expression::bool(false)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(and_expr)).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Bool(false)));

        // Test float operations
        let div_expr = Expression::binary(
            Expression::float(10.0),
            BinaryOperator::Divide,
            Expression::float(2.0)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(div_expr)).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Float(5.0)));

        // Test comparison operations
        let gt_expr = Expression::binary(
            Expression::integer(10),
            BinaryOperator::Greater,
            Expression::integer(5)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(gt_expr)).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::Bool(true)));

        // Test string literals
        let string_expr = Expression::string("Hello, World!".to_string());
        let result = vm.evaluate_expr(&AstNode::new_temp(string_expr)).unwrap();
        assert_eq!(result, VmValue::Literal(Literal::String("Hello, World!".to_string())));
    }

    #[test]
    fn test_vm_error_handling() {
        let vm = Vm::new(IoBackend::new());

        // Test undefined variable error
        let undefined_expr = Expression::identifier("undefined_variable".to_string());
        let result = vm.evaluate_expr(&AstNode::new_temp(undefined_expr));
        assert!(result.is_err());
    }
}
