#[cfg(test)]
mod tests {
    use anochi::ast::{AstNode, Expression, Statement, BinaryOperator, Literal};
    use anochi::vm::tree_walk::Vm;
    use anochi::vm::backend::IoBackend;

    #[test]
    fn test_variable_assignment_execution() {
        let mut vm = Vm::new(IoBackend::new());

        // x = 42
        let assignment = Statement::Assignment {
            identifier: "x".to_string(),
            value: AstNode::new_temp(Expression::integer(42)),
        };
        let assignment_node = AstNode::new_temp(assignment);
        
        vm.execute_statement(&assignment_node).unwrap();

        // Lookup x
        let x_expr = Expression::identifier("x".to_string());
        let x_node = AstNode::new_temp(x_expr);
        let result = vm.evaluate_expr(&x_node).unwrap();
        
        assert_eq!(result, Literal::Integer(42));
    }

    #[test]
    fn test_expression_with_variables() {
        let mut vm = Vm::new(IoBackend::new());
        
        // x = 42
        let x_assignment = Statement::Assignment {
            identifier: "x".to_string(),
            value: AstNode::new_temp(Expression::integer(42)),
        };
        vm.execute_statement(&AstNode::new_temp(x_assignment)).unwrap();

        // y = 8
        let y_assignment = Statement::Assignment {
            identifier: "y".to_string(),
            value: AstNode::new_temp(Expression::integer(8)),
        };
        vm.execute_statement(&AstNode::new_temp(y_assignment)).unwrap();

        // x + y
        let x_expr = Expression::identifier("x".to_string());
        let y_expr = Expression::identifier("y".to_string());
        let add_expr = Expression::binary(x_expr, BinaryOperator::Plus, y_expr);
        let add_node = AstNode::new_temp(add_expr);
        
        let result = vm.evaluate_expr(&add_node).unwrap();
        assert_eq!(result, Literal::Integer(50));
    }

    #[test]
    fn test_multiple_variable_assignments() {
        let mut vm = Vm::new(IoBackend::new());

        // Create multiple assignments
        let assignments = vec![
            ("a", 10),
            ("b", 20),
            ("c", 30),
        ];

        for (name, value) in assignments {
            let assignment = Statement::Assignment {
                identifier: name.to_string(),
                value: AstNode::new_temp(Expression::integer(value)),
            };
            vm.execute_statement(&AstNode::new_temp(assignment)).unwrap();
        }

        // Verify all variables are set correctly
        let a_result = vm.evaluate_expr(&AstNode::new_temp(Expression::identifier("a".to_string()))).unwrap();
        assert_eq!(a_result, Literal::Integer(10));

        let b_result = vm.evaluate_expr(&AstNode::new_temp(Expression::identifier("b".to_string()))).unwrap();
        assert_eq!(b_result, Literal::Integer(20));

        let c_result = vm.evaluate_expr(&AstNode::new_temp(Expression::identifier("c".to_string()))).unwrap();
        assert_eq!(c_result, Literal::Integer(30));
    }

    #[test]
    fn test_complex_arithmetic_expressions() {
        let mut vm = Vm::new(IoBackend::new());

        // Set up variables
        vm.execute_statement(&AstNode::new_temp(Statement::Assignment {
            identifier: "a".to_string(),
            value: AstNode::new_temp(Expression::integer(5)),
        })).unwrap();

        vm.execute_statement(&AstNode::new_temp(Statement::Assignment {
            identifier: "b".to_string(),
            value: AstNode::new_temp(Expression::integer(3)),
        })).unwrap();

        // Test (a + b) * 2
        let expr = Expression::binary(
            Expression::binary(
                Expression::identifier("a".to_string()),
                BinaryOperator::Plus,
                Expression::identifier("b".to_string())
            ),
            BinaryOperator::Multiply,
            Expression::integer(2)
        );

        let result = vm.evaluate_expr(&AstNode::new_temp(expr)).unwrap();
        assert_eq!(result, Literal::Integer(16)); // (5 + 3) * 2 = 16
    }

    #[test]
    fn test_boolean_expressions() {
        let vm = Vm::new(IoBackend::new());

        // Test true and false
        let and_expr = Expression::binary(
            Expression::bool(true),
            BinaryOperator::And,
            Expression::bool(false)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(and_expr)).unwrap();
        assert_eq!(result, Literal::Bool(false));

        // Test true or false
        let or_expr = Expression::binary(
            Expression::bool(true),
            BinaryOperator::Or,
            Expression::bool(false)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(or_expr)).unwrap();
        assert_eq!(result, Literal::Bool(true));
    }

    #[test]
    fn test_comparison_expressions() {
        let vm = Vm::new(IoBackend::new());

        // Test 10 > 5
        let gt_expr = Expression::binary(
            Expression::integer(10),
            BinaryOperator::Greater,
            Expression::integer(5)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(gt_expr)).unwrap();
        assert_eq!(result, Literal::Bool(true));

        // Test 3 == 3
        let eq_expr = Expression::binary(
            Expression::integer(3),
            BinaryOperator::Equal,
            Expression::integer(3)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(eq_expr)).unwrap();
        assert_eq!(result, Literal::Bool(true));

        // Test 7 <= 10
        let le_expr = Expression::binary(
            Expression::integer(7),
            BinaryOperator::LessEqual,
            Expression::integer(10)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(le_expr)).unwrap();
        assert_eq!(result, Literal::Bool(true));
    }

    #[test]
    fn test_float_expressions() {
        let vm = Vm::new(IoBackend::new());

        // Test 3.14 + 2.86
        let add_expr = Expression::binary(
            Expression::float(3.14),
            BinaryOperator::Plus,
            Expression::float(2.86)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(add_expr)).unwrap();
        assert_eq!(result, Literal::Float(6.0));

        // Test 10.0 / 2.0
        let div_expr = Expression::binary(
            Expression::float(10.0),
            BinaryOperator::Divide,
            Expression::float(2.0)
        );
        let result = vm.evaluate_expr(&AstNode::new_temp(div_expr)).unwrap();
        assert_eq!(result, Literal::Float(5.0));
    }

    #[test]
    fn test_undefined_variable_error() {
        let vm = Vm::new(IoBackend::new());

        let undefined_expr = Expression::identifier("undefined_variable".to_string());
        let result = vm.evaluate_expr(&AstNode::new_temp(undefined_expr));
        
        assert!(result.is_err());
    }

    #[test]
    fn test_string_literals() {
        let vm = Vm::new(IoBackend::new());

        let string_expr = Expression::string("Hello, World!".to_string());
        let result = vm.evaluate_expr(&AstNode::new_temp(string_expr)).unwrap();
        
        assert_eq!(result, Literal::String("Hello, World!".to_string()));
    }
}
