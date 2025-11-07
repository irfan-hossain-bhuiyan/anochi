use ast::{AstNode, BinaryOperator, Expression, Statement};
use token::Identifier;
use vm::backend::IoBackend;
use vm::tree_walk::{Vm, VmValue};

use crate::{ast, token, vm};

#[test]
fn test_vm_basic_operations() {
    let mut vm = Vm::new(IoBackend::new());

    // Test assignment and variable lookup
    let assignment = Statement::assignment(Identifier::new("x"), Expression::from_i64(42));
    vm.execute_statement(&AstNode::new_temp(assignment))
        .unwrap();

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
        Expression::from_bool(false),
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

#[test]
fn test_debug_statement_single_value() {
    use crate::vm::backend::TestBackend;
    let backend = TestBackend::new();
    let mut vm: Vm<TestBackend> = Vm::new(backend);

    // Create a debug statement with a single integer expression
    let expr = AstNode::new_temp(Expression::from_i64(42));
    let debug_stmt = AstNode::new_temp(Statement::debug(vec![expr]));

    // Execute the debug statement
    vm.execute_statement(&debug_stmt).unwrap();

    // Verify the debug output
    let debug_output = vm.backend.get_debug_output();
    assert_eq!(debug_output, "42");
}
