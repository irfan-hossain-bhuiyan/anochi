use ast::{BinaryOperator, Expression, Statement};
use token::Identifier;
use vm::backend::IoBackend;
use vm::tree_walk::{Vm, VmValue};

use crate::{ast, token, vm};

#[test]
fn test_vm_basic_operations() {
    let mut vm = Vm::new(IoBackend::new());

    // Test assignment and variable lookup
    let assignment = Statement::assignment_no_type(Identifier::new("x"), Expression::from_i64(42).to_node(()));
    vm.execute_statement(&assignment.to_node(()))
        .unwrap();

    let var_expr = Expression::identifier(Identifier::new("x".to_string()));
    let result = vm.evaluate_expr(&var_expr.to_node(())).unwrap();
    assert_eq!(result, VmValue::from_i64(42));

    // Test boolean operations
    let undefined_expr = Expression::identifier(Identifier::new("undefined_variable".to_string()));
    let undefined_result = vm.evaluate_expr(&undefined_expr.to_node(()));
    assert!(undefined_result.is_err()); // Should error for undefined variable

    let and_expr = Expression::binary(
        Expression::from_bool(true).to_node(()),
        BinaryOperator::And,
        Expression::from_bool(false).to_node(()),
    );
    let result = vm.evaluate_expr(&and_expr.to_node(())).unwrap();
    assert_eq!(result, VmValue::from_bool(false));
}

#[test]
fn test_vm_error_handling() {
    let mut vm = Vm::new(IoBackend::new());

    // Test undefined variable error
    let undefined_expr = Expression::identifier(Identifier::new("undefined_variable".to_string()));
    let result = vm.evaluate_expr(&undefined_expr.to_node(()));
    assert!(result.is_err());
}

#[test]
fn test_debug_statement_single_value() {
    use crate::vm::backend::TestBackend;
    let backend = TestBackend::new();
    let mut vm: Vm<TestBackend> = Vm::new(backend);

    // Create a debug statement with a single integer expression
    let expr = Expression::from_i64(42).to_node(());
    let debug_stmt = Statement::debug(vec![expr]).to_node(());

    // Execute the debug statement
    vm.execute_statement(&debug_stmt).unwrap();

    // Verify the debug output
    let debug_output = vm.backend.get_debug_output();
    assert_eq!(debug_output, "42");
}
