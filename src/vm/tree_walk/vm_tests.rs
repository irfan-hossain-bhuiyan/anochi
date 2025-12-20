use ast::{BinaryOperator, expression::ExpressionGeneric, StatementGeneric, CodeMetaData};
use token::Identifier;
use vm::backend::IoBackend;
use vm::tree_walk::{Vm };

use crate::prelude::Mappable;
use crate::vm::tree_walk::vm_value::{ ValuePrimitive, ParsedValueType};
use crate::{ast, token, vm};

#[test]
fn test_vm_basic_operations() {
    let mut vm = Vm::new(IoBackend::new());

    let assignment = StatementGeneric::assignment_no_type(Identifier::new("x"), ExpressionGeneric::from_i64(42).to_node(())).to_node(());
    let assignment = assignment.inner_map(&mut |_x|CodeMetaData::default());
    vm.execute_statement(&assignment)
        .unwrap();

    let var_expr = ExpressionGeneric::identifier(Identifier::new("x".to_string()));
    let result = vm.evaluate_expr(&var_expr.to_node(CodeMetaData::default())).unwrap();
    let expected = ValuePrimitive::from_i64(42).into_vm_value_generalized(&mut vm.types);
    assert_eq!(result, expected);

    let undefined_expr = ExpressionGeneric::identifier(Identifier::new("undefined_variable".to_string()));
    let undefined_result = vm.evaluate_expr(&undefined_expr.to_node(CodeMetaData::default()));
    assert!(undefined_result.is_err());

    let and_expr = ExpressionGeneric::binary(
        ExpressionGeneric::from_bool(true).to_node(CodeMetaData::default()),
        BinaryOperator::And,
        ExpressionGeneric::from_bool(false).to_node(CodeMetaData::default()),
    );
    let result = vm.evaluate_expr(&and_expr.to_node(CodeMetaData::default())).unwrap();
    let expected = ValuePrimitive::from_bool(false).into_vm_value_generalized(&mut vm.types);
    assert_eq!(result, expected);
}

#[test]
fn test_vm_error_handling() {
    let mut vm = Vm::new(IoBackend::new());

    let undefined_expr = ExpressionGeneric::identifier(Identifier::new("undefined_variable".to_string()));
    let result = vm.evaluate_expr(&undefined_expr.to_node(CodeMetaData::default()));
    assert!(result.is_err());
}

#[test]
fn test_debug_statement_single_value() {
    use crate::vm::backend::TestBackend;
    let backend = TestBackend::new();
    let mut vm: Vm<TestBackend> = Vm::new(backend);

    let expr = ExpressionGeneric::from_i64(42).to_node(());
    let debug_stmt = StatementGeneric::debug(vec![expr]).to_node(());
    let debug_stmt=debug_stmt.inner_map(&mut |_x|CodeMetaData::default());
    vm.execute_statement(&debug_stmt).unwrap();

    let debug_output = vm.backend.get_debug_output();
    assert_eq!(debug_output, "42");
}
