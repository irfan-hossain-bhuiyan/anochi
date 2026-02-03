use super::*;
use crate::parser::Parser;
use crate::token::Tokenizer;
use crate::vm::backend::IoBackend;

fn parse_stmt(source: &str) -> crate::ast::StatNodeGeneric<crate::ast::CodeMetaData> {
    let (tokens, errors) = Tokenizer::new(source).tokenize();
    assert!(errors.is_empty(), "Tokenizer errors: {errors:?}");
    let mut parser = Parser::new(&tokens);
    parser.parse_statements().expect("Parse error")
}

use crate::ast::{ Identifier, Statement};

fn get_type_id_from_metadata(meta: &crate::ast::CodeMetaData) -> Option<crate::types::TypeId> {
    meta.type_data
}

#[test]
fn test_simple_literal() {
    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
    vm.load_builtin_types();

    let mut stmt = parse_stmt("
        let flag = true;
        let x = 10;");
    let result = vm.type_check_statement(&mut stmt);
    assert!(
        result.is_ok(),
        "Type checking should succeed for simple integer assignment"
    );

    let expected_type = vm
        .types
        .get_builtin_type_id(crate::types::CompTimeBuiltinType::Int);
    
    // TODO: Need to change this code to StatementBlock.
    //if let Statement::Assignment { value, .. } = &stmt.stat {
    //    let expr_type = get_type_id_from_metadata(value.data());
    //    assert_eq!(expr_type, Some(expected_type));
    //} else {
    //    panic!("Expected assignment statement");
    //}
    
    // Testing boolean flag.
    let var_data = vm
        .variables
        .get_variable_data(&Identifier::new("flag"))
        .unwrap();
    let expected_type = vm
        .types
        .get_builtin_type_id(crate::types::CompTimeBuiltinType::Bool);
    assert_eq!(
        var_data.type_id, expected_type,
        "Variable should have Bool type"
    );

}

#[test]
fn test_explicit_type_annotation() {
    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
    vm.load_builtin_types();

    let mut stmt = parse_stmt(r"
        let x: int = 42;
    ");
    let result = vm.type_check_statement(&mut stmt);
    assert!(
        result.is_ok(),
        "Type checking should succeed with explicit type annotation"
    );

    let var_data = vm
        .variables
        .get_variable_data(&Identifier::new("x"))
        .unwrap();
    let expected_type = vm
        .types
        .get_builtin_type_id(crate::types::CompTimeBuiltinType::Int);
    assert_eq!(
        var_data.type_id, expected_type,
        "Variable should have Int type"
    );
}
#[test]
fn test_comptime_operation() {
    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
    vm.load_builtin_types();
    let mut stmt = parse_stmt(
        r"let y:int=10;
        let x:int=69;
    ",
    );
    let result = vm.type_check_statement(&mut stmt);
    assert_eq!(result, Ok(()));

    let var_data = vm
        .variables
        .get_variable_data(&Identifier::new("x"))
        .unwrap();
    let expected_type = vm
        .types
        .get_builtin_type_id(crate::types::CompTimeBuiltinType::Int);
    assert_eq!(
        var_data.type_id, expected_type,
        "Variable should have Int type"
    );
}
