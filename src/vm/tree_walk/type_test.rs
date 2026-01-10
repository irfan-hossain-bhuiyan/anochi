use super::*;
use crate::vm::backend::IoBackend;
use crate::parser::Parser;
use crate::token::Tokenizer;

fn parse_stmt(source: &str) -> crate::ast::StatNodeGeneric<crate::ast::CodeMetaData> {
    let (tokens, errors) = Tokenizer::new(source).tokenize();
    assert!(errors.is_empty(), "Tokenizer errors: {errors:?}");
    let mut parser = Parser::new(&tokens);
    parser.parse_statement().expect("Parse error")
}

use crate::ast::{Expression, Statement, Identifier};

fn get_type_id_from_metadata(meta: &crate::ast::CodeMetaData) -> Option<crate::types::TypeId> {
    meta.type_data
}



#[test]
fn test_simple_integer_literal() {
    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
    vm.load_builtin_types();

    let mut stmt = parse_stmt("let x = 10;");
    let result = vm.type_check_statement(&mut stmt);
    assert!(result.is_ok(), "Type checking should succeed for simple integer assignment");

    let expected_type = vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Int);
    // Check type in AST metadata (expression value of assignment)
    if let Statement::Assignment { value, .. } = &stmt.stat {
        let expr_type = get_type_id_from_metadata(value.data());
        assert_eq!(expr_type, Some(expected_type));
    } else {
        panic!("Expected assignment statement");
    }
}


#[test]
fn test_boolean_literal() {
    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
    vm.load_builtin_types();

    let mut stmt = parse_stmt("let flag = true;");
    let result = vm.type_check_statement(&mut stmt);
    assert!(result.is_ok(), "Type checking should succeed for boolean assignment");

    let var_data = vm.variables.get_variable_data(&Identifier::new("flag")).unwrap();
    let expected_type = vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Bool);
    assert_eq!(var_data.type_id, expected_type, "Variable should have Bool type");
    // Check type in AST metadata (expression value of assignment)
    if let Statement::Assignment { value, .. } = &stmt.stat {
        let expr_type = get_type_id_from_metadata(value.data());
        assert_eq!(expr_type, Some(expected_type));
    } else {
        panic!("Expected assignment statement");
    }
}


#[test]
fn test_explicit_type_annotation() {
    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
    vm.load_builtin_types();
    
    // Create AST: let x: int = 42;
    let value_expr = Expression::from_i64(42);
    let value_node = value_expr.to_node(crate::ast::CodeMetaData::default());
    
    let type_expr = Expression::identifier(Identifier::new("int"));
    let type_node = type_expr.to_node(crate::ast::CodeMetaData::default());
    
    let mut stmt = Statement::assignment(
        Identifier::new("x"),
        Some(type_node),
        value_node
    ).to_node(crate::ast::CodeMetaData::default());
    
    // Type check
    let result = vm.type_check_statement(&mut stmt);
    assert!(result.is_ok(), "Type checking should succeed with explicit type annotation");
    
    // Verify type
    let var_data = vm.variables.get_variable_data(&Identifier::new("x")).unwrap();
    let expected_type = vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Int);
    assert_eq!(var_data.type_id, expected_type, "Variable should have Int type");
}
//
//#[test]
//fn test_type_mismatch_error() {
//    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
//    vm.load_builtin_types();
//    
//    // Create AST: let x: bool = 42;  // Type mismatch!
//    let value_expr = Expression::from_i64(42);
//    let value_node = value_expr.to_node(crate::ast::CodeMetaData::default());
//    
//    let type_expr = Expression::identifier(Identifier::new("bool"));
//    let type_node = type_expr.to_node(crate::ast::CodeMetaData::default());
//    
//    let mut stmt = Statement::assignment(
//        Identifier::new("x"),
//        Some(type_node),
//        value_node
//    ).to_node(crate::ast::CodeMetaData::default());
//    
//    // Type check should fail
//    let result = vm.type_check_statement(&mut stmt);
//    assert!(result.is_err(), "Type checking should fail for type mismatch");
//}
//
//#[test]
//fn test_binary_expression() {
//    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
//    vm.load_builtin_types();
//    
//    // Create AST: let result = 10 + 20;
//    let left = Expression::from_i64(10).to_node(crate::ast::CodeMetaData::default());
//    let right = Expression::from_i64(20).to_node(crate::ast::CodeMetaData::default());
//    
//    let binary_expr = Expression::binary(left, crate::ast::BinaryOperator::Plus, right);
//    let binary_node = binary_expr.to_node(crate::ast::CodeMetaData::default());
//    
//    let mut stmt = Statement::assignment_no_type(
//        Identifier::new("result"),
//        binary_node
//    ).to_node(crate::ast::CodeMetaData::default());
//    
//    // Type check
//    let result = vm.type_check_statement(&mut stmt);
//    assert!(result.is_ok(), "Type checking should succeed for binary expression");
//    
//    // Verify type is Int
//    let var_data = vm.variables.get_variable_data(&Identifier::new("result")).unwrap();
//    let expected_type = vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Int);
//    assert_eq!(var_data.type_id, expected_type, "Result should have Int type");
//}
//
//#[test]
//fn test_comparison_expression() {
//    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
//    vm.load_builtin_types();
//    
//    // Create AST: let is_greater = 10 > 5;
//    let left = Expression::from_i64(10).to_node(crate::ast::CodeMetaData::default());
//    let right = Expression::from_i64(5).to_node(crate::ast::CodeMetaData::default());
//    
//    let comparison = Expression::binary(left, crate::ast::BinaryOperator::Greater, right);
//    let comparison_node = comparison.to_node(crate::ast::CodeMetaData::default());
//    
//    let mut stmt = Statement::assignment_no_type(
//        Identifier::new("is_greater"),
//        comparison_node
//    ).to_node(crate::ast::CodeMetaData::default());
//    
//    // Type check
//    let result = vm.type_check_statement(&mut stmt);
//    assert!(result.is_ok(), "Type checking should succeed for comparison");
//    
//    // Verify type is Bool
//    let var_data = vm.variables.get_variable_data(&Identifier::new("is_greater")).unwrap();
//    let expected_type = vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Bool);
//    assert_eq!(var_data.type_id, expected_type, "Comparison result should have Bool type");
//}
//
//#[test]
//fn test_product_type() {
//    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
//    vm.load_builtin_types();
//    
//    // Create AST: let point = {x = 10, y = 20};
//    let mut fields = std::collections::HashMap::new();
//    fields.insert(
//        Identifier::new("x"),
//        Expression::from_i64(10).to_node(crate::ast::CodeMetaData::default())
//    );
//    fields.insert(
//        Identifier::new("y"),
//        Expression::from_i64(20).to_node(crate::ast::CodeMetaData::default())
//    );
//    
//    let product_expr = Expression::product(fields);
//    let product_node = product_expr.to_node(crate::ast::CodeMetaData::default());
//    
//    let mut stmt = Statement::assignment_no_type(
//        Identifier::new("point"),
//        product_node
//    ).to_node(crate::ast::CodeMetaData::default());
//    
//    // Type check
//    let result = vm.type_check_statement(&mut stmt);
//    assert!(result.is_ok(), "Type checking should succeed for product type");
//    
//    // Verify variable exists
//    let var_data = vm.variables.get_variable_data(&Identifier::new("point"));
//    assert!(var_data.is_some(), "Variable point should be registered");
//}
//
//#[test]
//fn test_comptime_block() {
//    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
//    vm.load_builtin_types();
//    
//    // Create AST: comptime { let x = int; }
//    let type_expr = Expression::identifier(Identifier::new("int"));
//    let type_node = type_expr.to_node(crate::ast::CodeMetaData::default());
//    
//    let inner_stmt = Statement::assignment_no_type(
//        Identifier::new("my_type"),
//        type_node
//    ).to_node(crate::ast::CodeMetaData::default());
//    
//    let block = crate::ast::StatementBlockGeneric::new(
//        vec![inner_stmt],
//        crate::ast::CodeMetaData::default()
//    );
//    
//    let mut comptime_stmt = Statement::Comptime { statements: block }
//        .to_node(crate::ast::CodeMetaData::default());
//    
//    // Type check (should also execute the comptime block)
//    let result = vm.type_check_statement(&mut comptime_stmt);
//    assert!(result.is_ok(), "Type checking should succeed for comptime block");
//    
//    // After comptime execution, the variable should have an actual value (not just type info)
//    // Since comptime blocks execute during type checking
//    let var_data = vm.variables.get_variable_data(&Identifier::new("my_type"));
//    assert!(var_data.is_some(), "Comptime variable should be registered");
//}
//
//#[test]
//fn test_if_statement_type_check() {
//    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
//    vm.load_builtin_types();
//    
//    // Create AST: if true { let x = 10; }
//    let condition = Expression::from_bool(true).to_node(crate::ast::CodeMetaData::default());
//    
//    let body_stmt = Statement::assignment_no_type(
//        Identifier::new("x"),
//        Expression::from_i64(10).to_node(crate::ast::CodeMetaData::default())
//    ).to_node(crate::ast::CodeMetaData::default());
//    
//    let mut if_stmt = Statement::if_stmt(condition, body_stmt)
//        .to_node(crate::ast::CodeMetaData::default());
//    
//    // Type check
//    let result = vm.type_check_statement(&mut if_stmt);
//    assert!(result.is_ok(), "Type checking should succeed for if statement");
//}
//
//#[test]
//fn test_if_with_non_bool_condition_fails() {
//    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
//    vm.load_builtin_types();
//    
//    // Create AST: if 42 { let x = 10; }  // 42 is not a boolean!
//    let condition = Expression::from_i64(42).to_node(crate::ast::CodeMetaData::default());
//    
//    let body_stmt = Statement::assignment_no_type(
//        Identifier::new("x"),
//        Expression::from_i64(10).to_node(crate::ast::CodeMetaData::default())
//    ).to_node(crate::ast::CodeMetaData::default());
//    
//    let mut if_stmt = Statement::if_stmt(condition, body_stmt)
//        .to_node(crate::ast::CodeMetaData::default());
//    
//    // Type check should fail
//    let result = vm.type_check_statement(&mut if_stmt);
//    assert!(result.is_err(), "Type checking should fail for non-boolean if condition");
//}
//
//#[test]
//fn test_scoped_variables() {
//    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
//    vm.load_builtin_types();
//    
//    // Create AST: { let x = 10; }
//    let inner_stmt = Statement::assignment_no_type(
//        Identifier::new("x"),
//        Expression::from_i64(10).to_node(crate::ast::CodeMetaData::default())
//    ).to_node(crate::ast::CodeMetaData::default());
//    
//    let block = crate::ast::StatementBlockGeneric::new(
//        vec![inner_stmt],
//        crate::ast::CodeMetaData::default()
//    );
//    
//    let mut block_stmt = Statement::StatementBlock(block)
//        .to_node(crate::ast::CodeMetaData::default());
//    
//    // Type check
//    let result = vm.type_check_statement(&mut block_stmt);
//    assert!(result.is_ok(), "Type checking should succeed for scoped block");
//    
//    // Variable should not be accessible outside the block
//    let var_data = vm.variables.get_variable_data(&Identifier::new("x"));
//    assert!(var_data.is_none(), "Variable x should not be accessible outside its scope");
//}
//
//#[test]
//fn debug_builtin_types() {
//    let mut vm = Vm::<IoBackend>::new(IoBackend::default());
//    vm.load_builtin_types();
//    
//    // Check if builtin types are loaded
//    eprintln!("Checking builtin types...");
//    for name in &["int", "bool", "float"] {
//        let var = vm.variables.get_variable_data(&Identifier::new(name));
//        eprintln!("{}: exists={}", name, var.is_some());
//    }
//    
//    // Try to evaluate "int" identifier as a type annotation
//    let type_expr = Expression::identifier(Identifier::new("int"));
//    let mut type_node = type_expr.to_node(crate::ast::CodeMetaData::default());
//    
//    let result = vm.type_check_expr(&mut type_node);
//    eprintln!("Type check 'int' identifier: {:?}", result);
//    
//    // If it succeeded, what type did we get?
//    if let Ok(type_id) = result {
//        eprintln!("Got type_id: {:?}", type_id);
//        // This type_id should be the Type builtin
//        let type_type = vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Type);
//        eprintln!("Expected Type type_id: {:?}", type_type);
//        eprintln!("Match: {}", type_id == type_type);
//    }
//}
//
