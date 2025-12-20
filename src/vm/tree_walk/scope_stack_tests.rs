use crate::{
    ast::Identifier,
    types::TypeContainer,
    vm::tree_walk::{
        ScopeStack, VmErrorType,
        vm_value::{ParsedValueType, ValuePrimitive,VmValueSimplified},
    },
};
use num_bigint::BigInt;

/// Test basic scope operations: create variable, create scope, access from inner scope
#[test]
fn test_scope_variable_access_across_scopes() {
    let mut stack = ScopeStack::new();
    let mut type_container = TypeContainer::default();
    let var_name = Identifier::new("test_var".to_string());
    let value =
        ValuePrimitive::Integer(BigInt::from(42)).into_vm_value_generalized(&mut type_container);

    stack.insert_variable_default(var_name.clone(), value.clone());

    stack.create_scope();

    let retrieved = stack.get_value_from_name(&var_name, &type_container);
    assert!(retrieved.is_ok());
    assert_eq!(retrieved.unwrap(), value);

    stack.drop_scope();
    let retrieved = stack.get_value_from_name(&var_name, &type_container);
    assert!(retrieved.is_ok());
    assert_eq!(retrieved.unwrap(), value);
}

/// Test mutable access and modification of variables across scopes
#[test]
fn test_mutable_variable_access_and_modification() {
    let mut stack = ScopeStack::new();
    let mut type_container = TypeContainer::default();
    let var_name = Identifier::new("mutable_var".to_string());
    let initial_value =
        ValuePrimitive::Integer(BigInt::from(10)).into_vm_value_generalized(&mut type_container);
    let modified_value =
        ValuePrimitive::Integer(BigInt::from(20)).into_vm_value_generalized(&mut type_container);

    stack.insert_variable_default(var_name.clone(), initial_value.clone());

    stack.create_scope();

    stack
        .set_value_from_name(&var_name, modified_value.clone())
        .unwrap();

    let retrieved = stack
        .get_value_from_name(&var_name, &type_container)
        .unwrap();
    assert_eq!(retrieved, modified_value);

    stack.drop_scope();
    let retrieved = stack
        .get_value_from_name(&var_name, &type_container)
        .unwrap();
    assert_eq!(retrieved, modified_value);
}

/// Test variable shadowing: inner scope variable hides outer scope variable
#[test]
fn test_variable_shadowing() {
    let mut stack = ScopeStack::new();
    let mut type_container = TypeContainer::default();
    let var_name = Identifier::new("shadowed_var".to_string());
    let outer_value = ValuePrimitive::Bool(true).into_vm_value_generalized(&mut type_container);
    let inner_value = ValuePrimitive::Bool(false).into_vm_value_generalized(&mut type_container);

    stack.insert_variable_default(var_name.clone(), outer_value.clone());

    stack.create_scope();
    stack.insert_variable_default(var_name.clone(), inner_value.clone());

    let retrieved = stack
        .get_value_from_name(&var_name, &type_container)
        .unwrap();
    assert_eq!(retrieved, inner_value);

    stack.drop_scope();
    let retrieved = stack
        .get_value_from_name(&var_name, &type_container)
        .unwrap();
    assert_eq!(retrieved, outer_value);
}

/// Test error handling for undefined variables
#[test]
fn test_undefined_variable_access() {
    let stack = ScopeStack::new();
    let type_container = TypeContainer::default();
    let nonexistent_var = Identifier::new("nonexistent".to_string());

    let result = stack.get_value_from_name(&nonexistent_var, &type_container);
    assert!(matches!(result, Err(VmErrorType::UndefinedIdentifier(_))));
}

/// Test mutable access error handling for undefined variables
#[test]
fn test_undefined_variable_mutable_access() {
    let mut stack = ScopeStack::new();
    let mut type_container = TypeContainer::default();
    let nonexistent_var = Identifier::new("nonexistent".to_string());
    let dummy_value =
        ValuePrimitive::Integer(BigInt::from(0)).into_vm_value_generalized(&mut type_container);

    let result = stack.set_value_from_name(&nonexistent_var, dummy_value);
    assert!(matches!(result, Err(VmErrorType::UndefinedIdentifier(_))));
}

/// Test has_variable method across multiple scopes
#[test]
fn test_has_variable_across_scopes() {
    let mut stack = ScopeStack::new();
    let mut type_container = TypeContainer::default();
    let var1 = Identifier::new("var1".to_string());
    let var2 = Identifier::new("var2".to_string());
    let value =
        ValuePrimitive::Integer(BigInt::from(1)).into_vm_value_generalized(&mut type_container);

    assert!(!stack.has_variable(&var1));
    assert!(!stack.has_variable(&var2));

    stack.insert_variable_default(var1.clone(), value.clone());
    assert!(stack.has_variable(&var1));
    assert!(!stack.has_variable(&var2));

    stack.create_scope();
    stack.insert_variable_default(var2.clone(), value.clone());
    assert!(stack.has_variable(&var1));
    assert!(stack.has_variable(&var2));

    stack.drop_scope();
    assert!(stack.has_variable(&var1));
    assert!(!stack.has_variable(&var2));
}

/// Test nested scope operations with multiple variables
#[test]
fn test_nested_scopes_complex() {
    let mut stack = ScopeStack::new();
    let mut type_container = TypeContainer::default();
    let global_var = Identifier::new("global".to_string());
    let scope1_var = Identifier::new("scope1".to_string());
    let scope2_var = Identifier::new("scope2".to_string());

    let global_value =
        ValuePrimitive::Integer(BigInt::from(100)).into_vm_value_generalized(&mut type_container);
    let scope1_value =
        ValuePrimitive::Integer(BigInt::from(200)).into_vm_value_generalized(&mut type_container);
    let scope2_value =
        ValuePrimitive::Integer(BigInt::from(300)).into_vm_value_generalized(&mut type_container);

    stack.insert_variable_default(global_var.clone(), global_value.clone());

    stack.create_scope();
    stack.insert_variable_default(scope1_var.clone(), scope1_value.clone());

    stack.create_scope();
    stack.insert_variable_default(scope2_var.clone(), scope2_value.clone());

    assert!(stack.has_variable(&global_var));
    assert!(stack.has_variable(&scope1_var));
    assert!(stack.has_variable(&scope2_var));

    stack.drop_scope();
    assert!(stack.has_variable(&global_var));
    assert!(stack.has_variable(&scope1_var));
    assert!(!stack.has_variable(&scope2_var));

    stack.drop_scope();
    assert!(stack.has_variable(&global_var));
    assert!(!stack.has_variable(&scope1_var));
    assert!(!stack.has_variable(&scope2_var));
}
#[test]
fn scope_reference_test() {
    let mut stack = ScopeStack::new();
    let mut type_container = TypeContainer::default();
    let var1 = Identifier::new("var1".to_string());
    let var2 = Identifier::new("var2".to_string());
    
    stack.insert_variable_simple(var1.clone(), ValuePrimitive::from_i64(-15).into(), &mut type_container);
    
    let var1_ref = stack.get_reference_from_name(&var1).unwrap();
    stack.insert_variable_simple(var2.clone(), var1_ref.into(), &mut type_container);
    
    let var2_value = stack.get_value_from_name(&var2, &type_container).unwrap();
    let var2_simplified = var2_value.into_simplified_value(&type_container);
    
    if let VmValueSimplified::Reference(reference) = var2_simplified {
        let dereferenced_value = stack.get_value_from_reference(&reference, &type_container);
        let dereferenced_simplified = dereferenced_value.into_simplified_value(&type_container);
        
        if let VmValueSimplified::ValuePrimitive(ValuePrimitive::Integer(n)) = dereferenced_simplified {
            assert_eq!(n, BigInt::from(-15));
        } else {
            panic!("Expected Integer, got {:?}", dereferenced_simplified);
        }
    } else {
        panic!("Expected Reference, got {:?}", var2_simplified);
    }
}
