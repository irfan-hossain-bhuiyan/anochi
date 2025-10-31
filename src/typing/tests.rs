use crate::typing::{TypeDefinition, BuiltinKind, TypeContainer};
use std::collections::{BTreeMap, BTreeSet};

#[test]
fn test_type_definition_independence() {
    // TypeDefinition should work independently
    let int_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    // Create a product type with independent TypeDefinitions
    let mut fields = BTreeMap::new();
    fields.insert("x".to_string(), Box::new(int_type.clone()));
    fields.insert("active".to_string(), Box::new(bool_type.clone()));
    
    let product_type = TypeDefinition::product(fields);
    
    // Test compatibility
    assert!(int_type.is_compatible_with(&int_type));
    assert!(!int_type.is_compatible_with(&bool_type));
    
    // Test accessors
    assert_eq!(int_type.as_builtin(), Some(BuiltinKind::I64));
    assert!(product_type.as_product().is_some());
    assert!(product_type.as_sum().is_none());
}

#[test]
fn test_type_container_optimization() {
    let mut container = TypeContainer::new();
    
    // Create TypeDefinitions
    let int_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    // Convert to optimized and store in container
    let int_id = container.store_type(int_type.clone());
    let bool_id = container.store_type(bool_type.clone());
    
    // Verify we can get back the full definitions
    let retrieved_int = container.get_type(&int_id).expect("Type should exist").clone();
    let retrieved_bool = container.get_type(&bool_id).expect("Type should exist").clone();
    
    assert_eq!(retrieved_int, int_type);
    assert_eq!(retrieved_bool, bool_type);
}

#[test]
fn test_sum_type_creation() {
    let int_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    let mut variants = BTreeSet::new();
    variants.insert(Box::new(int_type));
    variants.insert(Box::new(bool_type));
    
    let sum_type = TypeDefinition::sum(variants);
    
    assert!(sum_type.as_sum().is_some());
    assert_eq!(sum_type.as_sum().unwrap().len(), 2);
}
