use crate::typing::{BuiltinKind, TypeContainer, TypeDefinition, UnifiedTypeDefinition};
use std::collections::{BTreeMap, BTreeSet};

#[test]
fn test_type_definition_independence() {
    // TypeDefinition should work independently
    let int_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    // Create a product type with independent TypeDefinitions
    let mut fields = BTreeMap::new();
    fields.insert("x".to_string(), int_type.clone());
    fields.insert("active".to_string(), bool_type.clone());
    
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
    
    // Convert through the proper flow: TypeDefinition -> UnifiedTypeDefinition -> OptimizedTypeDefinition
    let int_unified = int_type.to_unified();
    let bool_unified = bool_type.to_unified();
    
    let int_optimized = int_unified.to_optimized(&mut container);
    let bool_optimized = bool_unified.to_optimized(&mut container);
    
    let int_id = container.store_type(int_optimized.clone());
    let bool_id = container.store_type(bool_optimized.clone());
    
    // Verify we can get back the optimized definitions
    let retrieved_int = container.get_type(&int_id).expect("Type should exist").clone();
    let retrieved_bool = container.get_type(&bool_id).expect("Type should exist").clone();
    
    // Compare by converting both to unified format for comparison
    let retrieved_int_unified = retrieved_int.to_unified();
    let retrieved_bool_unified = retrieved_bool.to_unified();
    let int_optimized_unified = UnifiedTypeDefinition::from_optimized(int_optimized);
    let bool_optimized_unified = UnifiedTypeDefinition::from_optimized(bool_optimized);
    
    assert_eq!(retrieved_int_unified, int_optimized_unified);
    assert_eq!(retrieved_bool_unified, bool_optimized_unified);
}

#[test]
fn test_sum_type_creation() {
    let int_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    let mut variants = BTreeSet::new();
    variants.insert(int_type);
    variants.insert(bool_type);
    
    let sum_type = TypeDefinition::sum(variants);
    
    assert!(sum_type.as_sum().is_some());
    assert_eq!(sum_type.as_sum().unwrap().len(), 2);
}
