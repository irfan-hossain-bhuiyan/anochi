use crate::{typing::{TypeDefinition, TypeContainer, BuiltinKind, UnifiedTypeDefinition, TypeRef}, token::token_type::Identifier};
use std::collections::BTreeMap;

#[test]
fn test_type_container_complex_nested_deduplication() {
    let mut container = TypeContainer::new();
    
    // Create basic types
    let i64_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    // Create inner type: {a: i64, b: bool}
    let mut inner_fields = BTreeMap::new();
    inner_fields.insert(Identifier::new("a".to_string()), i64_type.clone());
    inner_fields.insert(Identifier::new("b".to_string()), bool_type.clone());
    let inner_type = TypeDefinition::product(inner_fields);
    
    // Create first outer type: {a: i64, b: {a: i64, b: bool}}
    let mut outer_fields_1 = BTreeMap::new();
    outer_fields_1.insert(Identifier::new("a".to_string()), i64_type.clone());
    outer_fields_1.insert(Identifier::new("b".to_string()), inner_type.clone());
    let outer_type_1 = TypeDefinition::product(outer_fields_1);
    
    // Create second outer type: {a: i64, b: {a: i64, b: bool}} (identical structure)
    let mut outer_fields_2 = BTreeMap::new();
    outer_fields_2.insert(Identifier::new("a".to_string()), i64_type.clone());
    outer_fields_2.insert(Identifier::new("b".to_string()), inner_type.clone());
    let outer_type_2 = TypeDefinition::product(outer_fields_2);
    
    // Convert through proper flow and store
    let unified_1 = outer_type_1.to_unified();
    let optimized_1 = unified_1.to_optimized(&mut container);
    let type_id_1 = container.store_type(optimized_1);
    
    let unified_2 = outer_type_2.to_unified();
    let optimized_2 = unified_2.to_optimized(&mut container);
    let type_id_2 = container.store_type(optimized_2);
    
    // TypeIds should be the same (deduplication working)
    assert_eq!(type_id_1, type_id_2, "Identical complex nested types should have same TypeId");
    
    // Verify we can retrieve the type
    let retrieved_type = container.get_type(&type_id_1).expect("Type should exist in container");
    
    // Check the structure is correct
    if let Some(fields) = retrieved_type.as_product() {
        assert_eq!(fields.len(), 2, "Outer type should have 2 fields");
        // Note: In the optimized form, fields contain TypeIds, not direct types
        // This is the expected behavior for the optimized storage
    } else {
        panic!("Retrieved type should be a product type");
    }
}

#[test]
fn test_type_container_different_complex_types() {
    let mut container = TypeContainer::new();
    
    // Create basic types
    let i64_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    let f64_type = TypeDefinition::builtin(BuiltinKind::F64);
    
    // Create type 1: {a: i64, b: {a: i64, b: bool}}
    let mut inner_fields_1 = BTreeMap::new();
    inner_fields_1.insert(Identifier::new("a".to_string()), i64_type.clone());
    inner_fields_1.insert(Identifier::new("b".to_string()), bool_type.clone());
    let inner_type_1 = TypeDefinition::product(inner_fields_1);
    
    let mut outer_fields_1 = BTreeMap::new();
    outer_fields_1.insert(Identifier::new("a".to_string()), i64_type.clone());
    outer_fields_1.insert(Identifier::new("b".to_string()), inner_type_1);
    let outer_type_1 = TypeDefinition::product(outer_fields_1);
    
    // Create type 2: {a: i64, b: {a: i64, b: f64}} (different inner type)
    let mut inner_fields_2 = BTreeMap::new();
    inner_fields_2.insert(Identifier::new("a".to_string()), i64_type.clone());
    inner_fields_2.insert(Identifier::new("b".to_string()), f64_type.clone()); // Different: f64 instead of bool
    let inner_type_2 = TypeDefinition::product(inner_fields_2);
    
    let mut outer_fields_2 = BTreeMap::new();
    outer_fields_2.insert(Identifier::new("a".to_string()), i64_type.clone());
    outer_fields_2.insert(Identifier::new("b".to_string()), inner_type_2);
    let outer_type_2 = TypeDefinition::product(outer_fields_2);
    
    // Convert and store both types
    let unified_1 = outer_type_1.to_unified();
    let optimized_1 = unified_1.to_optimized(&mut container);
    let type_id_1 = container.store_type(optimized_1);
    
    let unified_2 = outer_type_2.to_unified();
    let optimized_2 = unified_2.to_optimized(&mut container);
    let type_id_2 = container.store_type(optimized_2);
    
    // TypeIds should be different (different structure)
    assert_ne!(type_id_1, type_id_2, "Different complex types should have different TypeIds");
}
