use crate::typing::{TypeDefinition, TypeContainer, BuiltinKind};
use std::collections::BTreeMap;

#[test]
fn test_type_container_complex_nested_deduplication() {
    let mut container = TypeContainer::new();
    
    // Create basic types
    let i64_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    // Create inner type: {a: i64, b: bool}
    let mut inner_fields = BTreeMap::new();
    inner_fields.insert("a".to_string(), Box::new(i64_type.clone()));
    inner_fields.insert("b".to_string(), Box::new(bool_type.clone()));
    let inner_type = TypeDefinition::product(inner_fields);
    
    // Create first outer type: {a: i64, b: {a: i64, b: bool}}
    let mut outer_fields_1 = BTreeMap::new();
    outer_fields_1.insert("a".to_string(), Box::new(i64_type.clone()));
    outer_fields_1.insert("b".to_string(), Box::new(inner_type.clone()));
    let outer_type_1 = TypeDefinition::product(outer_fields_1);
    
    // Create second outer type: {a: i64, b: {a: i64, b: bool}} (identical structure)
    let mut outer_fields_2 = BTreeMap::new();
    outer_fields_2.insert("a".to_string(), Box::new(i64_type.clone()));
    outer_fields_2.insert("b".to_string(), Box::new(inner_type.clone()));
    let outer_type_2 = TypeDefinition::product(outer_fields_2);
    
    // Store first type and get TypeId
    let type_id_1 = container.store_type(outer_type_1);
    
    // Store second identical type and get TypeId
    let type_id_2 = container.store_type(outer_type_2);
    
    // TypeIds should be the same (deduplication working)
    assert_eq!(type_id_1, type_id_2, "Identical complex nested types should have same TypeId");
    
    // Verify we can retrieve the type
    let retrieved_type = container.get_type(&type_id_1).expect("Type should exist in container");
    
    // Check the structure is correct
    if let Some(fields) = retrieved_type.as_product() {
        assert_eq!(fields.len(), 2, "Outer type should have 2 fields");
        
        // Check field 'a' is i64
        let field_a = fields.get("a").expect("Field 'a' should exist");
        assert_eq!(field_a.as_builtin(), Some(BuiltinKind::I64));
        
        // Check field 'b' is a product type
        let field_b = fields.get("b").expect("Field 'b' should exist");
        if let Some(inner_fields) = field_b.as_product() {
            assert_eq!(inner_fields.len(), 2, "Inner type should have 2 fields");
            
            // Check inner field 'a' is i64
            let inner_a = inner_fields.get("a").expect("Inner field 'a' should exist");
            assert_eq!(inner_a.as_builtin(), Some(BuiltinKind::I64));
            
            // Check inner field 'b' is bool
            let inner_b = inner_fields.get("b").expect("Inner field 'b' should exist");
            assert_eq!(inner_b.as_builtin(), Some(BuiltinKind::Bool));
        } else {
            panic!("Field 'b' should be a product type");
        }
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
    inner_fields_1.insert("a".to_string(), Box::new(i64_type.clone()));
    inner_fields_1.insert("b".to_string(), Box::new(bool_type.clone()));
    let inner_type_1 = TypeDefinition::product(inner_fields_1);
    
    let mut outer_fields_1 = BTreeMap::new();
    outer_fields_1.insert("a".to_string(), Box::new(i64_type.clone()));
    outer_fields_1.insert("b".to_string(), Box::new(inner_type_1));
    let outer_type_1 = TypeDefinition::product(outer_fields_1);
    
    // Create type 2: {a: i64, b: {a: i64, b: f64}} (different inner type)
    let mut inner_fields_2 = BTreeMap::new();
    inner_fields_2.insert("a".to_string(), Box::new(i64_type.clone()));
    inner_fields_2.insert("b".to_string(), Box::new(f64_type.clone())); // Different: f64 instead of bool
    let inner_type_2 = TypeDefinition::product(inner_fields_2);
    
    let mut outer_fields_2 = BTreeMap::new();
    outer_fields_2.insert("a".to_string(), Box::new(i64_type.clone()));
    outer_fields_2.insert("b".to_string(), Box::new(inner_type_2));
    let outer_type_2 = TypeDefinition::product(outer_fields_2);
    
    // Store both types
    let type_id_1 = container.store_type(outer_type_1);
    let type_id_2 = container.store_type(outer_type_2);
    
    // TypeIds should be different (different structure)
    assert_ne!(type_id_1, type_id_2, "Different complex types should have different TypeIds");
}