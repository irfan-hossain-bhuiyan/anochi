use crate::{typing::{BuiltinKind, TypeContainer, TypeDefinition, UnifiedTypeDefinition}, token::token_type::Identifier};
use std::collections::BTreeMap;

#[test]
fn test_flexible_type_system_with_mixed_references() {
    let mut container = TypeContainer::new();
    
    // Create basic types
    let i64_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    // Create type 'a' = {x: i64, y: i64}
    let mut a_fields = BTreeMap::new();
    a_fields.insert(Identifier::new("x".to_string()), i64_type.clone());
    a_fields.insert(Identifier::new("y".to_string()), i64_type.clone());
    let type_a = TypeDefinition::product(a_fields);
    
    // Store 'a' and get its TypeId
    let unified_a = type_a.to_unified();
    let optimized_a = unified_a.to_optimized(&mut container);
    let id_a = container.store_type(optimized_a);
    
    // Create type 'b' = {x: {r: a, x: a}, y: bool}
    // This demonstrates mixing direct types and references!
    
    // Inner type: {r: a, x: a} (using references to 'a')
    let mut inner_fields = BTreeMap::new();
    inner_fields.insert(Identifier::new("r".to_string()), UnifiedTypeDefinition::TypeId(id_a));  // Reference to 'a'
    inner_fields.insert(Identifier::new("x".to_string()), UnifiedTypeDefinition::TypeId(id_a));  // Reference to 'a'
    let inner_unified = UnifiedTypeDefinition::product(inner_fields);
    
    // Outer type: {x: inner_type, y: bool} (mixing direct and references)
    let mut outer_fields = BTreeMap::new();
    outer_fields.insert(Identifier::new("x".to_string()), inner_unified);  // Direct nested type
    outer_fields.insert(Identifier::new("y".to_string()),
        UnifiedTypeDefinition::Builtin(BuiltinKind::Bool)  // Direct builtin
    );
    let type_b = UnifiedTypeDefinition::product(outer_fields);
    
    // Store 'b' and get its TypeId
    let optimized_b = type_b.to_optimized(&mut container);
    let id_b = container.store_type(optimized_b);
    
    // Verify we can retrieve both types
    assert!(container.get_type(&id_a).is_some());
    assert!(container.get_type(&id_b).is_some());
    
    // Verify the structure of type 'b'
    let retrieved_b = container.get_type(&id_b).unwrap();
    if let Some(fields) = retrieved_b.as_product() {
        assert_eq!(fields.len(), 2, "Type 'b' should have 2 fields");
        assert!(fields.contains_key(&Identifier::new("x".to_string())), "Should have field 'x'");
        assert!(fields.contains_key(&Identifier::new("y".to_string())), "Should have field 'y'");
    } else {
        panic!("Type 'b' should be a product type");
    }
    
    // Test conversion back to TypeDefinition (expanding all references)
    // retrieved_b is already a TypeDefinition (expanded), so we can use it directly
    let expanded_b = retrieved_b;
    if let Some(expanded_fields) = expanded_b.as_product() {
        assert_eq!(expanded_fields.len(), 2);
        
        // Check that 'x' field is a product type (the expanded inner type)
        if let Some(x_field) = expanded_fields.get(&Identifier::new("x".to_string())) {
            if let Some(x_inner_fields) = x_field.as_product() {
                assert_eq!(x_inner_fields.len(), 2, "Inner type should have 2 fields");
                assert!(x_inner_fields.contains_key(&Identifier::new("r".to_string())), "Should have field 'r'");
                assert!(x_inner_fields.contains_key(&Identifier::new("x".to_string())), "Should have field 'x'");
                
                // Both 'r' and 'x' should be i64 product types (expanded from reference to 'a')
                if let Some(r_field) = x_inner_fields.get(&Identifier::new("r".to_string())) {
                    assert!(r_field.as_product().is_some(), "Field 'r' should be expanded to product type");
                }
            } else {
                panic!("Field 'x' should be a product type");
            }
        } else {
            panic!("Should have field 'x'");
        }
        
        // Check that 'y' field is a boolean
        if let Some(y_field) = expanded_fields.get(&Identifier::new("y".to_string())) {
            assert_eq!(y_field.as_builtin(), Some(BuiltinKind::Bool));
        } else {
            panic!("Should have field 'y'");
        }
    } else {
        panic!("Expanded type 'b' should be a product type");
    }
}

#[test]
fn test_type_deduplication_with_mixed_structure() {
    let mut container = TypeContainer::new();
    
    // Create type with direct definition: {vx: {x: i64, y: i64}, vy: {x: i64, y: i64}}
    let i64_type = TypeDefinition::builtin(BuiltinKind::I64);
    let mut inner_fields_1 = BTreeMap::new();
    inner_fields_1.insert(Identifier::new("x".to_string()), i64_type.clone());
    inner_fields_1.insert(Identifier::new("y".to_string()), i64_type.clone());
    let inner_type_1 = TypeDefinition::product(inner_fields_1);
    
    let mut outer_fields_1 = BTreeMap::new();
    outer_fields_1.insert(Identifier::new("vx".to_string()), inner_type_1.clone());
    outer_fields_1.insert(Identifier::new("vy".to_string()), inner_type_1.clone());
    let type_direct = TypeDefinition::product(outer_fields_1);
    
    // Create equivalent type using references
    // First store the inner type: a = {x: i64, y: i64}
    let inner_unified = inner_type_1.to_unified();
    let inner_optimized = inner_unified.to_optimized(&mut container);
    let inner_id = container.store_type(inner_optimized);
    
    // Then create: {vx: a, vy: a} using references
    let mut outer_fields_2 = BTreeMap::new();
    outer_fields_2.insert(Identifier::new("vx".to_string()), UnifiedTypeDefinition::TypeId(inner_id));
    outer_fields_2.insert(Identifier::new("vy".to_string()), UnifiedTypeDefinition::TypeId(inner_id));
    let type_ref = UnifiedTypeDefinition::product(outer_fields_2);
    
    // Both should result in the same optimized TypeId
    let direct_optimized = type_direct.to_optimized(&mut container);
    let ref_optimized = type_ref.to_optimized(&mut container);
    
    let direct_id = container.store_type(direct_optimized);
    let ref_id = container.store_type(ref_optimized);
    
    // They should have the same TypeId due to deduplication!
    assert_eq!(direct_id, ref_id, "Direct and reference-based equivalent types should have same TypeId");
}
