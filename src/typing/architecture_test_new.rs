use crate::typing::{TypeDefinition, TypeContainer, BuiltinKind};

#[test]
fn test_new_type_system_architecture() {
    let mut container = TypeContainer::new();
    
    // Create some types
    let int_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    // Follow the conversion flow: TypeDefinition -> UnifiedTypeDefinition -> OptimizedTypeDefinition -> TypeId
    let int_unified = int_type.to_unified();
    let bool_unified = bool_type.to_unified();
    
    let int_optimized = int_unified.to_optimized(&mut container);
    let bool_optimized = bool_unified.to_optimized(&mut container);
    
    // Store optimized types and get TypeIds
    let int_id = container.store_type(int_optimized.clone());
    let bool_id = container.store_type(bool_optimized.clone());
    
    // TypeIds should be different (unique hashes)
    assert_ne!(int_id, bool_id);
    
    // Should be able to retrieve original optimized types
    let retrieved_int = container.get_type(&int_id).unwrap();
    let retrieved_bool = container.get_type(&bool_id).unwrap();
    
    assert_eq!(retrieved_int, &int_optimized);
    assert_eq!(retrieved_bool, &bool_optimized);
    
    // Same type should get same TypeId (deduplication)
    let another_int_type = TypeDefinition::builtin(BuiltinKind::I64);
    let another_int_unified = another_int_type.to_unified();
    let another_int_optimized = another_int_unified.to_optimized(&mut container);
    let another_int_id = container.store_type(another_int_optimized);
    assert_eq!(int_id, another_int_id);
}
