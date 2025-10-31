use crate::typing::{TypeDefinition, TypeContainer, BuiltinKind};

#[test]
fn test_new_type_system_architecture() {
    let mut container = TypeContainer::new();
    
    // Create some types
    let int_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    // Store types and get TypeIds
    let int_id = container.store_type(int_type.clone());
    let bool_id = container.store_type(bool_type.clone());
    
    // TypeIds should be different (unique hashes)
    assert_ne!(int_id, bool_id);
    
    // Should be able to retrieve original types
    let retrieved_int = container.get_type(&int_id).unwrap();
    let retrieved_bool = container.get_type(&bool_id).unwrap();
    
    assert_eq!(retrieved_int, &int_type);
    assert_eq!(retrieved_bool, &bool_type);
    
    // Same type should get same TypeId (deduplication)
    let another_int_type = TypeDefinition::builtin(BuiltinKind::I64);
    let another_int_id = container.store_type(another_int_type);
    assert_eq!(int_id, another_int_id);
}