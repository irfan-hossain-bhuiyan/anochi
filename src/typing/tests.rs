use crate::{
    token::token_type::Identifier,
    typing::{BuiltinKind, TypeContainer, TypeDefinition, TypeRef, UnifiedTypeDefinition},
};
use std::collections::BTreeMap;

#[test]
fn test_type_container_complex_nested_deduplication() {
    let mut container = TypeContainer::new();

    // Create basic types
    let i64_type = TypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = TypeDefinition::builtin(BuiltinKind::Bool);
    
    // Create inner type: {a: i64, b: bool}
    let mut inner_fields = BTreeMap::new();
    inner_fields.insert(Identifier::new("a"), i64_type.clone());
    inner_fields.insert(Identifier::new("b"), bool_type.clone());
    let inner_type = TypeDefinition::product(inner_fields);

    // Create first outer type: {a: i64, b: {a: i64, b: bool}}
    let mut outer_fields_1 = BTreeMap::new();
    outer_fields_1.insert(Identifier::new("a"), i64_type.clone());
    outer_fields_1.insert(Identifier::new("b"), inner_type.clone());
    let outer_type_1 = TypeDefinition::product(outer_fields_1);
    
    // Create second outer type: {a: i64, b: {a: i64, b: bool}} (identical structure)
    let mut outer_fields_2 = BTreeMap::new();
    outer_fields_2.insert(Identifier::new("a"), i64_type.clone());
    outer_fields_2.insert(Identifier::new("b"), inner_type.clone());
    let outer_type_2 = TypeDefinition::product(outer_fields_2);
    
    let mut outer_fields_3 = BTreeMap::new();
    outer_fields_3.insert(Identifier::new("a"), i64_type.clone());
    outer_fields_3.insert(Identifier::new("b"), bool_type.clone());
    let outer_type_3 = TypeDefinition::product(outer_fields_3);

    let type_id_1 = container.store_type_definition(outer_type_1.clone());
    let type_id_2 = container.store_type_definition(outer_type_2.clone());
    let type_id_3 = container.store_type_definition(outer_type_3.clone());

    // TypeIds should be the same (deduplication working)
    assert_eq!(
        type_id_1, type_id_2,
        "Identical complex nested types should have same TypeId"
    );
    assert_ne!(type_id_1,type_id_3,
        "This are two different type id."
    );

    // Verify we can retrieve the type
    let retrieved_type_1 = container
        .get_type(&type_id_1)
        .expect("Type should exist in container");
    let retrieved_type_3 = container
        .get_type(&type_id_3)
        .expect("Type should exist in container");

    // Check the structure is correct
    assert_eq!(retrieved_type_1,outer_type_2);
    assert_eq!(retrieved_type_3,outer_type_3);
    let inner_type_id=container.store_type_definition(inner_type);
    let mut outer_fields_1_new = BTreeMap::new();
    outer_fields_1_new.insert(Identifier::new("a"), TypeRef::Direct(UnifiedTypeDefinition::Builtin(BuiltinKind::I64)));
    outer_fields_1_new.insert(Identifier::new("b"), TypeRef::Reference(inner_type_id));
    let outer_type_1_new = UnifiedTypeDefinition::Product { fields: outer_fields_1_new };
    let outer_type_1_id=container.store_unified_type(outer_type_1_new);
    assert_eq!(outer_type_1_id,type_id_1);
}


