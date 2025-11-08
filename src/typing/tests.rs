use crate::{
    token::token_type::Identifier,
    typing::{BuiltinKind, TypeContainer, TypeDefinition, UnifiedTypeDefinition},
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
    outer_fields_1_new.insert(Identifier::new("a"), UnifiedTypeDefinition::Builtin(BuiltinKind::I64));
    outer_fields_1_new.insert(Identifier::new("b"), UnifiedTypeDefinition::TypeId(inner_type_id));
    let outer_type_1_new = UnifiedTypeDefinition::Product { fields: outer_fields_1_new };
    let outer_type_1_id=container.store_unified_type(outer_type_1_new);
    assert_eq!(outer_type_1_id,type_id_1);
}

#[test]
fn test_vm_value_to_unified_type_definition() {
    use crate::vm::tree_walk::VmValue;
    use std::collections::HashMap;
    
    let container = TypeContainer::new();
    
    // Test get_type_of_value for primitive values
    let bool_value = VmValue::from_bool(true);
    let bool_type = bool_value.get_type_of_value().unwrap();
    assert_eq!(bool_type, UnifiedTypeDefinition::Builtin(BuiltinKind::Bool));
    
    let int_value = VmValue::from_i64(42);
    let int_type = int_value.get_type_of_value().unwrap();
    assert_eq!(int_type, UnifiedTypeDefinition::Builtin(BuiltinKind::I64));
    
    let float_value = VmValue::from_f64(3.14);
    let float_type = float_value.get_type_of_value().unwrap();
    assert_eq!(float_type, UnifiedTypeDefinition::Builtin(BuiltinKind::F64));
    
    // Test product value conversion
    let mut fields = HashMap::new();
    fields.insert(Identifier::new("age"), VmValue::from_i64(25));
    fields.insert(Identifier::new("active"), VmValue::from_bool(true));
    
    let product_value = VmValue::Product(fields);
    let product_type = product_value.get_type_of_value().unwrap();
    
    if let UnifiedTypeDefinition::Product { fields: type_fields } = product_type {
        assert_eq!(type_fields.len(), 2);
        assert_eq!(type_fields[&Identifier::new("age")], UnifiedTypeDefinition::Builtin(BuiltinKind::I64));
        assert_eq!(type_fields[&Identifier::new("active")], UnifiedTypeDefinition::Builtin(BuiltinKind::Bool));
    } else {
        panic!("Expected Product type");
    }
}


