use crate::{
    token::token_type::Identifier,
    types::{BuiltinKind, TypeContainer, TypeDefinition, UnifiedTypeDefinition},
    vm::tree_walk::VmValue::{self},
    vm::tree_walk::StructValue
};
use std::collections::{BTreeMap, HashMap};

#[test]
fn test_type_container_complex_nested_deduplication() {
    let mut container = TypeContainer::new();

    // Create basic types
    let i64_type = UnifiedTypeDefinition::builtin(BuiltinKind::I64);
    let bool_type = UnifiedTypeDefinition::builtin(BuiltinKind::Bool);

    // Create inner type: {a: i64, b: bool}
    let mut inner_fields = BTreeMap::new();
    inner_fields.insert(Identifier::new("a"), i64_type.clone());
    inner_fields.insert(Identifier::new("b"), bool_type.clone());
    let inner_type = UnifiedTypeDefinition::product(inner_fields);

    // Create first outer type: {a: i64, b: {a: i64, b: bool}}
    let mut outer_fields_1 = BTreeMap::new();
    outer_fields_1.insert(Identifier::new("a"), i64_type.clone());
    outer_fields_1.insert(Identifier::new("b"), inner_type.clone());
    let outer_type_1 = UnifiedTypeDefinition::product(outer_fields_1);

    // Create second outer type: {a: i64, b: {a: i64, b: bool}} (identical structure)
    let mut outer_fields_2 = BTreeMap::new();
    outer_fields_2.insert(Identifier::new("a"), i64_type.clone());
    outer_fields_2.insert(Identifier::new("b"), inner_type.clone());
    let outer_type_2 = UnifiedTypeDefinition::product(outer_fields_2);

    let mut outer_fields_3 = BTreeMap::new();
    outer_fields_3.insert(Identifier::new("a"), i64_type.clone());
    outer_fields_3.insert(Identifier::new("b"), bool_type.clone());
    let outer_type_3 = UnifiedTypeDefinition::product(outer_fields_3);

    let type_id_1 = container.store_unified_type(outer_type_1.clone());
    let type_id_2 = container.store_unified_type(outer_type_2.clone());
    let type_id_3 = container.store_unified_type(outer_type_3.clone());

    // TypeIds should be the same (deduplication working)
    assert_eq!(
        type_id_1, type_id_2,
        "Identical complex nested types should have same TypeId"
    );
    assert_ne!(type_id_1, type_id_3, "This are two different type id.");
}

#[test]
fn test_vm_value_to_unified_type_definition() {
    use crate::vm::tree_walk::VmValue;

    // Test get_type_of_value for primitive values
    let bool_value = VmValue::from_bool(true);
    let bool_type = bool_value.get_type_of_value();
    assert_eq!(bool_type, UnifiedTypeDefinition::builtin(BuiltinKind::Bool));

    let int_value = VmValue::from_i64(42);
    let int_type = int_value.get_type_of_value();
    assert_eq!(int_type, UnifiedTypeDefinition::builtin(BuiltinKind::I64));

    let float_value = VmValue::from_f64(3.145);
    let float_type = float_value.get_type_of_value();
    assert_eq!(float_type, UnifiedTypeDefinition::builtin(BuiltinKind::F64));
}
#[test]
fn test_vm_value_for_complex_type() {
    let mut map = HashMap::new();
    map.insert(Identifier::new("x"), VmValue::from_i64(50));
    map.insert(Identifier::new("y"), VmValue::from_i64(50));
    let value = VmValue::Product(StructValue::new(map));
    let mut container = TypeContainer::new();
    let value_type = value.get_type_of_value();
    let id1 = container.store_unified_type(value_type);

    let type_def = container
        .get_type(&id1)
        .unwrap()
        .clone()
        .into_type_def(&container);
    let mut map = BTreeMap::new();
    map.insert(
        Identifier::new("x"),
        TypeDefinition::builtin(BuiltinKind::I64),
    );
    map.insert(
        Identifier::new("y"),
        TypeDefinition::builtin(BuiltinKind::I64),
    );
    let type1 = TypeDefinition::product(map);
    assert_eq!(type_def, type1)
    //let id2=container.store_type_def(type1);
    //assert_eq!(id1,id2);
}
