//! Type system module for the Anochi programming language.
//!
//! This module provides the core type infrastructure including:
//! - Type identification (TypeId)
//! - Type definitions (product types and sum types)
//! - Type container for managing types and name resolution
//! - Variable identifier aliasing

use std::collections::HashMap;
use crate::ast::Identifier;
use std::collections::HashSet;
use hash_cons::HcTable;
use hash_cons::Hc;
/// Built-in type kinds.
/// 
/// Represents primitive types that are built into the type system.
/// These types are pre-registered in the TypeContainer and cannot be user-defined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinKind {
    /// 64-bit signed integer
    I64,
    /// 64-bit floating point
    F64,
    /// Boolean type
    Bool,
    /// Machine-sized unsigned integer (pointer size)
    Usize,
}
type TypeId = Hc<TypeDefinition>;
/// Type definition representing the structure of a type.
/// 
/// Supports both product types (structs with named fields) and sum types
/// (enums with variants). In the future, function types may be added.
/// 
/// Product types use HashMap<Identifier, TypeId> to map field names to their types,
/// preventing field name duplication.
/// 
/// Sum types use HashSet<TypeId> to store variant types,
/// preventing type duplication across variants.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeDefinition {
    /// Product type: a collection of named fields (struct-like).
    /// Fields are stored as a HashMap mapping field names to their types.
    Product {
        /// Fields in the product type, mapped by identifier
        fields: HashMap<Identifier, TypeId>,
    },
    /// Sum type: a collection of variants (enum-like).
    /// Variants are stored as a set of types to prevent duplication.
    Sum {
        /// Types associated with variants in the sum type
        variants: HashSet<TypeId>,
    },
    /// Built-in type: a primitive type provided by the language.
    Builtin(BuiltinKind),
}

impl TypeDefinition {
    /// Creates a new product type (struct).
    pub fn product(fields: HashMap<Identifier, TypeId>) -> Self {
        TypeDefinition::Product { fields }
    }

    /// Creates a new sum type (enum).
    pub fn sum(variants: HashSet<TypeId>) -> Self {
        TypeDefinition::Sum { variants }
    }

    /// Returns the fields of a product type, or None if this is a sum type.
    pub fn as_product(&self) -> Option<&HashMap<Identifier, TypeId>> {
        match self {
            TypeDefinition::Product { fields, .. } => Some(fields),
            _ => None,
        }
    }

    /// Returns the variants of a sum type, or None if this is a product type.
    pub fn as_sum(&self) -> Option<&HashSet<TypeId>> {
        match self {
            TypeDefinition::Sum { variants, .. } => Some(variants),
            _ => None,
        }
    }

    /// Returns the builtin kind if this is a builtin type, or None otherwise.
    pub fn as_builtin(&self) -> Option<BuiltinKind> {
        match self {
            TypeDefinition::Builtin(kind) => Some(*kind),
            _ => None,
        }
    }
}

/// Container for managing types and their metadata.
/// 
/// The TypeContainer stores type definitions, manages TypeId allocation,
/// and maintains mappings from names (VarId) to TypeIds. Multiple names
/// can map to the same TypeId (type aliases).
pub struct TypeContainer {
    /// Arena storing all type definitions, indexed by TypeId
    types: Vec<TypeDefinition>,
    /// Maps variable identifiers to their TypeIds
    /// Multiple VarIds can map to the same TypeId (aliases)
    name_map: HashMap<VarId, TypeId>,
}

impl TypeContainer {
    /// Creates a new empty TypeContainer.
    pub fn new() -> Self {
        TypeContainer {
            types: Vec::new(),
            name_map: HashMap::new(),
        }
    }

    /// Creates a TypeContainer with all built-in types pre-registered.
    pub fn with_builtins() -> Self {
        let mut container = Self::new();
        container.register_builtin(BuiltinKind::I64, "i64");
        container.register_builtin(BuiltinKind::F64, "f64");
        container.register_builtin(BuiltinKind::Bool, "bool");
        container.register_builtin(BuiltinKind::Usize, "usize");
        container
    }

    /// Registers a builtin type with a name alias.
    fn register_builtin(&mut self, kind: BuiltinKind, name: &str) {
        let id = TypeId::new(self.types.len());
        self.types.push(TypeDefinition::Builtin(kind));
        self.name_map.insert(name.to_string(), id);
    }

    /// Creates a TypeContainer with a preallocated capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        TypeContainer {
            types: Vec::with_capacity(capacity),
            name_map: HashMap::with_capacity(capacity),
        }
    }

    /// Registers a new type definition and returns its TypeId.
    /// 
    /// This allocates a new TypeId for the given definition.
    /// To create name associations, use `add_name_alias`.
    pub fn create_type(&mut self, definition: TypeDefinition) -> TypeId {
        let id = TypeId::new(self.types.len());
        self.types.push(definition);
        id
    }

    /// Retrieves a type definition by its TypeId.
    /// 
    /// Returns None if the TypeId is invalid.
    pub fn get_type(&self, id: TypeId) -> Option<&TypeDefinition> {
        self.types.get(id.as_usize())
    }

    /// Mutably retrieves a type definition by its TypeId.
    /// 
    /// Returns None if the TypeId is invalid.
    pub fn get_type_mut(&mut self, id: TypeId) -> Option<&mut TypeDefinition> {
        self.types.get_mut(id.as_usize())
    }

    /// Associates a name (VarId) with a TypeId.
    /// 
    /// This allows multiple names to reference the same type (aliases).
    /// If the name was already associated, the previous TypeId is overwritten.
    pub fn add_name_alias(&mut self, name: VarId, type_id: TypeId) {
        self.name_map.insert(name, type_id);
    }

    /// Resolves a name to its TypeId.
    /// 
    /// Returns None if the name is not registered.
    pub fn resolve_name(&self, name: &VarId) -> Option<TypeId> {
        self.name_map.get(name).copied()
    }

    /// Retrieves a type by name resolution.
    /// 
    /// Combines name resolution and type lookup.
    /// Returns None if the name is not found or the TypeId is invalid.
    pub fn get_type_by_name(&self, name: &VarId) -> Option<&TypeDefinition> {
        self.resolve_name(name)
            .and_then(|id| self.get_type(id))
    }

    /// Mutably retrieves a type by name resolution.
    /// 
    /// Combines name resolution and mutable type lookup.
    pub fn get_type_by_name_mut(&mut self, name: &VarId) -> Option<&mut TypeDefinition> {
        let id = self.resolve_name(name)?;
        self.get_type_mut(id)
    }

    /// Returns the total number of types registered.
    pub fn type_count(&self) -> usize {
        self.types.len()
    }

    /// Returns the total number of name associations.
    pub fn name_count(&self) -> usize {
        self.name_map.len()
    }

    /// Returns all TypeIds currently registered.
    pub fn all_type_ids(&self) -> Vec<TypeId> {
        (0..self.types.len())
            .map(|i| TypeId::new(i))
            .collect()
    }

    /// Checks if a TypeId is valid.
    pub fn is_valid_type_id(&self, id: TypeId) -> bool {
        id.as_usize() < self.types.len()
    }

    /// Checks if a name is registered.
    pub fn has_name(&self, name: &VarId) -> bool {
        self.name_map.contains_key(name)
    }

    /// Gets all aliases (names) for a given TypeId.
    pub fn get_aliases(&self, type_id: TypeId) -> Vec<VarId> {
        self.name_map
            .iter()
            .filter_map(|(name, &id)| {
                if id == type_id {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect()
    }
}

impl Default for TypeContainer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_id_creation_and_access() {
        let id = TypeId::new(42);
        assert_eq!(id.as_usize(), 42);
        assert_eq!(id, TypeId::new(42));
    }

    #[test]
    fn test_type_definition_product() {
        let mut fields = HashMap::new();
        fields.insert("x".to_string(), TypeId::new(0));
        fields.insert("y".to_string(), TypeId::new(0));

        let def = TypeDefinition::product(fields);

        assert!(def.as_product().is_some());
        assert!(def.as_sum().is_none());

        let product = def.as_product().unwrap();
        assert_eq!(product.len(), 2);
        assert_eq!(product.get(&"x".to_string()), Some(&TypeId::new(0)));
        assert_eq!(product.get(&"y".to_string()), Some(&TypeId::new(0)));
    }

    #[test]
    fn test_type_definition_sum() {
        let mut variants = HashSet::new();
        variants.insert(TypeId::new(0));
        variants.insert(TypeId::new(1));

        let def = TypeDefinition::sum(variants);

        assert!(def.as_product().is_none());
        assert!(def.as_sum().is_some());

        let sum = def.as_sum().unwrap();
        assert_eq!(sum.len(), 2);
        assert!(sum.contains(&TypeId::new(0)));
        assert!(sum.contains(&TypeId::new(1)));
    }

    #[test]
    fn test_container_create_type() {
        let mut container = TypeContainer::new();
        let def = TypeDefinition::product(HashMap::new());

        let id1 = container.create_type(def.clone());
        let id2 = container.create_type(def);

        assert_eq!(id1, TypeId::new(0));
        assert_eq!(id2, TypeId::new(1));
        assert_eq!(container.type_count(), 2);
    }

    #[test]
    fn test_container_get_type() {
        let mut container = TypeContainer::new();
        let def = TypeDefinition::product(HashMap::new());

        let id = container.create_type(def);
        let retrieved = container.get_type(id);

        assert!(retrieved.is_some());
    }

    #[test]
    fn test_container_name_aliases() {
        let mut container = TypeContainer::new();
        let def = TypeDefinition::product(HashMap::new());

        let id = container.create_type(def);

        // Add multiple aliases for the same type
        container.add_name_alias("alias1".to_string(), id);
        container.add_name_alias("alias2".to_string(), id);

        assert_eq!(container.resolve_name(&"alias1".to_string()), Some(id));
        assert_eq!(container.resolve_name(&"alias2".to_string()), Some(id));
        assert_eq!(container.name_count(), 2);
    }

    #[test]
    fn test_container_get_type_by_name() {
        let mut container = TypeContainer::new();
        let def = TypeDefinition::product(HashMap::new());

        let id = container.create_type(def);
        container.add_name_alias("myname".to_string(), id);

        let retrieved = container.get_type_by_name(&"myname".to_string());
        assert!(retrieved.is_some());
    }

    #[test]
    fn test_container_get_aliases() {
        let mut container = TypeContainer::new();
        let def = TypeDefinition::product(HashMap::new());

        let id = container.create_type(def);
        container.add_name_alias("alias1".to_string(), id);
        container.add_name_alias("alias2".to_string(), id);

        let aliases = container.get_aliases(id);
        assert_eq!(aliases.len(), 2);
        assert!(aliases.contains(&"alias1".to_string()));
        assert!(aliases.contains(&"alias2".to_string()));
    }

    #[test]
    fn test_container_invalid_type_id() {
        let container = TypeContainer::new();
        assert!(container.get_type(TypeId::new(0)).is_none());
        assert!(!container.is_valid_type_id(TypeId::new(0)));
    }

    #[test]
    fn test_container_unresolved_name() {
        let container = TypeContainer::new();
        assert!(container.resolve_name(&"unknown".to_string()).is_none());
        assert!(!container.has_name(&"unknown".to_string()));
    }

    #[test]
    fn test_builtin_types_registration() {
        let container = TypeContainer::with_builtins();
        
        // Check that all builtins are registered
        assert!(container.has_name(&"i64".to_string()));
        assert!(container.has_name(&"f64".to_string()));
        assert!(container.has_name(&"bool".to_string()));
        assert!(container.has_name(&"usize".to_string()));
        
        // Check that we have exactly 4 types
        assert_eq!(container.type_count(), 4);
    }

    #[test]
    fn test_builtin_type_lookup() {
        let container = TypeContainer::with_builtins();
        
        let i64_type = container.get_type_by_name(&"i64".to_string());
        assert!(i64_type.is_some());
        assert_eq!(i64_type.unwrap().as_builtin(), Some(BuiltinKind::I64));
        
        let f64_type = container.get_type_by_name(&"f64".to_string());
        assert!(f64_type.is_some());
        assert_eq!(f64_type.unwrap().as_builtin(), Some(BuiltinKind::F64));
        
        let bool_type = container.get_type_by_name(&"bool".to_string());
        assert!(bool_type.is_some());
        assert_eq!(bool_type.unwrap().as_builtin(), Some(BuiltinKind::Bool));
        
        let usize_type = container.get_type_by_name(&"usize".to_string());
        assert!(usize_type.is_some());
        assert_eq!(usize_type.unwrap().as_builtin(), Some(BuiltinKind::Usize));
    }
}
