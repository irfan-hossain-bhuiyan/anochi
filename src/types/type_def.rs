//! TypeDefinition module - contains the main TypeDefinition type
//!
//! TypeDefinition is now defined as TypeGeneric<Self>, representing
//! a recursive type structure.

use super::{BuiltinKind, TypeGeneric};
use crate::ast::Identifier;
use crate::prelude::Mappable;
use crate::types::{OptimizedTypeDefinition, TypeContainer};
use std::collections::{BTreeMap, BTreeSet};

/// TypeDefinition wraps TypeGeneric<Self> to create a proper recursive type
/// This represents a recursive type structure where types can contain other types
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeDefinition(TypeGeneric<Self>);

impl TypeDefinition {
    /// Get the inner TypeGeneric
    pub fn inner(&self) -> &TypeGeneric<TypeDefinition> {
        &self.0
    }

    /// Get builtin kind if this is a builtin type
    pub fn as_builtin(&self) -> Option<BuiltinKind> {
        match &self.0 {
            TypeGeneric::Builtin(kind) => Some(*kind),
            _ => None,
        }
    }

    /// Get product fields if this is a product type
    pub fn as_product(&self) -> Option<&BTreeMap<Identifier, TypeDefinition>> {
        match &self.0 {
            TypeGeneric::Product { fields } => Some(fields),
            _ => None,
        }
    }

    /// Get sum variants if this is a sum type
    pub fn as_sum(&self) -> Option<&BTreeSet<TypeDefinition>> {
        match &self.0 {
            TypeGeneric::Sum { variants } => Some(variants),
            _ => None,
        }
    }
    /// Get reference inner type if this is a reference type
    pub fn as_reference(&self) -> Option<&TypeDefinition> {
        match &self.0 {
            TypeGeneric::Reference(inner) => Some(inner),
            _ => None,
        }
    }
}

impl TypeDefinition {
    /// Creates a new product type (struct).
    pub fn product(fields: BTreeMap<Identifier, TypeDefinition>) -> Self {
        Self(TypeGeneric::Product { fields })
    }

    /// Creates a new sum type (enum).
    pub fn sum(variants: BTreeSet<TypeDefinition>) -> Self {
        Self(TypeGeneric::Sum { variants })
    }

    /// Creates a new builtin type.
    pub fn builtin(kind: BuiltinKind) -> Self {
        Self(TypeGeneric::Builtin(kind))
    }

    /// Creates a new reference type.
    pub fn reference(inner: TypeDefinition) -> Self {
        Self(TypeGeneric::Reference(Box::new(inner)))
    }

    /// Check if this type is compatible with another type
    pub fn is_compatible_with(&self, other: &Self) -> bool {
        self == other
    }

    /// Convert TypeDefinition to UnifiedTypeDefinition
    pub fn from_optimized(other: OptimizedTypeDefinition, container: &TypeContainer) -> TypeDefinition {
        TypeDefinition(other.0.inner_map(&mut |x|container.get_type_def(&x).unwrap()))
    }
    pub fn into_optimized(self,container: &mut TypeContainer)->OptimizedTypeDefinition{
        OptimizedTypeDefinition(self.0.inner_map(&mut |x|container.store_type_def(x)))
    }
}
impl OptimizedTypeDefinition{
    pub fn into_type_def(self,container:&TypeContainer)->TypeDefinition{
        TypeDefinition::from_optimized(self, container)
    }
}
