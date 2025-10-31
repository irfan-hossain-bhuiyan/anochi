//! Type system module for the Anochi programming language.
//!
//! This module provides the core type infrastructure including:
//! - Type identification (TypeId)
//! - Type definitions (product types and sum types)
//! - Type container for managing types and name resolution
//! - Variable identifier aliasing

use crate::{ast::Identifier, prelude::HashPtr};
use std::collections::{BTreeMap, BTreeSet};
use crate::prelude::HashCons;

/// Represents primitive types that are built into the type system.
/// These types are pre-registered in the TypeContainer and cannot be user-defined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
pub type TypeId = HashPtr<TypeDefinition>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeDefinition {
    /// Product type: a collection of named fields (struct-like).
    /// Fields are stored as a HashMap mapping field names to their types.
    Product {
        /// Fields in the product type, mapped by identifier
        fields: BTreeMap<Identifier, Box<TypeDefinition>>,
    },
    /// Sum type: a collection of variants (enum-like).
    /// Variants are stored as a set of types to prevent duplication.
    Sum {
        /// Types associated with variants in the sum type
        variants: BTreeSet<Box<TypeDefinition>>,
    },
    /// Built-in type: a primitive type provided by the language.
    Builtin(BuiltinKind),
}

/// TypeContainer provides optimization and deduplication for types.
/// It stores TypeDefinitions and returns TypeIds for comparison and retrieval.
#[derive(Debug)]
pub struct TypeContainer {
    storage: HashCons<TypeDefinition>,
}

impl TypeContainer {
    pub fn new() -> Self {
        Self {
            storage: HashCons::new(),
        }
    }

    /// Store a TypeDefinition and return its TypeId
    pub fn store_type(&mut self, type_def: TypeDefinition) -> TypeId {
        self.storage.push(type_def)
    }

    /// Retrieve a TypeDefinition by its TypeId
    pub fn get_type(&self, type_id: &TypeId) -> Option<&TypeDefinition> {
        self.storage.get(type_id)
    }
}

impl Default for TypeContainer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;

#[cfg(test)]
mod architecture_test;

#[cfg(test)]
mod container_test;

impl TypeDefinition {
    /// Creates a new product type (struct).
    pub fn product(fields: BTreeMap<Identifier, Box<TypeDefinition>>) -> Self {
        TypeDefinition::Product { fields }
    }

    /// Creates a new sum type (enum).
    pub fn sum(variants: BTreeSet<Box<TypeDefinition>>) -> Self {
        TypeDefinition::Sum { variants }
    }

    /// Creates a new builtin type.
    pub fn builtin(kind: BuiltinKind) -> Self {
        TypeDefinition::Builtin(kind)
    }

    /// Returns the fields of a product type, or None if this is a sum type.
    pub fn as_product(&self) -> Option<&BTreeMap<Identifier, Box<TypeDefinition>>> {
        match self {
            TypeDefinition::Product { fields, .. } => Some(fields),
            _ => None,
        }
    }

    /// Returns the variants of a sum type, or None if this is a product type.
    pub fn as_sum(&self) -> Option<&BTreeSet<Box<TypeDefinition>>> {
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

    /// Checks if this type is compatible with another type.
    /// This is where the main type logic resides.
    pub fn is_compatible_with(&self, other: &TypeDefinition) -> bool {
        match (self, other) {
            (TypeDefinition::Builtin(a), TypeDefinition::Builtin(b)) => a == b,
            (TypeDefinition::Product { fields: fields_a }, TypeDefinition::Product { fields: fields_b }) => {
                fields_a.len() == fields_b.len() &&
                fields_a.iter().all(|(key, type_a)| {
                    fields_b.get(key).map_or(false, |type_b| type_a.is_compatible_with(type_b))
                })
            },
            (TypeDefinition::Sum { variants: variants_a }, TypeDefinition::Sum { variants: variants_b }) => {
                variants_a.len() == variants_b.len() &&
                variants_a.iter().all(|variant_a| {
                    variants_b.iter().any(|variant_b| variant_a.is_compatible_with(variant_b))
                })
            },
            _ => false,
        }
    }
}
