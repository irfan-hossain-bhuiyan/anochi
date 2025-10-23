//! Type system module for the Anochi programming language.
//!
//! This module provides the core type infrastructure including:
//! - Type identification (TypeId)
//! - Type definitions (product types and sum types)
//! - Type container for managing types and name resolution
//! - Variable identifier aliasing

use std::collections::{BTreeMap, BTreeSet, HashMap};
use crate::{ast::Identifier, prelude::HashPtr};
use std::collections::HashSet;
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
type TypeId = HashPtr<TypeDefinition>;
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
#[derive(Debug, Clone, PartialEq, Eq,Hash)]
pub enum TypeDefinition {
    /// Product type: a collection of named fields (struct-like).
    /// Fields are stored as a HashMap mapping field names to their types.
    Product {
        /// Fields in the product type, mapped by identifier
        fields: BTreeMap<Identifier, TypeId>,
    },
    /// Sum type: a collection of variants (enum-like).
    /// Variants are stored as a set of types to prevent duplication.
    Sum {
        /// Types associated with variants in the sum type
        variants: BTreeSet<TypeId>,
    },
    /// Built-in type: a primitive type provided by the language.
    Builtin(BuiltinKind),
}

impl TypeDefinition {
    /// Creates a new product type (struct).
    pub fn product(fields: BTreeMap<Identifier, TypeId>) -> Self {
        TypeDefinition::Product { fields }
    }

    /// Creates a new sum type (enum).
    pub fn sum(variants: BTreeSet<TypeId>) -> Self {
        TypeDefinition::Sum { variants }
    }

    /// Returns the fields of a product type, or None if this is a sum type.
    pub fn as_product(&self) -> Option<&BTreeMap<Identifier, TypeId>> {
        match self {
            TypeDefinition::Product { fields, .. } => Some(fields),
            _ => None,
        }
    }

    /// Returns the variants of a sum type, or None if this is a product type.
    pub fn as_sum(&self) -> Option<&BTreeSet<TypeId>> {
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





