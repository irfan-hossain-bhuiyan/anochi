//! Type system module for the Anochi programming language.
//!
//! This module provides the core type infrastructure including:
//! - Type identification (TypeId)
//! - Type definitions (product types and sum types)
//! - Type container for managing types and name resolution
//! - Variable identifier aliasing

use crate::prelude::{HashCons, Mappable};
use crate::{ast::Identifier, prelude::HashPtr};
use std::collections::{BTreeMap, BTreeSet};
pub mod type_def;
pub use type_def::TypeDefinition;

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
    /// Type of types (meta-type)
    Type,
}

pub type TypeId = HashPtr<OptimizedTypeDefinition>;
impl TypeId{
    pub fn to_type_def(&self,container:&TypeContainer)->Option<TypeDefinition>{
        container.get_type_def(self)
    }
}

/// Generic type container that can hold product types, sum types, and builtins
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeGeneric<T> {
    /// Product type: a collection of named fields (struct-like).
    /// Fields are stored as a BTreeMap mapping field names to their types.
    Product {
        /// Fields in the product type, mapped by identifier
        fields: BTreeMap<Identifier, T>,
    },
    /// Sum type: a collection of variants (enum-like).
    /// Variants are stored as a set of types to prevent duplication.
    Sum {
        /// Types associated with variants in the sum type
        variants: BTreeSet<T>,
    },
    /// Built-in type: a primitive type provided by the language.
    Builtin(BuiltinKind),
}

impl<T> From<BuiltinKind> for TypeGeneric<T> {
    fn from(v: BuiltinKind) -> Self {
        Self::Builtin(v)
    }
}

impl<T> From<BTreeSet<T>> for TypeGeneric<T> {
    fn from(variants: BTreeSet<T>) -> Self {
        Self::Sum { variants }
    }
}

impl<T> From<BTreeMap<Identifier, T>> for TypeGeneric<T> {
    fn from(fields: BTreeMap<Identifier, T>) -> Self {
        Self::Product { fields }
    }
}

/// UnifiedTypeDefinition is the main type that can contain both direct types and references
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnifiedTypeDefinition {
    TypeId(TypeId),
    TypeDef(TypeGeneric<Self>),
}

impl From<TypeId> for UnifiedTypeDefinition {
    fn from(v: TypeId) -> Self {
        Self::TypeId(v)
    }
}

impl From<TypeGeneric<Self>> for UnifiedTypeDefinition {
    fn from(v: TypeGeneric<Self>) -> Self {
        Self::TypeDef(v)
    }
}

/// OptimizedTypeDefinition uses only TypeId references for efficient storage
#[derive(Debug,Ord,PartialEq, PartialOrd,Hash,Eq,Clone)]
pub struct OptimizedTypeDefinition(TypeGeneric<TypeId>);



/// TypeContainer provides optimization and deduplication for types.
/// It stores TypeDefinitions and returns TypeIds for comparison and retrieval.
#[derive(Debug)]
pub struct TypeContainer {
    storage: HashCons<OptimizedTypeDefinition>,
}

impl TypeContainer {
    pub fn new() -> Self {
        Self {
            storage: HashCons::new(),
        }
    }

    /// Store an OptimizedTypeDefinition and return its TypeId (private)
    fn store_type(&mut self, type_def: OptimizedTypeDefinition) -> TypeId {
        self.storage.push(type_def)
    }

    pub fn store_optimized(&mut self, optimized_type: OptimizedTypeDefinition) -> TypeId {
        self.store_type(optimized_type)
    }

    /// Store a UnifiedTypeDefinition by converting to optimized form and return its TypeId
    pub fn store_unified_type(&mut self, unified_def: UnifiedTypeDefinition) -> TypeId {
        let optimized:OptimizedTypeDefinition = match unified_def{
            UnifiedTypeDefinition::TypeId(x)=>return x,
            UnifiedTypeDefinition::TypeDef(x)=> OptimizedTypeDefinition(x.inner_map(&mut |x|self.store_unified_type(x)))
        };
        self.store_type(optimized)
    }
    fn get_type(&self, hash_ptr: &TypeId) -> Option<&OptimizedTypeDefinition> {
        self.storage.get(hash_ptr)
    }
    pub fn get_type_def(&self, hash_ptr: &TypeId) -> Option<TypeDefinition> {
        Some(self.get_type(hash_ptr)?.clone().into_type_def(self))
    }
    pub fn has_type(&self, type_id: &TypeId) -> bool {
        self.storage.contains(type_id)
    }

    fn store_type_def(&mut self, type1: TypeDefinition) -> TypeId {
        let optimized=type1.into_optimized(self);
        self.store_type(optimized)
    }
}

impl Default for TypeContainer {
    fn default() -> Self {
        Self::new()
    }
}

// Implementation of Mappable trait for TypeGeneric
impl<T, U:Ord> Mappable<T, U> for TypeGeneric<T> {
    type Mapped = TypeGeneric<U>;
    fn inner_map<F>(self, f:&mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U,
    {
        match self {
            TypeGeneric::Product { fields } => TypeGeneric::Product { fields: fields.inner_map(f) },
            
            TypeGeneric::Sum { variants } => TypeGeneric::Sum {variants: variants.inner_map(f)},
            
            TypeGeneric::Builtin(kind) => TypeGeneric::Builtin(kind),
        }
    }
}
// Implementation for TypeGeneric
impl<T> TypeGeneric<T> {
    /// Creates a new product type (struct).
    pub fn product(fields: BTreeMap<Identifier, T>) -> Self {
        TypeGeneric::Product { fields }
    }

    /// Creates a new sum type (enum).
    pub fn sum(variants: BTreeSet<T>) -> Self {
        TypeGeneric::Sum { variants }
    }

    /// Creates a new builtin type.
    pub fn builtin(kind: BuiltinKind) -> Self {
        TypeGeneric::Builtin(kind)
    }

    /// Get builtin kind if this is a builtin type
    pub fn as_builtin(&self) -> Option<BuiltinKind> {
        match self {
            TypeGeneric::Builtin(kind) => Some(*kind),
            _ => None,
        }
    }

    /// Get product fields if this is a product type
    pub fn as_product(&self) -> Option<&BTreeMap<Identifier, T>> {
        match self {
            TypeGeneric::Product { fields } => Some(fields),
            _ => None,
        }
    }

    /// Get sum variants if this is a sum type
    pub fn as_sum(&self) -> Option<&BTreeSet<T>> {
        match self {
            TypeGeneric::Sum { variants } => Some(variants),
            _ => None,
        }
    }
}

// Implementation for UnifiedTypeDefinition
impl UnifiedTypeDefinition {
    pub fn product(fields: BTreeMap<Identifier, Self>) -> Self {
        Self::TypeDef(TypeGeneric::Product { fields })
    }

    pub fn sum(variants: BTreeSet<Self>) -> Self {
        Self::TypeDef(TypeGeneric::Sum { variants })
    }

    pub fn builtin(kind: BuiltinKind) -> Self {
        Self::TypeDef(TypeGeneric::Builtin(kind))
    }

    pub fn type_id(id: TypeId) -> Self {
        Self::TypeId(id)
    }

    pub(crate) fn get_id(self,container:&mut TypeContainer) -> TypeId{
        container.store_unified_type(self)
    }

    //pub fn to_optimized(self, container: &mut TypeContainer) -> OptimizedTypeDefinition {
    //    match self {
    //        UnifiedTypeDefinition::TypeId(x) => OptimizedTypeDefinition::TypeId(x),
    //        UnifiedTypeDefinition::TypeDef(x) => {
    //            let a = x.map(|x| {
    //                let x = x.to_optimized(container);
    //                container.store_type(x)
    //            });
    //            OptimizedTypeDefinition::new(a)
    //        }
    //    }
    //}
}

// Implementation for OptimizedTypeDefinition
impl OptimizedTypeDefinition {
    pub fn new(type_generic: TypeGeneric<TypeId>) -> Self {
        Self(type_generic)
    }
}

