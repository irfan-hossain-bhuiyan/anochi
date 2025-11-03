//! Type system module for the Anochi programming language.
//!
//! This module provides the core type infrastructure including:
//! - Type identification (TypeId)
//! - Type definitions (product types and sum types)
//! - Type container for managing types and name resolution
//! - Variable identifier aliasing

use crate::prelude::HashCons;
use crate::{ast::Identifier, prelude::HashPtr};
use std::collections::{BTreeMap, BTreeSet};

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

/// TypeRef can hold either a direct type definition or a reference to a stored type
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeRef {
    Direct(UnifiedTypeDefinition), // Direct unified type definition
    Reference(TypeId),             // Reference to stored type
}

/// UnifiedTypeDefinition is the main type that can contain both direct types and references
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnifiedTypeDefinition {
    Product {
        fields: BTreeMap<Identifier, TypeRef>,
    },
    Sum {
        variants: BTreeSet<TypeRef>,
    },
    Builtin(BuiltinKind),
}

/// OptimizedTypeDefinition uses only TypeId references for efficient storage
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum OptimizedTypeDefinition {
    Product {
        fields: BTreeMap<Identifier, TypeId>,
    },
    Sum {
        variants: BTreeSet<TypeId>,
    },
    Builtin(BuiltinKind),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum TypeDefinition {
    /// Product type: a collection of named fields (struct-like).
    /// Fields are stored as a HashMap mapping field names to their types.
    Product {
        /// Fields in the product type, mapped by identifier
        fields: BTreeMap<Identifier, TypeDefinition>,
    },
    /// Sum type: a collection of variants (enum-like).
    /// Variants are stored as a set of types to prevent duplication.
    Sum {
        /// Types associated with variants in the sum type
        variants: BTreeSet<TypeDefinition>,
    },
    /// Built-in type: a primitive type provided by the language.
    Builtin(BuiltinKind),
}

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
    pub fn store_optimized(&mut self, optimized_type:OptimizedTypeDefinition) ->TypeId{
        self.store_type(optimized_type)
    }
    /// Store a UnifiedTypeDefinition by converting to optimized form and return its TypeId
    pub fn store_unified_type(&mut self, unified_def: UnifiedTypeDefinition) -> TypeId {
        let optimized=unified_def.to_optimized(self);
        self.store_type(optimized)
    }
    pub fn store_type_definition(&mut self,type_def:TypeDefinition)->TypeId{
        let optimized=type_def.to_optimized(self);
        self.store_type(optimized)
    }

    /// Retrieve a TypeDefinition by its TypeId
    pub fn get_type(&self, type_id: &TypeId) -> Option<TypeDefinition> {
        self.storage
            .get(type_id)
            .map(|x| x.clone().to_type_definition(self))
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

#[cfg(test)]
mod flexible_test;

impl TypeDefinition {
    /// Creates a new product type (struct).
    pub fn product(fields: BTreeMap<Identifier, TypeDefinition>) -> Self {
        TypeDefinition::Product { fields }
    }

    /// Creates a new sum type (enum).
    pub fn sum(variants: BTreeSet<TypeDefinition>) -> Self {
        TypeDefinition::Sum { variants }
    }

    /// Creates a new builtin type.
    pub fn builtin(kind: BuiltinKind) -> Self {
        TypeDefinition::Builtin(kind)
    }

   /// Check if this type is compatible with another type
   pub fn is_compatible_with(&self, other: &Self) -> bool {
       self == other
   }

   /// Get builtin kind if this is a builtin type
   pub fn as_builtin(&self) -> Option<BuiltinKind> {
       match self {
           TypeDefinition::Builtin(kind) => Some(*kind),
           _ => None,
       }
   }

   /// Get product fields if this is a product type
   pub fn as_product(&self) -> Option<&BTreeMap<Identifier, TypeDefinition>> {
       match self {
           TypeDefinition::Product { fields } => Some(fields),
           _ => None,
       }
   }

   /// Get sum variants if this is a sum type
   pub fn as_sum(&self) -> Option<&BTreeSet<TypeDefinition>> {
       match self {
           TypeDefinition::Sum { variants } => Some(variants),
           _ => None,
       }
   }
    /// Convert TypeDefinition to UnifiedTypeDefinition
    pub fn to_unified(self) -> UnifiedTypeDefinition {
        match self {
            TypeDefinition::Product { fields } => {
                let unified_fields = fields
                    .into_iter()
                    .map(|(id, type_def)| (id, TypeRef::Direct(type_def.to_unified())))
                    .collect();
                UnifiedTypeDefinition::Product {
                    fields: unified_fields,
                }
            }
            TypeDefinition::Sum { variants } => {
                let unified_variants = variants
                    .into_iter()
                    .map(|variant| TypeRef::Direct(variant.to_unified()))
                    .collect();
                UnifiedTypeDefinition::Sum {
                    variants: unified_variants,
                }
            }
            TypeDefinition::Builtin(kind) => UnifiedTypeDefinition::Builtin(kind),
        }
    }

    /// Convert TypeDefinition directly to OptimizedTypeDefinition
    pub fn to_optimized(self, container: &mut TypeContainer) -> OptimizedTypeDefinition {
        let unified = self.to_unified();
        unified.to_optimized(container)
    }
}

impl UnifiedTypeDefinition {
    pub fn product(fields: BTreeMap<Identifier, TypeRef>) -> Self {
        UnifiedTypeDefinition::Product { fields }
    }

    pub fn sum(variants: BTreeSet<TypeRef>) -> Self {
        UnifiedTypeDefinition::Sum { variants }
    }

    pub fn builtin(kind: BuiltinKind) -> Self {
        UnifiedTypeDefinition::Builtin(kind)
    }

    /// Convert UnifiedTypeDefinition to OptimizedTypeDefinition using TypeContainer
    pub fn to_optimized(self, container: &mut TypeContainer) -> OptimizedTypeDefinition {
        match self {
            UnifiedTypeDefinition::Product { fields } => {
                let optimized_fields = fields
                    .into_iter()
                    .map(|(id, type_ref)| {
                        let type_id = match type_ref {
                            TypeRef::Direct(type_def) => {
                                container.store_unified_type(type_def)
                            }
                            TypeRef::Reference(type_id) => type_id,
                        };
                        (id, type_id)
                    })
                    .collect();
                OptimizedTypeDefinition::Product {
                    fields: optimized_fields,
                }
            }
            UnifiedTypeDefinition::Sum { variants } => {
                let optimized_variants = variants
                    .into_iter()
                    .map(|type_ref| match type_ref {
                        TypeRef::Direct(type_def) => {
                            container.store_unified_type(type_def)
                        }
                        TypeRef::Reference(type_id) => type_id,
                    })
                    .collect();
                OptimizedTypeDefinition::Sum {
                    variants: optimized_variants,
                }
            }
            UnifiedTypeDefinition::Builtin(kind) => OptimizedTypeDefinition::Builtin(kind),
        }
    }

    /// Add conversion method from OptimizedTypeDefinition back to UnifiedTypeDefinition
    pub fn from_optimized(optimized: OptimizedTypeDefinition) -> UnifiedTypeDefinition {
        match optimized {
            OptimizedTypeDefinition::Product { fields } => {
                let unified_fields = fields
                    .into_iter()
                    .map(|(id, type_id)| (id, TypeRef::Reference(type_id)))
                    .collect();
                UnifiedTypeDefinition::Product {
                    fields: unified_fields,
                }
            }
            OptimizedTypeDefinition::Sum { variants } => {
                let unified_variants = variants
                    .into_iter()
                    .map(TypeRef::Reference)
                    .collect();
                UnifiedTypeDefinition::Sum {
                    variants: unified_variants,
                }
            }
            OptimizedTypeDefinition::Builtin(kind) => UnifiedTypeDefinition::Builtin(kind),
        }
    }

    /// Convert UnifiedTypeDefinition to TypeDefinition with reference expansion
    pub fn to_type_definition_expanded(self, container: &TypeContainer) -> TypeDefinition {
        match self {
            UnifiedTypeDefinition::Product { fields } => {
                let type_def_fields = fields
                    .into_iter()
                    .map(|(id, type_ref)| {
                        let type_def = match type_ref {
                            TypeRef::Direct(unified_def) => {
                                unified_def.to_type_definition_expanded(container)
                            }
                            TypeRef::Reference(type_id) => {
                                if let Some(opt_def) = container.get_type(&type_id) {
                                    opt_def
                                } else {
                                    panic!("TypeId not found in container")
                                }
                            }
                        };
                        (id, type_def)
                    })
                    .collect();
                TypeDefinition::Product {
                    fields: type_def_fields,
                }
            }
            UnifiedTypeDefinition::Sum { variants } => {
                let type_def_variants = variants
                    .into_iter()
                    .map(|type_ref| match type_ref {
                        TypeRef::Direct(unified_def) => {
                            unified_def.to_type_definition_expanded(container)
                        }
                        TypeRef::Reference(type_id) => {
                            if let Some(opt_def) = container.get_type(&type_id) {
                                opt_def
                            } else {
                                panic!("TypeId not found in container")
                            }
                        }
                    })
                    .collect();
                TypeDefinition::Sum {
                    variants: type_def_variants,
                }
            }
            UnifiedTypeDefinition::Builtin(kind) => TypeDefinition::Builtin(kind),
        }
    }
}

impl OptimizedTypeDefinition {
    pub fn product(fields: BTreeMap<Identifier, TypeId>) -> Self {
        OptimizedTypeDefinition::Product { fields }
    }

    pub fn sum(variants: BTreeSet<TypeId>) -> Self {
        OptimizedTypeDefinition::Sum { variants }
    }

    pub fn builtin(kind: BuiltinKind) -> Self {
        OptimizedTypeDefinition::Builtin(kind)
    }

    pub fn as_product(&self) -> Option<&BTreeMap<Identifier, TypeId>> {
        match self {
            OptimizedTypeDefinition::Product { fields } => Some(fields),
            _ => None,
        }
    }

    pub fn as_sum(&self) -> Option<&BTreeSet<TypeId>> {
        match self {
            OptimizedTypeDefinition::Sum { variants } => Some(variants),
            _ => None,
        }
    }

    pub fn as_builtin(&self) -> Option<BuiltinKind> {
        match self {
            OptimizedTypeDefinition::Builtin(kind) => Some(*kind),
            _ => None,
        }
    }
    pub fn to_type_definition(self, container: &TypeContainer) -> TypeDefinition {
        match self {
            OptimizedTypeDefinition::Product { fields } => {
                let type_def_fields = fields
                    .into_iter()
                    .map(|(id, type_id)| {
                        if let Some(opt_def) = container.get_type(&type_id) {
                            (id, opt_def)
                        } else {
                            panic!("TypeId not found in container")
                        }
                    })
                    .collect();
                TypeDefinition::Product {
                    fields: type_def_fields,
                }
            }
            OptimizedTypeDefinition::Sum { variants } => {
                let type_def_variants = variants
                    .into_iter()
                    .map(|type_id| {
                        if let Some(opt_def) = container.get_type(&type_id) {
                            opt_def
                        } else {
                            panic!("TypeId not found in container")
                        }
                    })
                    .collect();
                TypeDefinition::Sum {
                    variants: type_def_variants,
                }
            }
            OptimizedTypeDefinition::Builtin(kind) => TypeDefinition::Builtin(kind),
        }
    }
}
