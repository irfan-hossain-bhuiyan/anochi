//! Type system module for the Anochi programming language.
//!
//! This module provides the core type infrastructure including:
//! - Type identification (TypeId)
//! - Type definitions (product types and sum types)
//! - Type container for managing types and name resolution
//! - Variable identifier aliasing

use crate::prelude::{HashCons, Mappable};
use crate::{ast::Identifier, prelude::HashPtr};
use std::collections::{BTreeMap, BTreeSet, HashMap};
pub mod type_def;
use enum_as_inner::EnumAsInner;
pub use type_def::TypeDefinition;


trait BuiltInType{}
/// Represents primitive types that are built into the type system.
/// These types are pre-registered in the TypeContainer and cannot be user-defined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum CompTimeBuiltinType {
    Int,
    Float,
    Bool,
    Usize,
    Type,
}
impl BuiltInType for CompTimeBuiltinType {}

#[derive(Debug, Clone)]
pub enum TypeLayout {
    Product(HashMap<Identifier, usize>),
    Sum(HashMap<TypeId, usize>),
    Simple,
}

#[derive(Debug, Clone)]
pub struct VmTypeMetaData {
    pub size: usize,
    pub layout: TypeLayout,
}

pub type TypeId = HashPtr<OptimizedTypeDefinition>;
impl TypeId{
    pub fn to_type_def(&self,container:&TypeContainer)->Option<TypeDefinition>{
        container.get_type_def(self)
    }
}

/// Generic type container that can hold product types, sum types, and builtins
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord,EnumAsInner)]
pub enum TypeGeneric<T,B:BuiltInType> {
    /// Product type: a collection of named fields (struct-like).
    /// Fields are stored as a BTreeMap mapping field names to their types.
    Product(BTreeMap<Identifier, T>),
    /// Sum type: a collection of variants (enum-like).
    /// Variants are stored as a set of types to prevent duplication.
    Sum(BTreeSet<T>),
    /// Built-in type: a primitive type provided by the language.
    Builtin(B),
    /// Reference type: a pointer to another type
    Reference(Box<T>),
}
pub type CompTimeTypeGeneric<T>=TypeGeneric<T,CompTimeBuiltinType>;

/// UnifiedTypeDefinition is the main type that can contain both direct types and references
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UnifiedTypeDefinition {
    TypeId(TypeId),
    TypeDef(CompTimeTypeGeneric<Self>),
}

impl From<TypeId> for UnifiedTypeDefinition {
    fn from(v: TypeId) -> Self {
        Self::TypeId(v)
    }
}

impl From<CompTimeTypeGeneric<Self>> for UnifiedTypeDefinition {
    fn from(v: CompTimeTypeGeneric<Self>) -> Self {
        Self::TypeDef(v)
    }
}

/// OptimizedTypeDefinition uses only TypeId references for efficient storage
#[derive(Debug,Ord,PartialEq, PartialOrd,Hash,Eq,Clone)]
pub struct OptimizedTypeDefinition(CompTimeTypeGeneric<TypeId>);
impl OptimizedTypeDefinition {
    pub fn is_product(&self)->bool{
        self.0.as_product().is_some()
    }
}


/// TypeContainer provides optimization and deduplication for types.
/// It stores TypeDefinitions and returns TypeIds for comparison and retrieval.
#[derive(Debug)]
pub struct TypeContainer {
    storage: HashCons<OptimizedTypeDefinition>,
    metadata: HashMap<TypeId, VmTypeMetaData>,
}

impl TypeContainer {
    pub fn new() -> Self {
        Self {
            storage: HashCons::new(),
            metadata: HashMap::new(),
        }
    }

    /// Store an OptimizedTypeDefinition and return its TypeId (private)
    fn store_type(&mut self, type_def: OptimizedTypeDefinition) -> TypeId {
        let type_id = self.storage.push(type_def.clone());
        let metadata = type_def.calculate_metadata(self);
        self.metadata.insert(type_id.clone(), metadata);
        type_id
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
    pub fn get_type(&self, hash_ptr: &TypeId) -> Option<&OptimizedTypeDefinition> {
        self.storage.get(hash_ptr)
    }
    pub fn get_type_def(&self, hash_ptr: &TypeId) -> Option<TypeDefinition> {
        Some(self.get_type(hash_ptr)?.clone().into_type_def(self))
    }
    pub fn has_type(&self, type_id: &TypeId) -> bool {
        self.storage.contains(type_id)
    }

    pub fn get_metadata(&self, type_id: &TypeId) -> Option<&VmTypeMetaData> {
        self.metadata.get(type_id)
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
impl<T, U:Ord> Mappable<T, U> for CompTimeTypeGeneric<T> {
    type Mapped = CompTimeTypeGeneric<U>;
    fn inner_map<F>(self, f:&mut F) -> Self::Mapped
    where
        F: FnMut(T) -> U,
    {
        match self {
            CompTimeTypeGeneric::Product(fields) => CompTimeTypeGeneric::Product(fields.inner_map(f)),
            
            CompTimeTypeGeneric::Sum(variants) => CompTimeTypeGeneric::Sum(variants.inner_map(f)),
            
            CompTimeTypeGeneric::Reference(inner) => CompTimeTypeGeneric::Reference(Box::new(f(*inner))),
            CompTimeTypeGeneric::Builtin(kind) => CompTimeTypeGeneric::Builtin(kind),
        }
    }
}


// Implementation for UnifiedTypeDefinition
impl UnifiedTypeDefinition {
    pub fn reference(r#type:Self)->Self{
        Self::TypeDef(CompTimeTypeGeneric::Reference(Box::new(r#type)))
    }
    pub fn product(fields: BTreeMap<Identifier, Self>) -> Self {
        Self::TypeDef(CompTimeTypeGeneric::Product(fields))
    }

    pub fn sum(variants: BTreeSet<Self>) -> Self {
        Self::TypeDef(CompTimeTypeGeneric::Sum(variants))
    }

    pub fn builtin(kind: CompTimeBuiltinType) -> Self {
        Self::TypeDef(CompTimeTypeGeneric::Builtin(kind))
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
    pub fn new(type_generic: CompTimeTypeGeneric<TypeId>) -> Self {
        Self(type_generic)
    }

    pub fn calculate_metadata(&self, container: &TypeContainer) -> VmTypeMetaData {
        match &self.0 {
            CompTimeTypeGeneric::Builtin(_) => VmTypeMetaData {
                size: 1,
                layout: TypeLayout::Simple,
            },
            CompTimeTypeGeneric::Reference(_) => VmTypeMetaData {
                size: 2,
                layout: TypeLayout::Simple,
            },
            CompTimeTypeGeneric::Product(fields) => {
                let mut offset = 0;
                let mut field_offsets = HashMap::new();
                
                for (field_name, type_id) in fields {
                    field_offsets.insert(field_name.clone(), offset);
                    let field_size = container.get_metadata(type_id)
                        .map(|meta| meta.size)
                        .unwrap_or(0);
                    offset += field_size;
                }
                
                VmTypeMetaData {
                    size: offset,
                    layout: TypeLayout::Product(field_offsets),
                }
            }
            CompTimeTypeGeneric::Sum(variants) => {
                let mut tag_index = 0;
                let mut variant_tags = HashMap::new();
                let mut max_size = 0;
                
                for variant_id in variants {
                    variant_tags.insert(variant_id.clone(), tag_index);
                    tag_index += 1;
                    
                    let variant_size = container.get_metadata(variant_id)
                        .map(|meta| meta.size)
                        .unwrap_or(0);
                    max_size = max_size.max(variant_size);
                }
                
                VmTypeMetaData {
                    size: max_size + 1,
                    layout: TypeLayout::Sum(variant_tags),
                }
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reference_type_cycle() {
        let mut container = TypeContainer::new();
        
        // Create i64 type
        let i64_def = UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Int);
        
        // Create reference to i64
        let ref_def = UnifiedTypeDefinition::reference(i64_def);
        
        // Store in container
        let type_id = container.store_unified_type(ref_def);
        
        // Retrieve back as TypeDefinition
        let retrieved_def = container.get_type_def(&type_id).expect("Should retrieve type");
        
        // Verify it is a reference to i64
        match retrieved_def.inner() {
            CompTimeTypeGeneric::Reference(inner) => {
                assert_eq!(inner.as_builtin(), Some(CompTimeBuiltinType::Int));
            }
            _ => panic!("Expected Reference type, got {:?}", retrieved_def),
        }
    }
}

