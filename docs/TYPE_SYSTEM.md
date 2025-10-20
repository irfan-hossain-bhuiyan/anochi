# Type System Documentation

## Overview

The type system in Anochi provides infrastructure for managing types in the language. It currently supports product types (structs) and sum types (enums), with easy extensibility for function types in the future.

## Core Components

### TypeId
- **Type**: Newtype wrapper around `usize`
- **Purpose**: Uniquely identifies a type in the type container
- **Design**: Newtype allows for easy changes to the underlying representation without affecting the rest of the codebase
- **Methods**:
  - `new(id: usize)` - Create a new TypeId
  - `as_usize()` - Get the underlying value
  - Implements standard traits: `Clone`, `Copy`, `PartialEq`, `Eq`, `Hash`

### VarId
- **Type**: Type alias for `Identifier` (currently `String`)
- **Purpose**: Represents variable/name identifiers
- **Note**: Will be replaced with a more efficient implementation once varId is fully implemented

### TypeDefinition
- **Purpose**: Represents the structure of a type
- **Variants**:
  - `Product { name: Identifier, fields: Vec<Field> }` - Struct-like types with named fields
  - `Sum { name: Identifier, variants: Vec<Variant> }` - Enum-like types with named variants

#### Product Type
- Represents struct-like types with named fields
- Each field has an identifier and a TypeId
- Example:
  ```
  Point { x: int, y: int }
  ```

#### Sum Type
- Represents enum-like types with variants
- Variants can have associated data (tuple or struct-like)
- Example:
  ```
  Option { None, Some(T) }
  ```

### Field
- Represents a single field in a product type
- Contains: `name: Identifier`, `type_id: TypeId`

### Variant
- Represents a single variant in a sum type
- Contains: `name: Identifier`, `associated_types: Vec<TypeId>`
- Can be unit variants (no data) using `Variant::unit(name)`

### TypeContainer
- **Purpose**: Centralized management of types
- **Responsibilities**:
  - Stores type definitions in an arena (Vec)
  - Allocates TypeIds for new types
  - Maintains name-to-TypeId mappings
  - Supports multiple names (aliases) for the same type

#### Key Methods

##### Type Management
- `create_type(definition: TypeDefinition) -> TypeId` - Register a new type and get its ID
- `get_type(id: TypeId) -> Option<&TypeDefinition>` - Retrieve type definition
- `get_type_mut(id: TypeId) -> Option<&mut TypeDefinition>` - Mutably retrieve type definition

##### Name Resolution
- `add_name_alias(name: VarId, type_id: TypeId)` - Associate a name with a type
- `resolve_name(name: &VarId) -> Option<TypeId>` - Get TypeId from name
- `get_type_by_name(name: &VarId) -> Option<&TypeDefinition>` - Combined lookup
- `has_name(name: &VarId) -> bool` - Check if name exists

##### Inspection
- `type_count() -> usize` - Total number of registered types
- `name_count() -> usize` - Total number of name associations
- `all_type_ids() -> Vec<TypeId>` - Get all registered TypeIds
- `is_valid_type_id(id: TypeId) -> bool` - Check if TypeId is valid
- `get_aliases(type_id: TypeId) -> Vec<VarId>` - Get all names for a type

## Usage Example

```rust
use crate::r#type::*;

let mut container = TypeContainer::new();

// Create a product type (struct)
let point_type = TypeDefinition::product(
    "Point".to_string(),
    vec![
        Field::new("x".to_string(), TypeId::new(0)),
        Field::new("y".to_string(), TypeId::new(0)),
    ],
);

let point_id = container.create_type(point_type);

// Create an alias
container.add_name_alias("Coordinate".to_string(), point_id);

// Resolve name
let resolved = container.resolve_name(&"Coordinate".to_string());
assert_eq!(resolved, Some(point_id));
```

## Design Decisions

1. **Newtype TypeId**: Allows future changes (e.g., 64-bit, custom encoding) without refactoring
2. **Arena Storage**: Simple Vec-based storage with O(1) lookups by index
3. **Pass-by-Reference**: TypeContainer is passed as mutable reference, avoiding globals in prototype phase
4. **Alias Support**: Multiple names can reference the same type, enabling type aliases
5. **Flexible Definition**: Enum-based TypeDefinition allows easy addition of new type categories

## Future Extensions

- **Function Types**: `Fn { params: Vec<TypeId>, return_type: TypeId }`
- **VarId Implementation**: Replace string-based identifiers with efficient unique IDs
- **Type Constraints**: Generic types and trait bounds
- **Type Inference**: Automatic type deduction
- **Custom Type Categories**: Domain-specific type forms
