use std::{collections::HashMap, fmt::Display};
use crate::ast::{Identifier, Literal};
use crate::typing::{BuiltinKind, TypeContainer, TypeId, UnifiedTypeDefinition};
use num_bigint::BigInt;
use num_rational::BigRational;

/// Primitive values that can be stored in the VM
#[derive(Debug, Clone, PartialEq)]
pub enum ValuePrimitive {
    Bool(bool),
    Integer(BigInt),
    Float(BigRational),
}

impl Display for ValuePrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
        }
    }
}

impl From<Literal> for ValuePrimitive {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Bool(b) => Self::Bool(b),
            Literal::Integer(i) => Self::Integer(i),
            Literal::Float(f) => Self::Float(f),
            Literal::String(_) => panic!("String literals should be handled as arrays, not primitives"),
            Literal::Identifier(_) => panic!("Identifiers should be resolved before conversion to primitive"),
        }
    }
}

/// The main value type used in the VM
#[derive(Debug, Clone, PartialEq)]
pub enum VmValue {
    ValuePrimitive(ValuePrimitive),
    Product(HashMap<Identifier, VmValue>),
    Type(crate::typing::TypeId),
}

impl Display for VmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ValuePrimitive(x) => Display::fmt(x, f),
            Self::Product(fields) => {
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in fields {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}={}", key, value)?;
                    first = false;
                }
                write!(f, "}}")?;
                Ok(())
            },
            Self::Type(_) => write!(f, "Type"),
        }
    }
}

impl From<Literal> for VmValue {
    fn from(v: Literal) -> Self {
        match v {
            Literal::String(_) => panic!("String literals should be handled as arrays, not primitives"),
            Literal::Identifier(_) => panic!("Identifiers should be resolved before conversion"),
            _ => Self::ValuePrimitive(ValuePrimitive::from(v)),
        }
    }
}

impl VmValue {
    /// Create VmValue from i64
    pub fn from_i64(value: i64) -> Self {
        Self::ValuePrimitive(ValuePrimitive::Integer(BigInt::from(value)))
    }
    
    /// Create VmValue from f64
    pub fn from_f64(value: f64) -> Self {
        let rational = BigRational::from_float(value)
            .unwrap_or_else(|| BigRational::from(BigInt::from(0)));
        Self::ValuePrimitive(ValuePrimitive::Float(rational))
    }
    
    /// Create VmValue from bool
    pub fn from_bool(value: bool) -> Self {
        Self::ValuePrimitive(ValuePrimitive::Bool(value))
    }
    
    /// Create VmValue from BigInt
    pub fn from_bigint(value: BigInt) -> Self {
        Self::ValuePrimitive(ValuePrimitive::Integer(value))
    }
    
    /// Create VmValue from BigRational
    pub fn from_bigrational(value: BigRational) -> Self {
        Self::ValuePrimitive(ValuePrimitive::Float(value))
    }

    /// Convert VmValue to UnifiedTypeDefinition for type expressions only
    /// 
    /// This function only works on type expressions, not value expressions.
    /// Type expressions can be:
    /// - Type(id) - direct type reference
    /// - {a=Type(i64), b=Type(i64)} - product of types
    /// - {a={x=Type(i64),y=Type(i64)}, b=Type(i64)} - nested type expressions
    /// Returns None if the value is not a valid type expression.
    pub fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        match self {
            VmValue::ValuePrimitive(_) => {
                // Primitive values are not type expressions
                None
            }
            VmValue::Product(fields) => {
                // Check if all fields are valid type expressions (recursive)
                let mut type_fields = std::collections::BTreeMap::new();
                
                for (identifier, value) in fields {
                    let field_type = value.into_unified_type_definition()?;
                    type_fields.insert(identifier, field_type);
                }
                
                Some(UnifiedTypeDefinition::Product { fields: type_fields })
            }
            VmValue::Type(type_id) => {
                // A Type value represents a type expression
                Some(UnifiedTypeDefinition::TypeId(type_id))
            }
        }
    }
    pub fn into_type_id(self, type_container: &mut TypeContainer) -> Option<TypeId> {
        self.into_unified_type_definition().map(|x| type_container.store_unified_type(x))
    }

    /// Get the type of this VmValue as UnifiedTypeDefinition
    /// 
    /// Returns the type information for this value without requiring a TypeContainer.
    /// Returns None for mixed type/value products (not yet implemented).
    pub fn get_type_id_of_value(&self,container:&mut TypeContainer)->Option<TypeId>{
        self.get_type_of_value().map(|x|container.store_unified_type(x))
    }
    pub fn get_type_of_value(&self) -> Option<UnifiedTypeDefinition> {
        match self {
            VmValue::Type(_) => {
                // Return "type of type" (meta-type)
                Some(UnifiedTypeDefinition::Builtin(BuiltinKind::Type))
            }
            VmValue::ValuePrimitive(primitive) => {
                let builtin_kind = match primitive {
                    ValuePrimitive::Bool(_) => BuiltinKind::Bool,
                    ValuePrimitive::Integer(_) => BuiltinKind::I64,
                    ValuePrimitive::Float(_) => BuiltinKind::F64,
                };
                Some(UnifiedTypeDefinition::Builtin(builtin_kind))
            }
            VmValue::Product(fields) => {
                // Check if product contains mixed types and values
                let mut has_types = false;
                let mut has_values = false;

                for field_value in fields.values() {
                    match field_value {
                        VmValue::Type(_) => has_types = true,
                        VmValue::ValuePrimitive(_) | VmValue::Product(_) => has_values = true,
                    }
                }

                if has_types && has_values {
                    // Mixed type/value products not yet implemented
                    return None;
                }

                // Create product type from field types
                let mut type_fields = std::collections::BTreeMap::new();
                for (field_name, field_value) in fields {
                    if let Some(field_type) = field_value.get_type_of_value() {
                        type_fields.insert(field_name.clone(), field_type);
                    } else {
                        // If any field type cannot be determined, return None
                        return None;
                    }
                }

                Some(UnifiedTypeDefinition::Product {
                    fields: type_fields,
                })
            }
        }
    }

    pub fn of_type(&self, expected_type_id: TypeId,type_container: &mut TypeContainer) -> bool {
        self.get_type_id_of_value(type_container).unwrap()==expected_type_id
    }
}

use super::*;
/// Evaluates a unary operation on a VmValue.
///
/// This is a standalone function that handles negation and logical NOT operations 
/// on numeric and boolean values. It doesn't require any VM state and can be used independently.
///
/// # Arguments
///
/// * `operator` - The unary operator
/// * `operand` - The operand value
///
/// # Returns
///
/// A `VmResult` containing the computed result or an error.
pub fn evaluate_unary_op(operator: &UnaryOperator, operand: &VmValue) -> VmResult {
    match (operator, operand) {
        (UnaryOperator::Minus, VmValue::ValuePrimitive(ValuePrimitive::Integer(i))) => {
            Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(-i)))
        },
        (UnaryOperator::Minus, VmValue::ValuePrimitive(ValuePrimitive::Float(f))) => {
            Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(-f)))
        },
        (UnaryOperator::Not, VmValue::ValuePrimitive(ValuePrimitive::Bool(b))) => {
            Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(!b)))
        },
        (_, VmValue::Product(_)) => Err(VmError::InvalidOperation("Product operations not yet implemented".to_string())),
        _ => Err(VmError::InvalidOperation(format!(
            "Cannot apply {operator:?} to {operand:?}",
        ))),
    }
}

/// Evaluates a binary operation between two VmValues.
///
/// This is a standalone function that handles arithmetic, comparison, and logical 
/// operations with type coercion between integers and floats when necessary.
/// It doesn't require any VM state and can be used independently.
///
/// # Arguments
///
/// * `left` - The left operand value
/// * `operator` - The binary operator
/// * `right` - The right operand value
///
/// # Returns
///
/// A `VmResult` containing the computed result or an error.
pub fn evaluate_binary_op(
    left: &VmValue,
    operator: &BinaryOperator,
    right: &VmValue,
) -> VmResult {
    match (left, right) {
        // Bool operations
        (VmValue::ValuePrimitive(ValuePrimitive::Bool(l)), VmValue::ValuePrimitive(ValuePrimitive::Bool(r))) => match operator {
            BinaryOperator::Equal => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l == r))),
            BinaryOperator::NotEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l != r))),
            BinaryOperator::And => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(*l && *r))),
            BinaryOperator::Or => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(*l || *r))),
            BinaryOperator::Less => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(!*l && *r))), // false < true
            BinaryOperator::LessEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(!*l || *r))), // false <= true, true <= true
            BinaryOperator::Greater => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(*l && !*r))),   // true > false
            BinaryOperator::GreaterEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(*l || !*r))), // true >= false, true >= true
            _ => Err(VmError::InvalidOperation(format!(
                "Cannot apply {operator:?} to Bool"
            ))),
        },
        // Integer operations
        (VmValue::ValuePrimitive(ValuePrimitive::Integer(l)), VmValue::ValuePrimitive(ValuePrimitive::Integer(r))) => match operator {
            BinaryOperator::Plus => Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(l + r))),
            BinaryOperator::Minus => Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(l - r))),
            BinaryOperator::Multiply => Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(l * r))),
            BinaryOperator::Divide => {
                if *r == BigInt::from(0) {
                    Err(VmError::DivisionByZero)
                } else {
                    Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(l / r)))
                }
            }
            BinaryOperator::Modulo => {
                if *r == BigInt::from(0) {
                    Err(VmError::DivisionByZero)
                } else {
                    Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(l % r)))
                }
            }
            BinaryOperator::Equal => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l == r))),
            BinaryOperator::NotEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l != r))),
            BinaryOperator::Less => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l < r))),
            BinaryOperator::LessEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l <= r))),
            BinaryOperator::Greater => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l > r))),
            BinaryOperator::GreaterEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l >= r))),
            _ => Err(VmError::InvalidOperation(format!(
                "Cannot apply {operator:?} to integer"
            ))),
        },
        // Float operations
        (VmValue::ValuePrimitive(ValuePrimitive::Float(l)), VmValue::ValuePrimitive(ValuePrimitive::Float(r))) => match operator {
            BinaryOperator::Plus => Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(l + r))),
            BinaryOperator::Minus => Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(l - r))),
            BinaryOperator::Multiply => Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(l * r))),
            BinaryOperator::Divide => {
                if *r == BigRational::from(BigInt::from(0)) {
                    Err(VmError::DivisionByZero)
                } else {
                    Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(l / r)))
                }
            }
            BinaryOperator::Modulo => {
                if *r == BigRational::from(BigInt::from(0)) {
                    Err(VmError::DivisionByZero)
                } else {
                    Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(l % r)))
                }
            }
            BinaryOperator::Equal => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l == r))),
            BinaryOperator::NotEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l != r))),
            BinaryOperator::Less => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l < r))),
            BinaryOperator::LessEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l <= r))),
            BinaryOperator::Greater => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l > r))),
            BinaryOperator::GreaterEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l >= r))),
            _ => Err(VmError::InvalidOperation(format!(
                "Cannot apply {operator:?} to float"
            ))),
        },

        // Handle Product types - all operations return error for now
        (VmValue::Product(_), _) | (_, VmValue::Product(_)) => {
            Err(VmError::InvalidOperation("Product operations not yet implemented".to_string()))
        }
        
        // Type mismatch for other combinations
        _ => Err(VmError::InvalidOperation("The operation is not implemented yet".into())),
    }
}
