use std::{collections::HashMap, fmt::Display};
use crate::ast::{Identifier, Literal};
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
            Self::Product(x) => write!(f, "Product{x:?}"),
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
