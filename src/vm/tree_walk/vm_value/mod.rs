use crate::ast::{BinaryOperator, Identifier, Literal, StatNode, UnaryOperator};
use crate::prelude::IndexPtr;
use crate::types::{
    BuiltinKind, TypeContainer, TypeDefinition, TypeGeneric, TypeId, UnifiedTypeDefinition,
};
use num_bigint::BigInt;
use num_rational::BigRational;
use std::ops::Deref;
use std::{collections::HashMap, fmt::Display};
mod function;
#[cfg(test)]
mod tests;
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct VmFunc {
    param: TypeId,
    output: Option<TypeId>,
    body: StatNode<()>,
}

impl VmFunc {
    pub fn new_checked<T>(
        param: TypeId,
        output: Option<TypeId>,
        body: StatNode<T>,
        type_container: &TypeContainer,
    ) -> Option<Self> {
        if !type_container.get_type(&param)?.is_product() {
            return None;
        }
        Some(Self {
            param,
            output,
            body: body.to_null(),
        })
    }
    pub fn new<T>(param: TypeId,
        output: Option<TypeId>,
        body: StatNode<T>,
        type_container: &TypeContainer,
    )->Self{
        Self::new_checked(param, output, body, type_container).unwrap()
    }
    pub fn get_param(&self) -> TypeId {
        self.param
    }
    pub fn update_output_type(&mut self,output:TypeId)->Result<(), VmErrorType>{
        match self.output {
            None=> {self.output=Some(output);Ok(())},
            Some(x) if x==output=>{self.output=Some(output);Ok(())},
            _ =>{return Err(VmErrorType::TypeMismatch(""))}
        }
    }

    pub(crate) fn get_statement(&self) -> &StatNode<()> {
        &self.body
    }
}
pub type FuncId = IndexPtr<VmFunc>;
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
            Literal::String(_) => {
                panic!("String literals should be handled as arrays, not primitives")
            }
            Literal::Identifier(_) => {
                panic!("Identifiers should be resolved before conversion to primitive")
            }
        }
    }
}
#[derive(Debug, Default, Clone, PartialEq)]
pub struct StructValue {
    value: HashMap<Identifier, VmValue>,
}
impl StructValue {
    pub(crate) fn new(product: HashMap<Identifier, VmValue>) -> Self {
        Self { value: product }
    }
}
impl IntoIterator for StructValue {
    type Item = (Identifier, VmValue);
    type IntoIter = std::collections::hash_map::IntoIter<Identifier, VmValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.value.into_iter()
    }
}
impl Deref for StructValue {
    fn deref(&self) -> &Self::Target {
        &self.value
    }

    type Target = HashMap<Identifier, VmValue>;
}
/// The main value type used in the VM
#[derive(Debug, Clone, PartialEq)]
pub enum VmValue {
    ValuePrimitive(ValuePrimitive),
    Product(StructValue),
    Type(TypeId),
    Func(FuncId),
}

impl Display for VmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ValuePrimitive(x) => Display::fmt(x, f),
            Self::Product(fields) => {
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in fields.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}={value}")?;
                    first = false;
                }
                write!(f, "}}")?;
                Ok(())
            }
            Self::Type(_) => write!(f, "Type"),
            Self::Func(fun) => write!(f, "Func({fun:?})"),
        }
    }
}

impl From<Literal> for VmValue {
    fn from(v: Literal) -> Self {
        match v {
            Literal::String(_) => {
                panic!("String literals should be handled as arrays, not primitives")
            }
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
        let rational =
            BigRational::from_float(value).unwrap_or_else(|| BigRational::from(BigInt::from(0)));
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
    ///
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

                for (identifier, value) in fields.into_iter() {
                    let field_type = value.into_unified_type_definition()?;
                    type_fields.insert(identifier, field_type);
                }

                Some(UnifiedTypeDefinition::TypeDef(TypeGeneric::Product {
                    fields: type_fields,
                }))
            }
            VmValue::Type(type_id) => {
                // A Type value represents a type expression
                Some(UnifiedTypeDefinition::TypeId(type_id))
            }
            VmValue::Func(func) => None,
        }
    }
    pub fn into_type_definition(self, container: &mut TypeContainer) -> Option<TypeDefinition> {
        let type_id = container.store_unified_type(self.into_unified_type_definition()?);
        type_id.to_type_def(container)
    }
    pub fn into_type_id(self, type_container: &mut TypeContainer) -> Option<TypeId> {
        self.into_unified_type_definition()
            .map(|x| type_container.store_unified_type(x))
    }

    /// Get the type of this VmValue as UnifiedTypeDefinition
    ///
    /// Returns the type information for this value without requiring a TypeContainer.
    /// Returns None for mixed type/value products (not yet implemented).
    pub fn get_type_id_of_value(&self, container: &mut TypeContainer) -> TypeId {
        self.get_type_of_value().get_id(container)
    }
    pub fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        match self {
            VmValue::Type(_) => {
                // Return "type of type" (meta-type)
                UnifiedTypeDefinition::builtin(BuiltinKind::Type)
            }
            VmValue::ValuePrimitive(primitive) => {
                let builtin_kind = match primitive {
                    ValuePrimitive::Bool(_) => BuiltinKind::Bool,
                    ValuePrimitive::Integer(_) => BuiltinKind::I64,
                    ValuePrimitive::Float(_) => BuiltinKind::F64,
                };
                UnifiedTypeDefinition::builtin(builtin_kind)
            }
            VmValue::Product(fields) => {
                // Check if product contains mixed types and values
                for field_value in fields.values() {
                    match field_value {
                        VmValue::Type(_) => {
                            return UnifiedTypeDefinition::builtin(BuiltinKind::Type);
                        }
                        VmValue::ValuePrimitive(_) | VmValue::Product(_) | VmValue::Func(_) => {}
                    }
                }
                // Create product type from field types
                let mut type_fields = std::collections::BTreeMap::new();
                for (field_name, field_value) in fields.iter() {
                    let field_type = field_value.get_type_of_value();
                    type_fields.insert(field_name.clone(), field_type);
                }

                UnifiedTypeDefinition::TypeDef(TypeGeneric::Product {
                    fields: type_fields,
                })
            }
            VmValue::Func(func)=>UnifiedTypeDefinition::builtin(BuiltinKind::Type),
        }
    }

    pub fn of_type(&self, expected_type_id: TypeId, type_container: &mut TypeContainer) -> bool {
        let id = self.get_type_id_of_value(type_container);
        id == expected_type_id
    }

    pub(crate) fn from_func(func: FuncId) -> Self {
        Self::Func(func)
    }

    pub(crate) fn as_product(self) -> Option<StructValue> {
        match self {
            VmValue::Product(product) => Some(product),
            _ => None,
        }
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
pub fn evaluate_unary_op(operator: &UnaryOperator, operand: &VmValue) -> Result<VmValue, VmErrorType> {
    match (operator, operand) {
        (UnaryOperator::Minus, VmValue::ValuePrimitive(ValuePrimitive::Integer(i))) => {
            Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(-i)))
        }
        (UnaryOperator::Minus, VmValue::ValuePrimitive(ValuePrimitive::Float(f))) => {
            Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(-f)))
        }
        (UnaryOperator::Not, VmValue::ValuePrimitive(ValuePrimitive::Bool(b))) => {
            Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(!b)))
        }
        (_, VmValue::Product(_)) => Err(VmErrorType::InvalidOperation(
            "Product operations not yet implemented".to_string(),
        )),
        _ => Err(VmErrorType::InvalidOperation(format!(
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
) -> Result<VmValue, VmErrorType> {
    match (left, right) {
        // Bool operations
        (
            VmValue::ValuePrimitive(ValuePrimitive::Bool(l)),
            VmValue::ValuePrimitive(ValuePrimitive::Bool(r)),
        ) => match operator {
            BinaryOperator::Equal => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l == r))),
            BinaryOperator::NotEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l != r))),
            BinaryOperator::And => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(*l && *r))),
            BinaryOperator::Or => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(*l || *r))),
            BinaryOperator::Less => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(!*l && *r))), // false < true
            BinaryOperator::LessEqual => {
                Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(!*l || *r)))
            } // false <= true, true <= true
            BinaryOperator::Greater => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(*l && !*r))), // true > false
            BinaryOperator::GreaterEqual => {
                Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(*l || !*r)))
            } // true >= false, true >= true
            _ => Err(VmErrorType::InvalidOperation(format!(
                "Cannot apply {operator:?} to Bool"
            ))),
        },
        // Integer operations
        (
            VmValue::ValuePrimitive(ValuePrimitive::Integer(l)),
            VmValue::ValuePrimitive(ValuePrimitive::Integer(r)),
        ) => match operator {
            BinaryOperator::Plus => Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(l + r))),
            BinaryOperator::Minus => Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(l - r))),
            BinaryOperator::Multiply => Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(l * r))),
            BinaryOperator::Divide => {
                if *r == BigInt::from(0) {
                    Err(VmErrorType::DivisionByZero)
                } else {
                    Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(l / r)))
                }
            }
            BinaryOperator::Modulo => {
                if *r == BigInt::from(0) {
                    Err(VmErrorType::DivisionByZero)
                } else {
                    Ok(VmValue::ValuePrimitive(ValuePrimitive::Integer(l % r)))
                }
            }
            BinaryOperator::Equal => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l == r))),
            BinaryOperator::NotEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l != r))),
            BinaryOperator::Less => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l < r))),
            BinaryOperator::LessEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l <= r))),
            BinaryOperator::Greater => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l > r))),
            BinaryOperator::GreaterEqual => {
                Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l >= r)))
            }
            _ => Err(VmErrorType::InvalidOperation(format!(
                "Cannot apply {operator:?} to integer"
            ))),
        },
        // Float operations
        (
            VmValue::ValuePrimitive(ValuePrimitive::Float(l)),
            VmValue::ValuePrimitive(ValuePrimitive::Float(r)),
        ) => match operator {
            BinaryOperator::Plus => Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(l + r))),
            BinaryOperator::Minus => Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(l - r))),
            BinaryOperator::Multiply => Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(l * r))),
            BinaryOperator::Divide => {
                if *r == BigRational::from(BigInt::from(0)) {
                    Err(VmErrorType::DivisionByZero)
                } else {
                    Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(l / r)))
                }
            }
            BinaryOperator::Modulo => {
                if *r == BigRational::from(BigInt::from(0)) {
                    Err(VmErrorType::DivisionByZero)
                } else {
                    Ok(VmValue::ValuePrimitive(ValuePrimitive::Float(l % r)))
                }
            }
            BinaryOperator::Equal => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l == r))),
            BinaryOperator::NotEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l != r))),
            BinaryOperator::Less => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l < r))),
            BinaryOperator::LessEqual => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l <= r))),
            BinaryOperator::Greater => Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l > r))),
            BinaryOperator::GreaterEqual => {
                Ok(VmValue::ValuePrimitive(ValuePrimitive::Bool(l >= r)))
            }
            _ => Err(VmErrorType::InvalidOperation(format!(
                "Cannot apply {operator:?} to float"
            ))),
        },

        // Handle Product types - all operations return error for now
        (VmValue::Product(_), _) | (_, VmValue::Product(_)) => Err(VmErrorType::InvalidOperation(
            "Product operations not yet implemented".to_string(),
        )),

        // Type mismatch for other combinations
        _ => Err(VmErrorType::InvalidOperation(
            "The operation is not implemented yet".into(),
        )),
    }
}
