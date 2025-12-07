use crate::ast::{BinaryOperator, Identifier, Literal, UnaryOperator};
use crate::types::{
    BuiltinKind, TypeContainer, TypeDefinition, TypeGeneric, TypeId, UnifiedTypeDefinition,
};
use crate::vm::tree_walk::vm_error::VmErrorType;
use crate::vm::tree_walk::scope_stack::VariableEntry;
use crate::prelude::IndexPtr;
use enum_as_inner::EnumAsInner;
use enum_dispatch::enum_dispatch;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::ops::Deref;
use std::{collections::HashMap, fmt::Display};

mod function;
pub use function::{VmFunc,FuncId};
#[enum_dispatch]
pub trait VmVal {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition>;
    fn get_type_of_value(&self) -> UnifiedTypeDefinition;

    fn into_type_definition(self, container: &mut TypeContainer) -> Option<TypeDefinition>
    where
        Self: Sized,
    {
        let type_id = container.store_unified_type(self.into_unified_type_definition()?);
        type_id.to_type_def(container)
    }

    fn get_type_id(self, type_container: &mut TypeContainer) -> Option<TypeId>
    where
        Self: Sized,
    {
        self.into_unified_type_definition()
            .map(|x| type_container.store_unified_type(x))
    }

    fn get_type_id_of_value(&self, container: &mut TypeContainer) -> TypeId {
        self.get_type_of_value().get_id(container)
    }

    fn of_type(&self, expected_type_id: TypeId, type_container: &mut TypeContainer) -> bool {
        let id = self.get_type_id_of_value(type_container);
        id == expected_type_id
    }
}


/// Primitive values that can be stored in the VM
#[derive(Debug, Clone, PartialEq)]
pub enum ValuePrimitive {
    Bool(bool),
    Integer(BigInt),
    Float(BigRational),
    Reference(IndexPtr<VariableEntry>),
}

impl VmVal for ValuePrimitive {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        None
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        let builtin_kind = match self {
            ValuePrimitive::Bool(_) => BuiltinKind::Bool,
            ValuePrimitive::Integer(_) => BuiltinKind::I64,
            ValuePrimitive::Float(_) => BuiltinKind::F64,
            ValuePrimitive::Reference(_) => BuiltinKind::Reference,
        };
        UnifiedTypeDefinition::builtin(builtin_kind)
    }
}

impl ValuePrimitive {
    pub fn from_i64(value: i64) -> Self {
        Self::Integer(BigInt::from(value))
    }

    pub fn from_f64(value: f64) -> Self {
        let rational =
            BigRational::from_float(value).unwrap_or_else(|| BigRational::from(BigInt::from(0)));
        Self::Float(rational)
    }

    pub fn from_bool(value: bool) -> Self {
        Self::Bool(value)
    }

    pub fn from_bigint(value: BigInt) -> Self {
        Self::Integer(value)
    }

    pub fn from_bigrational(value: BigRational) -> Self {
        Self::Float(value)
    }
}

impl Display for ValuePrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Reference(ptr) => write!(f, "&{:?}", ptr),
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

impl VmVal for StructValue {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        let mut type_fields = std::collections::BTreeMap::new();
        for (identifier, value) in self.into_iter() {
            let field_type = value.into_unified_type_definition()?;
            type_fields.insert(identifier, field_type);
        }
        Some(UnifiedTypeDefinition::TypeDef(TypeGeneric::Product {
            fields: type_fields,
        }))
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        for field_value in self.values() {
            if let VmValue::TypeId(_) = field_value {
                return UnifiedTypeDefinition::builtin(BuiltinKind::Type);
            }
        }
        let mut type_fields = std::collections::BTreeMap::new();
        for (field_name, field_value) in self.iter() {
            let field_type = field_value.get_type_of_value();
            type_fields.insert(field_name.clone(), field_type);
        }
        UnifiedTypeDefinition::TypeDef(TypeGeneric::Product {
            fields: type_fields,
        })
    }
}

impl StructValue {
    pub fn create_unit() -> Self {
        Self::default()
    }
}

impl VmVal for TypeId {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        Some(UnifiedTypeDefinition::TypeId(self))
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        UnifiedTypeDefinition::builtin(BuiltinKind::Type)
    }
}

impl VmVal for FuncId {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        None
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        UnifiedTypeDefinition::builtin(BuiltinKind::Type)
    }
}
#[enum_dispatch(VmVal)]
#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum VmValue {
    ValuePrimitive,
    StructValue,
    TypeId,
    FuncId,
}

impl Display for VmValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ValuePrimitive(x) => Display::fmt(x, f),
            Self::StructValue(fields) => {
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
            Self::TypeId(_) => write!(f, "Type"),
            Self::FuncId(fun) => write!(f, "Func({fun:?})"),
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
    pub fn create_unit() -> Self {
        Self::StructValue(StructValue::default())
    }

    pub fn is_null(&self) -> bool {
        matches!(self, VmValue::StructValue(x) if x.is_empty())
    }
}

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
        (_, VmValue::StructValue(_)) => Err(VmErrorType::InvalidOperation(
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
        (VmValue::StructValue(_), _) | (_, VmValue::StructValue(_)) => Err(VmErrorType::InvalidOperation(
            "Product operations not yet implemented".to_string(),
        )),

        // Type mismatch for other combinations
        _ => Err(VmErrorType::InvalidOperation(
            "The operation is not implemented yet".into(),
        )),
    }
}
