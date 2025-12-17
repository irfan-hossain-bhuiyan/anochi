use crate::ast::{BinaryOperator, Identifier, Literal, UnaryOperator};
use crate::types::{
    CompTimeBuiltinType, CompTimeTypeGeneric, TypeContainer, TypeDefinition, TypeId, UnifiedTypeDefinition
};
use crate::vm::tree_walk::VmUnit;
use crate::vm::tree_walk::scope_stack::VmPtr;
use crate::vm::tree_walk::vm_error::VmErrorType;
use enum_as_inner::EnumAsInner;
use enum_dispatch::enum_dispatch;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::any::Any;
use std::collections::BTreeMap;
use std::fmt::Display;
use std::ops::Deref;

mod function;
pub use function::{FuncId, VmFunc};

#[enum_dispatch]
pub trait ParsedValueType 
where Self:Sized{
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition>;
    fn get_type_of_value(&self) -> UnifiedTypeDefinition;
    fn to_vm_units(self) -> Vec<VmUnit>;
    fn to_vm_value_generalized(self,type_container: &mut TypeContainer)->VmValueGeneralized{
        let type_id=self.get_type_id_of_value(type_container);
        VmValueGeneralized { bits: self.to_vm_units(), r#type: type_id }
    }
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
    Reference(VmPtr, TypeId),
}

impl ParsedValueType for ValuePrimitive {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        None
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        match self {
            ValuePrimitive::Bool(_) => UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Bool),
            ValuePrimitive::Integer(_) => UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Int),
            ValuePrimitive::Float(_) => UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Float),
            ValuePrimitive::Reference(_, type_id) => {
                UnifiedTypeDefinition::reference(UnifiedTypeDefinition::type_id(type_id.clone()))
            }
        }
    }

    fn to_vm_units(self) -> Vec<crate::vm::tree_walk::VmUnit> {
        match self {
            ValuePrimitive::Bool(b) => vec![VmUnit::Bool(b)],
            ValuePrimitive::Integer(i) => vec![VmUnit::Integer(i)],
            ValuePrimitive::Float(f) => vec![VmUnit::Float(f)],
            ValuePrimitive::Reference(stack_pos, _) => {
                vec![VmUnit::Usize(stack_pos.as_index())]
            }
        }
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

    pub fn binary_op(
        &self,
        operator: &BinaryOperator,
        right: &Self,
    ) -> Result<Self, VmErrorType> {
        match (self, right) {
            // Bool operations
            (
                ValuePrimitive::Bool(l),
                ValuePrimitive::Bool(r),
            ) => match operator {
                BinaryOperator::Equal => Ok(ValuePrimitive::Bool(l == r)),
                BinaryOperator::NotEqual => Ok(ValuePrimitive::Bool(l != r)),
                BinaryOperator::And => Ok(ValuePrimitive::Bool(*l && *r)),
                BinaryOperator::Or => Ok(ValuePrimitive::Bool(*l || *r)),
                BinaryOperator::Less => Ok(ValuePrimitive::Bool(!*l && *r)), // false < true
                BinaryOperator::LessEqual => {
                    Ok(ValuePrimitive::Bool(!*l || *r))
                } // false <= true, true <= true
                BinaryOperator::Greater => Ok(ValuePrimitive::Bool(*l && !*r)), // true > false
                BinaryOperator::GreaterEqual => {
                    Ok(ValuePrimitive::Bool(*l || !*r))
                } // true >= false, true >= true
                _ => Err(VmErrorType::InvalidOperation(format!(
                    "Cannot apply {operator:?} to Bool"
                ))),
            },
            // Integer operations
            (
                ValuePrimitive::Integer(l),
                ValuePrimitive::Integer(r),
            ) => match operator {
                BinaryOperator::Plus => Ok(ValuePrimitive::Integer(l + r)),
                BinaryOperator::Minus => Ok(ValuePrimitive::Integer(l - r)),
                BinaryOperator::Multiply => Ok(ValuePrimitive::Integer(l * r)),
                BinaryOperator::Divide => {
                    if *r == BigInt::from(0) {
                        Err(VmErrorType::DivisionByZero)
                    } else {
                        Ok(ValuePrimitive::Integer(l / r))
                    }
                }
                BinaryOperator::Modulo => {
                    if *r == BigInt::from(0) {
                        Err(VmErrorType::DivisionByZero)
                    } else {
                        Ok(ValuePrimitive::Integer(l % r))
                    }
                }
                BinaryOperator::Equal => Ok(ValuePrimitive::Bool(l == r)),
                BinaryOperator::NotEqual => Ok(ValuePrimitive::Bool(l != r)),
                BinaryOperator::Less => Ok(ValuePrimitive::Bool(l < r)),
                BinaryOperator::LessEqual => Ok(ValuePrimitive::Bool(l <= r)),
                BinaryOperator::Greater => Ok(ValuePrimitive::Bool(l > r)),
                BinaryOperator::GreaterEqual => {
                    Ok(ValuePrimitive::Bool(l >= r))
                }
                _ => Err(VmErrorType::InvalidOperation(format!(
                    "Cannot apply {operator:?} to integer"
                ))),
            },
            // Float operations
            (
                ValuePrimitive::Float(l),
                ValuePrimitive::Float(r),
            ) => match operator {
                BinaryOperator::Plus => Ok(ValuePrimitive::Float(l + r)),
                BinaryOperator::Minus => Ok(ValuePrimitive::Float(l - r)),
                BinaryOperator::Multiply => Ok(ValuePrimitive::Float(l * r)),
                BinaryOperator::Divide => {
                    if *r == BigRational::from(BigInt::from(0)) {
                        Err(VmErrorType::DivisionByZero)
                    } else {
                        Ok(ValuePrimitive::Float(l / r))
                    }
                }
                BinaryOperator::Modulo => {
                    if *r == BigRational::from(BigInt::from(0)) {
                        Err(VmErrorType::DivisionByZero)
                    } else {
                        Ok(ValuePrimitive::Float(l % r))
                    }
                }
                BinaryOperator::Equal => Ok(ValuePrimitive::Bool(l == r)),
                BinaryOperator::NotEqual => Ok(ValuePrimitive::Bool(l != r)),
                BinaryOperator::Less => Ok(ValuePrimitive::Bool(l < r)),
                BinaryOperator::LessEqual => Ok(ValuePrimitive::Bool(l <= r)),
                BinaryOperator::Greater => Ok(ValuePrimitive::Bool(l > r)),
                BinaryOperator::GreaterEqual => {
                    Ok(ValuePrimitive::Bool(l >= r))
                }
                _ => Err(VmErrorType::InvalidOperation(format!(
                    "Cannot apply {operator:?} to float"
                ))),
            },

            // Type mismatch for other combinations
            _ => Err(VmErrorType::InvalidOperation(
                "The operation is not implemented yet".into(),
            )),
        }
    }

    pub fn unary_op(
        &self,
        operator: &UnaryOperator,
    ) -> Result<Self, VmErrorType> {
        match (operator, self) {
            (UnaryOperator::Minus, ValuePrimitive::Integer(i)) => {
                Ok(ValuePrimitive::Integer(-i))
            }
            (UnaryOperator::Minus, ValuePrimitive::Float(f)) => {
                Ok(ValuePrimitive::Float(-f))
            }
            (UnaryOperator::Not, ValuePrimitive::Bool(b)) => {
                Ok(ValuePrimitive::Bool(!b))
            }
            _ => Err(VmErrorType::InvalidOperation(format!(
                "Cannot apply {operator:?} to {self:?}",
            ))),
        }
    }
}

impl Display for ValuePrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Reference(ptr, _) => write!(f, "&{:?}", ptr),
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
    value: BTreeMap<Identifier, VmParsedValue>,
}

impl StructValue {
    pub fn new(product: BTreeMap<Identifier, VmParsedValue>) -> Self {
        Self { value: product }
    }
}

impl IntoIterator for StructValue {
    type Item = (Identifier, VmParsedValue);
    type IntoIter = std::collections::btree_map::IntoIter<Identifier, VmParsedValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.value.into_iter()
    }
}

impl Deref for StructValue {
    fn deref(&self) -> &Self::Target {
        &self.value
    }

    type Target = BTreeMap<Identifier, VmParsedValue>;
}

impl ParsedValueType for StructValue {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        let mut type_fields = std::collections::BTreeMap::new();
        for (identifier, value) in self.into_iter() {
            let field_type = value.into_unified_type_definition()?;
            type_fields.insert(identifier, field_type);
        }
        Some(UnifiedTypeDefinition::TypeDef(
            CompTimeTypeGeneric::Product(type_fields),
        ))
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        for field_value in self.values() {
            if let VmParsedValue::TypeId(_) = field_value {
                return UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Type);
            }
        }
        let mut type_fields = std::collections::BTreeMap::new();
        for (field_name, field_value) in self.iter() {
            let field_type = field_value.get_type_of_value();
            type_fields.insert(field_name.clone(), field_type);
        }
        UnifiedTypeDefinition::TypeDef(CompTimeTypeGeneric::Product(type_fields))
    }

    fn to_vm_units(self) -> Vec<crate::vm::tree_walk::VmUnit> {
        let mut units = Vec::new();
        for (_field_name, field_value) in self {
            units.extend(field_value.to_vm_units());
        }
        units
    }
}

impl StructValue {
    pub fn create_unit() -> Self {
        Self::default()
    }
}

impl ParsedValueType for TypeId {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        Some(UnifiedTypeDefinition::TypeId(self))
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Type)
    }

    fn to_vm_units(self) -> Vec<crate::vm::tree_walk::VmUnit> {
        let value=VmUnit::HashValue(self.as_hash_value());
        vec![value]
    }
}

impl ParsedValueType for FuncId {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        None
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Type)
    }

    fn to_vm_units(self) -> Vec<crate::vm::tree_walk::VmUnit> {
        todo!()
        //vec![crate::vm::tree_walk::VmUnitType::Usize()]
    }
}
#[enum_dispatch(ParsedValueType)]
#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum VmParsedValue {
    ValuePrimitive,
    StructValue,
    TypeId,
    FuncId,
}

pub struct VmValueGeneralized{
    pub bits:Vec<VmUnit>,
    pub r#type:TypeId,
}

impl VmValueGeneralized {
    pub fn from_parsed_value(parsed: VmParsedValue, type_container: &mut TypeContainer) -> Self {
        parsed.to_vm_value_generalized(type_container)
    }
    
    pub fn try_to_primitive(&self, type_container: &TypeContainer) -> Option<ValuePrimitive> {
        if self.bits.len() != 1 {
            return None;
        }
        
        match &self.bits[0] {
            VmUnit::Bool(b) => Some(ValuePrimitive::Bool(*b)),
            VmUnit::Integer(i) => Some(ValuePrimitive::Integer(i.clone())),
            VmUnit::Float(f) => Some(ValuePrimitive::Float(f.clone())),
            VmUnit::Usize(ptr_index) => {
                let ptr = VmPtr::from_index(*ptr_index);
                Some(ValuePrimitive::Reference(ptr, self.r#type.clone()))
            }
            VmUnit::HashValue(_) => None,
        }
    }
    
    pub fn binary_op(
        left: Self,
        operator: &BinaryOperator,
        right: Self,
        type_container: &TypeContainer,
    ) -> Result<Self, VmErrorType> {
        let left_prim = left.try_to_primitive(type_container)
            .ok_or_else(|| VmErrorType::InvalidOperation("Cannot convert left operand to primitive".to_string()))?;
        let right_prim = right.try_to_primitive(type_container)
            .ok_or_else(|| VmErrorType::InvalidOperation("Cannot convert right operand to primitive".to_string()))?;
        
        let result_prim = left_prim.binary_op(operator, &right_prim)?;
        
        // Convert back to VmValueGeneralized
        let mut temp_container = type_container.clone();
        let result_parsed = VmParsedValue::ValuePrimitive(result_prim);
        Ok(result_parsed.to_vm_value_generalized(&mut temp_container))
    }
    
    pub fn unary_op(
        operand: Self,
        operator: &UnaryOperator,
        type_container: &TypeContainer,
    ) -> Result<Self, VmErrorType> {
        let operand_prim = operand.try_to_primitive(type_container)
            .ok_or_else(|| VmErrorType::InvalidOperation("Cannot convert operand to primitive".to_string()))?;
        
        let result_prim = operand_prim.unary_op(operator)?;
        
        // Convert back to VmValueGeneralized
        let mut temp_container = type_container.clone();
        let result_parsed = VmParsedValue::ValuePrimitive(result_prim);
        Ok(result_parsed.to_vm_value_generalized(&mut temp_container))
    }
}

impl Display for VmParsedValue {
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


impl VmParsedValue {
    pub fn create_unit() -> Self {
        Self::StructValue(StructValue::default())
    }

    pub fn is_null(&self) -> bool {
        matches!(self, VmParsedValue::StructValue(x) if x.is_empty())
    }
    
}


