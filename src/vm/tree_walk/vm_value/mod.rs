use crate::ast::{BinaryOperator, Identifier, Literal, UnaryOperator};
use crate::types::{
    CompTimeBuiltinType, CompTimeTypeGeneric, TypeContainer, TypeDefinition, TypeId,
    UnifiedTypeDefinition,
};
use crate::vm::tree_walk::VmUnit;
use crate::vm::tree_walk::scope_stack::VmPtr;
use crate::vm::tree_walk::vm_error::VmErrorType;
use derive_more::Deref;
use enum_as_inner::EnumAsInner;
use enum_dispatch::enum_dispatch;
use num_bigint::BigInt;
use num_rational::BigRational;
use std::any::Any;
use std::collections::BTreeMap;
use std::fmt::Display;
use std::mem::take;

mod function;
pub use function::{FuncId, VmFunc};

#[enum_dispatch]
pub trait ParsedValueType:Display
where
    Self: Sized,
{
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition>;
    fn get_type_of_value(&self) -> UnifiedTypeDefinition;
    fn into_vm_units(self) -> Vec<VmUnit>;
    fn into_vm_value_generalized(self, type_container: &mut TypeContainer) -> VmValueGeneralized {
        let type_id = self.get_type_id_of_value(type_container);
        VmValueGeneralized {
            bits: self.into_vm_units(),
            r#type: type_id,
        }
    }
    fn into_type_definition(self, container: &mut TypeContainer) -> Option<TypeDefinition>
    where
        Self: Sized,
    {
        let type_id = container.store_unified_type(self.into_unified_type_definition()?);
        type_id.to_type_def(container)
    }

    fn into_type_id(self, type_container: &mut TypeContainer) -> Option<TypeId>
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

#[derive(Debug, Clone, PartialEq)]
pub struct Reference {
    pub ptr: VmPtr,
    pub type_id: TypeId,
}

impl Reference {
    pub fn new(ptr: VmPtr, type_id: TypeId) -> Self {
        Self { ptr, type_id }
    }
}
impl Display for Reference{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{type_id:?}* {ptr:?}",type_id=self.type_id,ptr=self.ptr)
    }
}
impl ParsedValueType for Reference {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        todo!()
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        todo!()
    }

    fn into_vm_units(self) -> Vec<VmUnit> {
        todo!()
    }
}

/// Primitive values that can be stored in the VM
#[derive(Debug, Clone, PartialEq)]
pub enum ValuePrimitive {
    Bool(bool),
    Integer(BigInt),
    Float(BigRational),
    Index(usize), // This one is for indexing in an array.
}

impl ParsedValueType for ValuePrimitive {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        todo!()
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        match self {
            ValuePrimitive::Bool(_) => UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Bool),
            ValuePrimitive::Integer(_) => UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Int),
            ValuePrimitive::Float(_) => UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Float),
            ValuePrimitive::Index(_) => UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Usize),
        }
    }

    fn into_vm_units(self) -> Vec<crate::vm::tree_walk::VmUnit> {
        match self {
            ValuePrimitive::Bool(b) => vec![VmUnit::Bool(b)],
            ValuePrimitive::Integer(i) => vec![VmUnit::Integer(i)],
            ValuePrimitive::Float(f) => vec![VmUnit::Float(f)],
            ValuePrimitive::Index(index) => vec![VmUnit::Usize(index)],
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

    pub fn binary_op(&self, operator: &BinaryOperator, right: &Self) -> Result<Self, VmErrorType> {
        match (self, right) {
            // Bool operations
            (ValuePrimitive::Bool(l), ValuePrimitive::Bool(r)) => match operator {
                BinaryOperator::Equal => Ok(ValuePrimitive::Bool(l == r)),
                BinaryOperator::NotEqual => Ok(ValuePrimitive::Bool(l != r)),
                BinaryOperator::And => Ok(ValuePrimitive::Bool(*l && *r)),
                BinaryOperator::Or => Ok(ValuePrimitive::Bool(*l || *r)),
                BinaryOperator::Less => Ok(ValuePrimitive::Bool(!*l && *r)), // false < true
                BinaryOperator::LessEqual => Ok(ValuePrimitive::Bool(!*l || *r)), // false <= true, true <= true
                BinaryOperator::Greater => Ok(ValuePrimitive::Bool(*l && !*r)),   // true > false
                BinaryOperator::GreaterEqual => Ok(ValuePrimitive::Bool(*l || !*r)), // true >= false, true >= true
                _ => Err(VmErrorType::InvalidOperation(format!(
                    "Cannot apply {operator:?} to Bool"
                ))),
            },
            // Integer operations
            (ValuePrimitive::Integer(l), ValuePrimitive::Integer(r)) => match operator {
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
                BinaryOperator::GreaterEqual => Ok(ValuePrimitive::Bool(l >= r)),
                _ => Err(VmErrorType::InvalidOperation(format!(
                    "Cannot apply {operator:?} to integer"
                ))),
            },
            // Float operations
            (ValuePrimitive::Float(l), ValuePrimitive::Float(r)) => match operator {
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
                BinaryOperator::GreaterEqual => Ok(ValuePrimitive::Bool(l >= r)),
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

    pub fn unary_op(&self, operator: &UnaryOperator) -> Result<Self, VmErrorType> {
        match (operator, self) {
            (UnaryOperator::Minus, ValuePrimitive::Integer(i)) => Ok(ValuePrimitive::Integer(-i)),
            (UnaryOperator::Minus, ValuePrimitive::Float(f)) => Ok(ValuePrimitive::Float(-f)),
            (UnaryOperator::Not, ValuePrimitive::Bool(b)) => Ok(ValuePrimitive::Bool(!b)),
            _ => Err(VmErrorType::InvalidOperation(format!(
                "Cannot apply {operator:?} to {self:?}",
            ))),
        }
    }
}

impl Display for ValuePrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
            Self::Index(index) => write!(f, "{index}"),
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
#[derive(Debug, Default, Clone, PartialEq, Deref)]
pub struct StructValue {
    #[deref]
    pub value: BTreeMap<Identifier, VmValueGeneralized>,
}

impl StructValue {
    pub fn new(product: BTreeMap<Identifier, VmValueGeneralized>) -> Self {
        Self { value: product }
    }
}
impl ParsedValueType for StructValue {
    fn into_vm_units(self) -> Vec<crate::vm::tree_walk::VmUnit> {
        let mut units = Vec::new();
        for (_field_name,mut field_value) in self.value {
            units.append(&mut field_value.bits);
        }
        units
    }

    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        todo!()
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        todo!()
    }
}

impl Display for StructValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut first = true;
        for (key, value) in self.value.iter() {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
            first = false;
        }
        write!(f, "}}")
    }
}

impl StructValue {
    pub fn create_unit() -> Self {
        Self::default()
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeId({:?})", self.as_hash_value())
    }
}

impl ParsedValueType for TypeId {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        Some(UnifiedTypeDefinition::TypeId(self))
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Type)
    }

    fn into_vm_units(self) -> Vec<crate::vm::tree_walk::VmUnit> {
        let value = VmUnit::HashValue(self.as_hash_value());
        vec![value]
    }
}

impl Display for FuncId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FuncId({:?})", self)
    }
}

impl ParsedValueType for FuncId {
    fn into_unified_type_definition(self) -> Option<UnifiedTypeDefinition> {
        None
    }

    fn get_type_of_value(&self) -> UnifiedTypeDefinition {
        UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Type)
    }

    fn into_vm_units(self) -> Vec<crate::vm::tree_walk::VmUnit> {
        todo!()
        //vec![crate::vm::tree_walk::VmUnitType::Usize()]
    }
}
#[enum_dispatch(ParsedValueType)]
#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum VmSimplifiedValue {
    ValuePrimitive,
    StructValue,
    Reference,
    TypeId,
    FuncId,
}

impl Display for VmSimplifiedValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmSimplifiedValue::ValuePrimitive(v) => write!(f, "{}", v),
            VmSimplifiedValue::StructValue(v) => write!(f, "{}", v),
            VmSimplifiedValue::Reference(v) => write!(f, "{}", v),
            VmSimplifiedValue::TypeId(v) => write!(f, "{}", v),
            VmSimplifiedValue::FuncId(v) => write!(f, "{}", v),
        }
    }
}

impl VmSimplifiedValue {
    pub fn create_unit() -> Self {
        Self::StructValue(StructValue::default())
    }

    pub fn is_null(&self) -> bool {
        matches!(self, VmSimplifiedValue::StructValue(x) if x.is_empty())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VmValueGeneralized {
    pub bits: Vec<VmUnit>,
    pub r#type: TypeId,
}

impl VmValueGeneralized {
    pub fn new(bits: Vec<VmUnit>, r#type: TypeId) -> Self {
        Self { bits, r#type }
    }

    pub fn from_simplified_value(
        parsed: VmSimplifiedValue,
        type_container: &mut TypeContainer,
    ) -> Self {
        parsed.into_vm_value_generalized(type_container)
    }

    pub fn into_simplified_value(self, type_container: &TypeContainer) -> VmSimplifiedValue {
        let type_def = type_container.get_type(&self.r#type).unwrap();

        match &type_def.0 {
            CompTimeTypeGeneric::Builtin(builtin_type) => match builtin_type {
                CompTimeBuiltinType::Bool => {
                    let b = self.bits[0].as_bool().unwrap();
                    VmSimplifiedValue::ValuePrimitive(ValuePrimitive::Bool(*b))
                }
                CompTimeBuiltinType::Int => {
                    let i = self.bits[0].as_integer().unwrap();
                    VmSimplifiedValue::ValuePrimitive(ValuePrimitive::Integer(i.clone()))
                }
                CompTimeBuiltinType::Float => {
                    let f = self.bits[0].as_float().unwrap();
                    VmSimplifiedValue::ValuePrimitive(ValuePrimitive::Float(f.clone()))
                }
                CompTimeBuiltinType::Type => {
                    let hash = self.bits[0].as_hash_value().unwrap();
                    let type_id = unsafe { TypeId::new(*hash) };
                    VmSimplifiedValue::TypeId(type_id)
                }
                CompTimeBuiltinType::Usize => {
                    let ptr_index = self.bits[0].as_usize().unwrap();
                    VmSimplifiedValue::ValuePrimitive(ValuePrimitive::Index(*ptr_index))
                }
            },
            CompTimeTypeGeneric::Reference(inner_type) => {
                let index = self.bits[0].as_usize().unwrap();
                let ptr_index = unsafe { VmPtr::new(*index) };
                let r#ref = Reference::new(ptr_index, **inner_type);
                VmSimplifiedValue::from(r#ref)
            }
            CompTimeTypeGeneric::Product(fields) => {
                let mut total_bytes=self.bits;
                let mut ans=BTreeMap::new();
                for (param,r#type) in fields{
                    let meta_data=type_container.get_metadata(r#type).unwrap();
                    let rest_bits=total_bytes.split_off(meta_data.size);
                    if total_bytes.len()!=meta_data.size {
                        panic!("The bits doesn't match with type size");
                    }
                    let value=VmValueGeneralized::new(take(&mut total_bytes),*r#type);
                    ans.insert(param.clone(), value);
                    total_bytes=rest_bits;
                }
                StructValue::new(ans).into()
            }
            CompTimeTypeGeneric::Sum(_variants) => {
                unimplemented!("Sum types not yet implemented")
            }
        }
    }

    pub fn try_into_primitive(self, type_container: &TypeContainer) -> Option<ValuePrimitive> {
        self.into_simplified_value(type_container)
            .as_value_primitive()
            .cloned()
    }

    pub fn binary_op(
        left: Self,
        operator: &BinaryOperator,
        right: Self,
        type_container: &mut TypeContainer,
    ) -> Result<Self, VmErrorType> {
        let left_prim = left.try_into_primitive(type_container).ok_or_else(|| {
            VmErrorType::InvalidOperation("Cannot convert left operand to primitive".to_string())
        })?;
        let right_prim = right.try_into_primitive(type_container).ok_or_else(|| {
            VmErrorType::InvalidOperation("Cannot convert right operand to primitive".to_string())
        })?;

        let result_prim = left_prim.binary_op(operator, &right_prim)?;

        Ok(result_prim.into_vm_value_generalized(type_container))
    }

    pub fn unary_op(
        operand: Self,
        operator: &UnaryOperator,
        type_container: &mut TypeContainer,
    ) -> Result<Self, VmErrorType> {
        let operand_prim = operand.try_into_primitive(type_container).ok_or_else(|| {
            VmErrorType::InvalidOperation("Cannot convert operand to primitive".to_string())
        })?;

        let result_prim = operand_prim.unary_op(operator)?;

        // Convert back to VmValueGeneralized
        Ok(result_prim.into_vm_value_generalized(type_container))
    }

    pub fn create_unit(type_container: &mut TypeContainer) -> VmValueGeneralized {
        let type_id: TypeId = type_container.get_unit_type();
        VmValueGeneralized {
            bits: Vec::new(),
            r#type: type_id,
        }
    }

    pub(crate) fn of_type(&self, expected_type_id: TypeId, types: &mut TypeContainer) -> bool {
        self.r#type.can_cast_to(&expected_type_id, types)
    }
}

impl Display for VmValueGeneralized {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "VmValue(type={:?}, bits={:?})", self.r#type.as_hash_value(), self.bits)
    }
}
