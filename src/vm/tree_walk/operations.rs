use crate::ast::{BinaryOperator, UnaryOperator};
use crate::types::{CompTimeBuiltinType, CompTimeTypeGeneric, UnifiedTypeDefinition};
use crate::vm::tree_walk::vm_value::{VmParsedValue, ValuePrimitive};
use crate::vm::tree_walk::vm_error::VmErrorType;
use num_bigint::BigInt;
use num_rational::BigRational;

pub trait Evaluable: Sized {
    type Error;
    
    fn bool_value(b: bool) -> Self;
    fn int_value(i: BigInt) -> Self;
    fn float_value(f: BigRational) -> Self;
    
    fn binary_op(left: Self, op: &BinaryOperator, right: Self) -> Result<Self, Self::Error>;
    fn unary_op(op: &UnaryOperator, operand: Self) -> Result<Self, Self::Error>;
}

impl Evaluable for VmParsedValue {
    type Error = VmErrorType;
    
    fn bool_value(b: bool) -> Self {
        VmParsedValue::ValuePrimitive(ValuePrimitive::Bool(b))
    }
    
    fn int_value(i: BigInt) -> Self {
        VmParsedValue::ValuePrimitive(ValuePrimitive::Integer(i))
    }
    
    fn float_value(f: BigRational) -> Self {
        VmParsedValue::ValuePrimitive(ValuePrimitive::Float(f))
    }
    
    fn binary_op(left: Self, op: &BinaryOperator, right: Self) -> Result<Self, Self::Error> {
        match (left, right) {
            (VmParsedValue::ValuePrimitive(ValuePrimitive::Bool(l)), 
             VmParsedValue::ValuePrimitive(ValuePrimitive::Bool(r))) => {
                match op {
                    BinaryOperator::Equal => Ok(Self::bool_value(l == r)),
                    BinaryOperator::NotEqual => Ok(Self::bool_value(l != r)),
                    BinaryOperator::And => Ok(Self::bool_value(l && r)),
                    BinaryOperator::Or => Ok(Self::bool_value(l || r)),
                    BinaryOperator::Less => Ok(Self::bool_value(!l && r)),
                    BinaryOperator::LessEqual => Ok(Self::bool_value(!l || r)),
                    BinaryOperator::Greater => Ok(Self::bool_value(l && !r)),
                    BinaryOperator::GreaterEqual => Ok(Self::bool_value(l || !r)),
                    _ => Err(VmErrorType::InvalidOperation(format!("Cannot apply {:?} to Bool", op)))
                }
            }
            (VmParsedValue::ValuePrimitive(ValuePrimitive::Integer(l)), 
             VmParsedValue::ValuePrimitive(ValuePrimitive::Integer(r))) => {
                match op {
                    BinaryOperator::Plus => Ok(Self::int_value(l + r)),
                    BinaryOperator::Minus => Ok(Self::int_value(l - r)),
                    BinaryOperator::Multiply => Ok(Self::int_value(l * r)),
                    BinaryOperator::Divide => {
                        if r == BigInt::from(0) {
                            Err(VmErrorType::DivisionByZero)
                        } else {
                            Ok(Self::int_value(l / r))
                        }
                    }
                    BinaryOperator::Modulo => {
                        if r == BigInt::from(0) {
                            Err(VmErrorType::DivisionByZero)
                        } else {
                            Ok(Self::int_value(l % r))
                        }
                    }
                    BinaryOperator::Equal => Ok(Self::bool_value(l == r)),
                    BinaryOperator::NotEqual => Ok(Self::bool_value(l != r)),
                    BinaryOperator::Less => Ok(Self::bool_value(l < r)),
                    BinaryOperator::LessEqual => Ok(Self::bool_value(l <= r)),
                    BinaryOperator::Greater => Ok(Self::bool_value(l > r)),
                    BinaryOperator::GreaterEqual => Ok(Self::bool_value(l >= r)),
                    _ => Err(VmErrorType::InvalidOperation(format!("Cannot apply {:?} to integer", op)))
                }
            }
            (VmParsedValue::ValuePrimitive(ValuePrimitive::Float(l)), 
             VmParsedValue::ValuePrimitive(ValuePrimitive::Float(r))) => {
                match op {
                    BinaryOperator::Plus => Ok(Self::float_value(l + r)),
                    BinaryOperator::Minus => Ok(Self::float_value(l - r)),
                    BinaryOperator::Multiply => Ok(Self::float_value(l * r)),
                    BinaryOperator::Divide => {
                        if r == BigRational::from(BigInt::from(0)) {
                            Err(VmErrorType::DivisionByZero)
                        } else {
                            Ok(Self::float_value(l / r))
                        }
                    }
                    BinaryOperator::Modulo => {
                        if r == BigRational::from(BigInt::from(0)) {
                            Err(VmErrorType::DivisionByZero)
                        } else {
                            Ok(Self::float_value(l % r))
                        }
                    }
                    BinaryOperator::Equal => Ok(Self::bool_value(l == r)),
                    BinaryOperator::NotEqual => Ok(Self::bool_value(l != r)),
                    BinaryOperator::Less => Ok(Self::bool_value(l < r)),
                    BinaryOperator::LessEqual => Ok(Self::bool_value(l <= r)),
                    BinaryOperator::Greater => Ok(Self::bool_value(l > r)),
                    BinaryOperator::GreaterEqual => Ok(Self::bool_value(l >= r)),
                    _ => Err(VmErrorType::InvalidOperation(format!("Cannot apply {:?} to float", op)))
                }
            }
            (VmParsedValue::StructValue(_), _) | (_, VmParsedValue::StructValue(_)) => {
                Err(VmErrorType::InvalidOperation("Product operations not yet implemented".to_string()))
            }
            _ => Err(VmErrorType::InvalidOperation("The operation is not implemented yet".into()))
        }
    }
    
    fn unary_op(op: &UnaryOperator, operand: Self) -> Result<Self, Self::Error> {
        match (op, operand) {
            (UnaryOperator::Minus, VmParsedValue::ValuePrimitive(ValuePrimitive::Integer(i))) => {
                Ok(Self::int_value(-i))
            }
            (UnaryOperator::Minus, VmParsedValue::ValuePrimitive(ValuePrimitive::Float(f))) => {
                Ok(Self::float_value(-f))
            }
            (UnaryOperator::Not, VmParsedValue::ValuePrimitive(ValuePrimitive::Bool(b))) => {
                Ok(Self::bool_value(!b))
            }
            (_, VmParsedValue::StructValue(_)) => {
                Err(VmErrorType::InvalidOperation("Product operations not yet implemented".to_string()))
            }
            _ => Err(VmErrorType::InvalidOperation(format!("Cannot apply {:?} to operand", op)))
        }
    }
}

impl Evaluable for UnifiedTypeDefinition {
    type Error = String;
    
    fn bool_value(_: bool) -> Self {
        UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Bool)
    }
    
    fn int_value(_: BigInt) -> Self {
        UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Int)
    }
    
    fn float_value(_: BigRational) -> Self {
        UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Float)
    }
    
    fn binary_op(left: Self, op: &BinaryOperator, right: Self) -> Result<Self, Self::Error> {
        match (&left, &right) {
            (UnifiedTypeDefinition::TypeDef(CompTimeTypeGeneric::Builtin(CompTimeBuiltinType::Bool)),
             UnifiedTypeDefinition::TypeDef(CompTimeTypeGeneric::Builtin(CompTimeBuiltinType::Bool))) => {
                match op {
                    BinaryOperator::Equal | BinaryOperator::NotEqual | 
                    BinaryOperator::And | BinaryOperator::Or |
                    BinaryOperator::Less | BinaryOperator::LessEqual |
                    BinaryOperator::Greater | BinaryOperator::GreaterEqual => 
                        Ok(UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Bool)),
                    _ => Err(format!("Cannot apply {:?} to Bool", op))
                }
            }
            (UnifiedTypeDefinition::TypeDef(CompTimeTypeGeneric::Builtin(CompTimeBuiltinType::Int)),
             UnifiedTypeDefinition::TypeDef(CompTimeTypeGeneric::Builtin(CompTimeBuiltinType::Int))) => {
                match op {
                    BinaryOperator::Plus | BinaryOperator::Minus | 
                    BinaryOperator::Multiply | BinaryOperator::Divide | 
                    BinaryOperator::Modulo => 
                        Ok(UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Int)),
                    BinaryOperator::Equal | BinaryOperator::NotEqual |
                    BinaryOperator::Less | BinaryOperator::LessEqual |
                    BinaryOperator::Greater | BinaryOperator::GreaterEqual => 
                        Ok(UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Bool)),
                    _ => Err(format!("Cannot apply {:?} to Int", op))
                }
            }
            (UnifiedTypeDefinition::TypeDef(CompTimeTypeGeneric::Builtin(CompTimeBuiltinType::Float)),
             UnifiedTypeDefinition::TypeDef(CompTimeTypeGeneric::Builtin(CompTimeBuiltinType::Float))) => {
                match op {
                    BinaryOperator::Plus | BinaryOperator::Minus | 
                    BinaryOperator::Multiply | BinaryOperator::Divide | 
                    BinaryOperator::Modulo => 
                        Ok(UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Float)),
                    BinaryOperator::Equal | BinaryOperator::NotEqual |
                    BinaryOperator::Less | BinaryOperator::LessEqual |
                    BinaryOperator::Greater | BinaryOperator::GreaterEqual => 
                        Ok(UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Bool)),
                    _ => Err(format!("Cannot apply {:?} to Float", op))
                }
            }
            _ => Err("Type mismatch in binary operation".to_string())
        }
    }
    
    fn unary_op(op: &UnaryOperator, operand: Self) -> Result<Self, Self::Error> {
        match (op, &operand) {
            (UnaryOperator::Minus, UnifiedTypeDefinition::TypeDef(CompTimeTypeGeneric::Builtin(CompTimeBuiltinType::Int))) => {
                Ok(UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Int))
            }
            (UnaryOperator::Minus, UnifiedTypeDefinition::TypeDef(CompTimeTypeGeneric::Builtin(CompTimeBuiltinType::Float))) => {
                Ok(UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Float))
            }
            (UnaryOperator::Not, UnifiedTypeDefinition::TypeDef(CompTimeTypeGeneric::Builtin(CompTimeBuiltinType::Bool))) => {
                Ok(UnifiedTypeDefinition::builtin(CompTimeBuiltinType::Bool))
            }
            _ => Err(format!("Cannot apply {:?} to type", op))
        }
    }
}
