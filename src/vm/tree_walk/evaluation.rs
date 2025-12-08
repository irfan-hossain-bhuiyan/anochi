use super::*;
use crate::ast::{Expression, Literal, UnaryOperator};
use crate::vm::tree_walk::vm_value::{self, ValuePrimitive, VmVal};
use crate::vm::tree_walk::vm_error::{VmError, VmErrorType};
use crate::vm::tree_walk::scope_stack::VariableEntry;
use crate::prelude::IndexPtr;
use crate::types::UnifiedTypeDefinition;
use std::collections::{HashMap, BTreeSet};

pub(super) fn get_reference<Backend: VmBackend, T: Clone + HasPosition>(
    vm: &mut Vm<Backend>,
    expression_node: &ExpNode<T>,
) -> Result<IndexPtr<VariableEntry>, VmError> {
    let node_data = expression_node.data().get_position().clone();
    let map_err = |e| VmError::new(e, node_data.clone());
    let expression = &expression_node.exp;
    
    match expression {
        Expression::Literal(Literal::Identifier(id)) => {
            vm.variables.get_index_from_name(id)
                .ok_or_else(|| map_err(VmErrorType::UndefinedIdentifier(id.clone())))
        }
        Expression::Unary { operator: UnaryOperator::Deref, operand } => {
            let value = evaluate_expr(vm, operand)?;
            if let VmValue::ValuePrimitive(ValuePrimitive::Reference(ptr)) = value {
                Ok(ptr)
            } else {
                Err(map_err(VmErrorType::TypeMismatch(
                    "Dereference operator (*) requires a reference value",
                )))
            }
        }
        _ => {
            Err(map_err(VmErrorType::InvalidOperation(
                "Cannot get reference to a non-variable expression".to_string(),
            )))
        }
    }
}

pub(super) fn evaluate_expr<Backend: VmBackend, T: Clone + HasPosition>(
    vm: &mut Vm<Backend>,
    expression_node: &ExpNode<T>,
) -> VmExprResult {
    let node_data = expression_node.data().get_position().clone();
    let map_err = |e| VmError::new(e, node_data.clone());
    let expression = &expression_node.exp;
    match expression {
        Expression::Literal(literal) => match literal {
            Literal::Identifier(x) => vm.variables.get_value_or_err(&x).map_err(map_err),
            Literal::Bool(_) | Literal::Float(_) | Literal::Integer(_) => Ok(
                VmValue::ValuePrimitive(ValuePrimitive::from(literal.clone())),
            ),
            Literal::String(_) => {
                Err(map_err(VmErrorType::Unsupported(
                    "String literals not yet supported as arrays".to_string(),
                )))
            }
        },
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left_val = vm.evaluate_expr(left)?;
            let right_val = vm.evaluate_expr(right)?;
            vm_value::evaluate_binary_op(&left_val, &operator, &right_val).map_err(map_err)
        }
        Expression::Unary { operator, operand } => match operator {
            UnaryOperator::Ref => {
                let ptr = get_reference(vm, operand)?;
                Ok(VmValue::ValuePrimitive(ValuePrimitive::Reference(ptr)))
            }
            UnaryOperator::Deref => {
                let operand_val = vm.evaluate_expr(operand)?;
                if let VmValue::ValuePrimitive(ValuePrimitive::Reference(ptr)) = operand_val {
                    Ok(vm.variables.get_value_from_index(ptr).clone())
                } else {
                    Err(map_err(VmErrorType::TypeMismatch(
                        "Dereference operator (*) requires a reference value",
                    )))
                }
            }
            _ => {
                let operand_val = vm.evaluate_expr(operand)?;
                vm_value::evaluate_unary_op(&operator, &operand_val).map_err(map_err)
            }
        },
        Expression::Grouping { expression } => vm.evaluate_expr(expression),
        Expression::Product { data } => {
            let mut product = HashMap::new();
            for (key, value) in data.iter() {
                product.insert(key.clone(), vm.evaluate_expr(value)?);
            }
            Ok(VmValue::StructValue(StructValue::new(product)))
        }
        Expression::Sum { data } => {
            let mut type_set = BTreeSet::new();
            for expr in data.iter() {
                match vm.evaluate_expr(expr)? {
                    VmValue::TypeId(type_id) => {
                        type_set.insert(type_id);
                    }
                    _ => {
                        return Err(map_err(VmErrorType::InvalidOperation(
                            "Sum types can only contain type values".to_string(),
                        )));
                    }
                }
            }

            // For sum types, we need to convert TypeIds back to TypeDefinitions to create the sum type
            let mut variants = BTreeSet::new();
            for type_id in type_set {
                if vm.types.has_type(&type_id) {
                    // Store the TypeId directly as UnifiedTypeDefinition::TypeId
                    variants.insert(UnifiedTypeDefinition::TypeId(type_id));
                } else {
                    return Err(map_err(VmErrorType::InvalidOperation(
                        "Type not found in container".to_string(),
                    )));
                }
            }

            let unified = crate::types::UnifiedTypeDefinition::sum(variants);
            let type_id = vm.types.store_unified_type(unified);
            Ok(VmValue::TypeId(type_id))
        }
        Expression::MemberAccess { object, member } => todo!(),
        Expression::Function {
            input,
            output,
            statements,
        } => {
            let input = vm.evaluate_expr(input)?;
            let input_type=match vm.to_type(input) {
                Ok(x)=>x,
                Err(x)=>return Err(map_err(x)),
            };
            let output_type = match output {
                None => None,
                Some(x) => {
                    let output = vm.evaluate_expr(x)?;
                    let output_type =match vm.to_type(output){
                        Ok(x)=>x,
                        Err(x)=>return Err(map_err(x)),
                    };
                    Some(output_type)
                }
            };
            let func =
                VmFunc::new_checked(input_type, output_type, *statements.clone(), &vm.types)
                    .ok_or(VmErrorType::FuncInvalidInput).map_err(map_err)?;
            let func_id = vm.add_function(func);
            Ok(func_id.into())
        }
        Expression::FnCall { caller, callee } => {
            let caller = vm.evaluate_expr(caller)?;
            let callee = vm.evaluate_expr(callee)?;
            let VmValue::FuncId(func_id) = caller else {
                return Err(map_err(VmErrorType::CallingNonFunc));
            };
            let param_type = vm.get_func(func_id).get_param();
            if !callee.of_type(param_type, &mut vm.types) {
                return Err(map_err(VmErrorType::FuncInvalidInput));
            }
            // type check already gaurentee that callee is of struct type
            vm.execute_function(func_id, callee)
        }
    }
}

