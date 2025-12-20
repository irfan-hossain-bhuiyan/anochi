use super::*;
use crate::ast::{Expression, CodeMetaData, Literal, UnaryOperator};

use crate::vm::tree_walk::vm_value::{ParsedValueType, Reference, ValuePrimitive, VmSimplifiedValue};
use crate::ast::expression::ExprNodeGeneric;
use crate::vm::tree_walk::vm_error::{VmError, VmErrorType};
use crate::prelude::IndexPtr;
use crate::types::{UnifiedTypeDefinition};
use std::collections::{BTreeMap, BTreeSet};
/// get_reference is used to get reference from expression,this is used,
/// this is used on assignment to get the address,
pub(super) fn get_reference<Backend: VmBackend>(
    vm: &mut Vm<Backend>,
    expression_node: &ExpressionNode,
) -> Result<Reference, VmError> {
    let node_data = expression_node.data().get_position().clone();
    let map_err = |e| VmError::new(e, node_data.clone());
    let expression = &expression_node.exp;
    
    match expression {
        Expression::Literal(Literal::Identifier(id)) => {
            vm.variables.get_reference_from_name(id)
                .ok_or_else(|| map_err(VmErrorType::UndefinedIdentifier(id.clone())))
        }
        Expression::Unary { operator: UnaryOperator::Deref, operand } => {
            let value = evaluate_expr(vm, operand)?;
            if let VmSimplifiedValue::Reference(reference) = value.into_simplified_value(&vm.types) {
                Ok(reference)
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

pub(super) fn type_evaluation<Backend: VmBackend>(
    vm: &mut Vm<Backend>,
    expression_node: &ExpressionNode
) -> Result<TypeId, VmError> {
    let node_data = expression_node.data().get_position().clone();
    let map_err = |e| VmError::new(e, node_data.clone());
    let expression = &expression_node.exp;
    
    match expression {
        Expression::Literal(literal) => match literal {
            Literal::Identifier(name) => {
                let var_state = vm.variables.get_variable_data(name)
                    .ok_or_else(|| map_err(VmErrorType::UndefinedIdentifier(name.clone())))?;
                Ok(var_state.type_id)
            }
            Literal::Bool(_) => {
                let type_def = UnifiedTypeDefinition::builtin(crate::types::CompTimeBuiltinType::Bool);
                Ok(vm.types.store_unified_type(type_def))
            }
            Literal::Integer(_) => {
                let type_def = UnifiedTypeDefinition::builtin(crate::types::CompTimeBuiltinType::Int);
                Ok(vm.types.store_unified_type(type_def))
            }
            Literal::Float(_) => {
                let type_def = UnifiedTypeDefinition::builtin(crate::types::CompTimeBuiltinType::Float);
                Ok(vm.types.store_unified_type(type_def))
            }
            Literal::String(_) => {
                unimplemented!()
            }
        }
        Expression::Binary { left, operator: _operator, right } => {
            let left_type = type_evaluation(vm, left)?;
            let right_type = type_evaluation(vm, right)?;
            
            if left_type != right_type {
                return Err(map_err(VmErrorType::TypeMismatch(
                    "Binary operation requires matching types",
                )));
            }
            
            Ok(left_type)
        }
        Expression::Unary { operator, operand } => match operator {
            UnaryOperator::Ref => {
                let operand_type = type_evaluation(vm, operand)?;
                let type_def=UnifiedTypeDefinition::reference(UnifiedTypeDefinition::TypeId(operand_type));
                Ok(vm.types.store_unified_type(type_def))
            }
            UnaryOperator::Deref => {
                let _operand_type = type_evaluation(vm, operand)?;
                unimplemented!()
            }
            _ => {
                let operand_type = type_evaluation(vm, operand)?;
                Ok(operand_type)
            },
        }
        Expression::Grouping { expression } => type_evaluation(vm, expression),
        Expression::Product { data } => {
            let mut product_types = BTreeMap::new();
            for (key, value_expr) in data.iter() {
                let value_type = type_evaluation(vm, value_expr)?;
                product_types.insert(key.clone(), UnifiedTypeDefinition::TypeId(value_type));
            }
            
            let unified = UnifiedTypeDefinition::product(product_types);
            let type_id = vm.types.store_unified_type(unified);
            Ok(type_id)
        }
        Expression::Sum { data } => {
            let mut type_set = BTreeSet::new();
            for expr in data.iter() {
                let type_id = type_evaluation(vm, expr)?;
                type_set.insert(UnifiedTypeDefinition::TypeId(type_id));
            }
            
            let unified = UnifiedTypeDefinition::sum(type_set);
            let type_id = vm.types.store_unified_type(unified);
            Ok(type_id)
        }
        Expression::MemberAccess { .. } => todo!(),
        Expression::Function { .. } => todo!(),
        Expression::FnCall { .. } => todo!(),
    }
}

pub(super) fn evaluate_expr<Backend: VmBackend>(
    vm: &mut Vm<Backend>,
    expression_node: &ExprNodeGeneric<CodeMetaData>,
) -> VmExprResult {
    let node_data = expression_node.data().get_position().clone();
    let map_err = |e| VmError::new(e, node_data.clone());
    let expression = &expression_node.exp;
    match expression {
        Expression::Literal(literal) => match literal {
            Literal::Identifier(x) => vm.variables.get_value_from_name(&x,&vm.types).map_err(map_err),
            Literal::Bool(_) | Literal::Float(_) | Literal::Integer(_) => {
                let value=ValuePrimitive::from(literal.clone());
                Ok(value.into_vm_value_generalized(&mut vm.types))
            },
            Literal::String(_) => {unimplemented!()}
        },
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left_val = vm.evaluate_expr(left)?;
            let right_val = vm.evaluate_expr(right)?;
            //vm_value::evaluate_binary_op(&left_val, &operator, &right_val).map_err(map_err)
            VmValueGeneralized::binary_op(left_val,operator,right_val,&mut vm.types).map_err(map_err)
        }
        Expression::Unary { operator, operand } => match operator {
            UnaryOperator::Ref => {
                Ok(get_reference(vm, operand)?.into_vm_value_generalized(&mut vm.types))
            }
            UnaryOperator::Deref => {
                let operand_val = vm.evaluate_expr(operand)?.into_simplified_value(&vm.types);
                if let VmSimplifiedValue::Reference(reference) = operand_val {
                    Ok(vm.variables.get_value_from_reference(&reference,&vm.types).clone())
                } else {
                    Err(map_err(VmErrorType::TypeMismatch(
                        "Dereference operator (*) requires a reference value",
                    )))
                }
            }
            _ => {
                let operand_val = vm.evaluate_expr(operand)?;
                let operand_val= operand_val.try_into_primitive(&vm.types).ok_or(VmErrorType::InvalidOperation("Operation isn't 
                        implemented for this type".to_owned())).map_err(map_err)?;
                let operand_val=operand_val.unary_op(operator).map_err(map_err).map(|x|x.into_vm_value_generalized(&mut vm.types));
                operand_val
            }
        },
        Expression::Grouping { expression } => vm.evaluate_expr(expression),
        Expression::Product { data } => {
            let mut product = BTreeMap::new();
            for (key, value) in data.iter() {
                product.insert(key.clone(), vm.evaluate_expr(value)?);
            }
            Ok(StructValue::new(product).into_vm_value_generalized(&mut vm.types))
        }
        Expression::Sum { data } => {
            //let mut type_set = BTreeSet::new();
            todo!()
        }
        Expression::MemberAccess { .. } => todo!(),
        Expression::Function {
            input,
            output,
            statements,
        } => {
            let input = vm.evaluate_expr(input)?;
            let input_type=match vm.into_type(input) {
                Ok(x)=>x,
                Err(x)=>return Err(map_err(x)),
            };
            let output_type = match output {
                None => None,
                Some(x) => {
                    let output = vm.evaluate_expr(x)?;
                    let output_type =match vm.into_type(output){
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
            todo!()
        }
        Expression::FnCall { caller, callee } => {
            let caller = vm.evaluate_expr(caller)?;
            let callee = vm.evaluate_expr(callee)?;
            todo!()
            //let VmSimplifiedValue::FuncId(func_id) = caller else {
            //    return Err(map_err(VmErrorType::CallingNonFunc));
            //};
            //let param_type = vm.get_func(func_id).get_param();
            //if !callee.of_type(param_type, &mut vm.types) {
            //    return Err(map_err(VmErrorType::FuncInvalidInput));
            //}
            //// type check already gaurentee that callee is of struct type
            //vm.execute_function(func_id, callee)
        }
    }
}

