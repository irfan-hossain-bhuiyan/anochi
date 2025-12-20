use super::*;
use crate::ast::{CodeMetaData, Expression, Literal, UnaryOperator};

use crate::ast::expression::ExprNodeGeneric;
use crate::vm::tree_walk::vm_error::{VmError, VmErrorType};
use crate::vm::tree_walk::vm_value::{
    ParsedValueType, Reference, ValuePrimitive, VmValueSimplified,
};
use std::collections::BTreeMap;
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
        Expression::Literal(Literal::Identifier(id)) => vm
            .variables
            .get_reference_from_name(id)
            .ok_or_else(|| map_err(VmErrorType::UndefinedIdentifier(id.clone()))),
        Expression::Unary {
            operator: UnaryOperator::Deref,
            operand,
        } => {
            let value = evaluate_expr(vm, operand)?;
            if let VmValueSimplified::Reference(reference) = value.into_simplified_value(&vm.types)
            {
                Ok(reference)
            } else {
                Err(map_err(VmErrorType::TypeMismatch(
                    "Dereference operator (*) requires a reference value",
                )))
            }
        }
        _ => Err(map_err(VmErrorType::InvalidOperation(
            "Cannot get reference to a non-variable expression".to_string(),
        ))),
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
            Literal::Identifier(x) => vm
                .variables
                .get_value_from_name(x, &vm.types)
                .map_err(map_err),
            Literal::Bool(_) | Literal::Float(_) | Literal::Integer(_) => {
                let value = ValuePrimitive::from(literal.clone());
                Ok(value.into_vm_value_generalized(&mut vm.types))
            }
            Literal::String(_) => {
                unimplemented!()
            }
        },
        Expression::Binary {
            left,
            operator,
            right,
        } => {
            let left_val = vm.evaluate_expr(left)?;
            let right_val = vm.evaluate_expr(right)?;
            //vm_value::evaluate_binary_op(&left_val, &operator, &right_val).map_err(map_err)
            VmValueGeneralized::binary_op(left_val, operator, right_val, &mut vm.types)
                .map_err(map_err)
        }
        Expression::Unary { operator, operand } => match operator {
            UnaryOperator::Ref => {
                Ok(get_reference(vm, operand)?.into_vm_value_generalized(&mut vm.types))
            }
            UnaryOperator::Deref => {
                let operand_val = vm.evaluate_expr(operand)?.into_simplified_value(&vm.types);
                if let VmValueSimplified::Reference(reference) = operand_val {
                    Ok(vm
                        .variables
                        .get_value_from_reference(&reference, &vm.types)
                        .clone())
                } else {
                    Err(map_err(VmErrorType::TypeMismatch(
                        "Dereference operator (*) requires a reference value",
                    )))
                }
            }
            _ => {
                let operand_val = vm.evaluate_expr(operand)?;
                let operand_val = operand_val
                    .try_into_primitive(&vm.types)
                    .ok_or(VmErrorType::InvalidOperation(
                        "Operation isn't 
                        implemented for this type"
                            .to_owned(),
                    ))
                    .map_err(map_err)?;
                let operand_val = operand_val
                    .unary_op(operator)
                    .map_err(map_err)
                    .map(|x| x.into_vm_value_generalized(&mut vm.types));
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
        Expression::Sum { data: _ } => {
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
            let input_type = match vm.into_type(input) {
                Ok(x) => x,
                Err(x) => return Err(map_err(x)),
            };
            let output_type = match output {
                None => None,
                Some(x) => {
                    let output = vm.evaluate_expr(x)?;
                    let output_type = match vm.into_type(output) {
                        Ok(x) => x,
                        Err(x) => return Err(map_err(x)),
                    };
                    Some(output_type)
                }
            };
            let func = VmFunc::new_checked(input_type, output_type, *statements.clone(), &vm.types)
                .ok_or(VmErrorType::FuncInvalidInput)
                .map_err(map_err)?;
            let func_id = vm.add_function(func);
            Ok(VmValueSimplified::from(func_id).into_vm_value_generalized(&mut vm.types))
        }
        Expression::FnCall { caller, callee } => {
            let caller = vm.evaluate_expr(caller)?;
            let callee = vm.evaluate_expr(callee)?.into_simplified_value(&vm.types);
            let VmValueSimplified::FuncId(func_id) = caller.into_simplified_value(&vm.types) else {
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
