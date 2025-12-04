use super::*;
use crate::ast::{Expression, Literal};
use crate::vm::tree_walk::vm_value::{self, ValuePrimitive, VmFunc};
use crate::vm::tree_walk::vm_error::{VmError, VmErrorType};
use crate::types::UnifiedTypeDefinition;
use std::collections::{HashMap, BTreeSet};

pub(super) fn evaluate_expr<Backend: VmBackend, T: Clone + HasPosition>(
    vm: &mut Vm<Backend>,
    expression_node: &ExpNode<T>,
) -> VmExprResult {
    let node_data = expression_node.data().get_position().clone();
    let map_err = |e| VmError::new(e, node_data.clone());
    let expression = &expression_node.exp;
    match expression {
        Expression::Literal(literal) => match literal {
            Literal::Identifier(x) => vm.variables.get_variable_or_err(&x).map_err(map_err),
            Literal::Bool(_) | Literal::Float(_) | Literal::Integer(_) => Ok(
                VmValue::ValuePrimitive(ValuePrimitive::from(literal.clone())),
            ),
            Literal::String(_) => {
                // TODO: Handle strings as arrays when array implementation is ready
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
        Expression::Unary { operator, operand } => {
            let operand_val = vm.evaluate_expr(operand)?;
            vm_value::evaluate_unary_op(&operator, &operand_val).map_err(map_err)
        }
        Expression::Grouping { expression } => vm.evaluate_expr(expression),
        Expression::Product { data } => {
            let mut product = HashMap::new();
            for (key, value) in data.iter() {
                product.insert(key.clone(), vm.evaluate_expr(value)?);
            }
            Ok(VmValue::Product(StructValue::new(product)))
        }
        Expression::Sum { data } => {
            let mut type_set = BTreeSet::new();
            for expr in data.iter() {
                match vm.evaluate_expr(expr)? {
                    VmValue::Type(type_id) => {
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
            Ok(VmValue::Type(type_id))
        }
        Expression::MemberAccess { object, member } => todo!(),
        Expression::Function {
            input,
            output,
            statements,
        } => {
            let input = vm.evaluate_expr(&input)?;
            let input_type=match vm.to_type(input) {
                Ok(x)=>x,
                Err(x)=>return Err(map_err(x)),
            };
            let output_type = match output {
                None => None,
                Some(x) => {
                    let output = vm.evaluate_expr(&x)?;
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
            Ok(VmValue::from_func(func_id))
        }
        Expression::FnCall { caller, callee } => {
            let caller = vm.evaluate_expr(&caller)?;
            let callee = vm.evaluate_expr(&callee)?;
            let VmValue::Func(func_id) = caller else {
                return Err(VmErrorType::CallingNonFunc).map_err(map_err);
            };
            todo!()
            //vm.execute_function(&func_id, callee)
        }
    }
}

