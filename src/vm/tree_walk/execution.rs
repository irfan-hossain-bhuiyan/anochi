use super::*;
use crate::ast::{Statement};
use crate::vm::tree_walk::vm_value::{ValuePrimitive, VmVal, VmValue};
use crate::vm::tree_walk::vm_error::{VmError, VmErrorType};
use crate::vm::tree_walk::evaluation::{evaluate_expr, get_reference};

pub(super) fn execute_statement<Backend: VmBackend>(
    vm: &mut Vm<Backend>,
    stat_node: &StatementNode,
) -> StatementResult {
    let node_data = stat_node.data().get_position().clone();
    let map_err = |e| VmError::new(e, node_data.clone());
    let stmt = &stat_node.stat;
    match stmt {
        Statement::Assignment {
            target,
            r#type,
            value,
        } => {
            let value = evaluate_expr(vm,value)?;
            if let Some(type_expr) = r#type {
                let type_value = evaluate_expr(vm,type_expr)?;
                let expected_type_id = VmVal::get_type_id(type_value, &mut vm.types)
                    .ok_or(VmErrorType::InvalidTypeDefination).map_err(map_err)?;
                if !value.of_type(expected_type_id, &mut vm.types) {
                    return Err(map_err(VmErrorType::TypeMismatch("")));
                }
                // Use insert_variable_check for type verification
            }
            vm.insert_variable(target.clone(), value).map_err(map_err)?;
            // Use insert_variable for automatic type inference
            Ok(StatementEvent::None)
        }
        Statement::Statements(block) => {
            for x in block.statements.iter() {
                match vm.execute_statement(x)? {
                    StatementEvent::None => continue,
                    event => return Ok(event), // Propagate break, continue, return
                }
            }
            Ok(StatementEvent::None)
        }
        Statement::MutableAssignment { target, value } => {
            let ptr = get_reference(vm, target)?;
            let evaluated_value = evaluate_expr(vm,value)?;
            vm.variables.set_value_from_index(
                ptr,
                evaluated_value,
                &mut vm.types,
            ).map_err(map_err)?;
            Ok(StatementEvent::None)
        }
        Statement::StatementBlock(stmtblock) => vm.run_block(&stmtblock),
        Statement::If { condition, on_true } => {
            let VmValue::ValuePrimitive(ValuePrimitive::Bool(x)) =
                evaluate_expr(vm,condition)?
            else {
                return Err(map_err(VmErrorType::TypeMismatch(
                    "The expression in if should be boolean",
                )));
            };
            if x {
                vm.execute_statement(on_true)
            } else {
                Ok(StatementEvent::None)
            }
        }
        Statement::IfElse {
            condition,
            on_true,
            on_false,
        } => {
            let VmValue::ValuePrimitive(ValuePrimitive::Bool(x)) =
                evaluate_expr(vm,condition)?
            else {
                return Err(map_err(VmErrorType::TypeMismatch(
                    "The expression on ifelse should be bool",
                )));
            };
            if x {
                vm.execute_statement(on_true)
            } else {
                vm.execute_statement(on_false)
            }
        }
        Statement::Debug { expr_vec } => {
            if expr_vec.is_empty() {
                vm.print_stack();
            } else {
                for expr in expr_vec.iter() {
                    let expr = evaluate_expr(vm,expr)?;
                    vm.backend.debug_print(&expr.to_string()).unwrap();
                }
            }
            Ok(StatementEvent::None)
        }
        Statement::Continue => Ok(StatementEvent::Continue),
        Statement::Break => Ok(StatementEvent::Break),
        Statement::Loop { statements } => {
            loop {
                match vm.run_block(statements)? {
                    StatementEvent::None => {}
                    StatementEvent::Break => {
                        break;
                    }
                    StatementEvent::Continue => {
                        continue;
                    }
                    StatementEvent::Return(x) => return Ok(StatementEvent::Return(x)),
                }
            }
            Ok(StatementEvent::None)
        }
        Statement::Return(x)=>{
            let return_value=match x {
                Some(value)=>evaluate_expr(vm,value)?,
                None=>VmValue::create_unit(),
            };
            Ok(StatementEvent::Return(return_value))
        }
        Statement::Comptime { statements } => todo!(),
    }
}

