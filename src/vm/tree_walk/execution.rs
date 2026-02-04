use super::*;
use crate::ast::{Statement};
use crate::vm::tree_walk::vm_value::{ValuePrimitive, ParsedValueType};
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
                let type_value = evaluate_expr(vm,type_expr)?.into_simplified_value(&mut vm.types);
                let expected_type_id = ParsedValueType::into_type_id(type_value, &mut vm.types)
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
            let r#ref = get_reference(vm, target)?;
            let evaluated_value = evaluate_expr(vm,value)?;
            unsafe {
                vm.variables.set_value_from_reference(
                    &r#ref,
                    evaluated_value,
                ).map_err(map_err)?
            };
            Ok(StatementEvent::None)
        }
        Statement::StatementBlock(stmtblock) => vm.execute_block(&stmtblock),
        Statement::If { condition, on_true } => {
            let Some(ValuePrimitive::Bool(x)) =
                evaluate_expr(vm,condition)?.try_into_primitive(&vm.types)
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
            let Some(ValuePrimitive::Bool(x)) =
                evaluate_expr(vm,condition)?.try_into_primitive(&vm.types)
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
                    vm.backend.debug_print(&expr.into_simplified_value(&vm.types).to_string()).unwrap();
                }
            }
            Ok(StatementEvent::None)
        }
        Statement::Continue => Ok(StatementEvent::Continue),
        Statement::Break => Ok(StatementEvent::Break),
        Statement::Loop { statements } => {
            loop {
                match vm.execute_block(statements)? {
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
                None=>VmValueGeneralized::create_unit(&mut vm.types),
            };
            Ok(StatementEvent::Return(return_value))
        }
        Statement::Comptime { .. } => {
            // Comptime blocks are executed during type checking phase
            // Skip execution during runtime
            Ok(StatementEvent::None)
        }
        Statement::ForeignCall(name) => {
            let result = vm
                .backend
                .call_foreign(name, &mut vm.variables, &mut vm.types)
                .map_err(map_err)?;
            Ok(StatementEvent::Return(result))
        }
        Statement::Expression(expr) => {
            // Evaluate the expression and discard the result
            evaluate_expr(vm, expr)?;
            Ok(StatementEvent::None)
        }
    }
}


