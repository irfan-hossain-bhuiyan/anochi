use super::*;
use crate::ast::{Expression, Statement};
use crate::vm::tree_walk::vm_error::{VmError, VmErrorType};

/// Type checks a statement and stores type information in AST metadata
pub(super) fn type_check_statement<Backend: VmBackend>(
    vm: &mut Vm<Backend>,
    stat_node: &mut StatementNode,
) -> Result<(), VmError> {
    let node_data = stat_node.data().get_position().clone();
    let map_err = |e| VmError::new(e, node_data.clone());
    
    match &mut stat_node.stat {
        Statement::Assignment {
            target,
            r#type,
            value,
        } => {
            // Type check the value expression
            let value_type = type_check_expr(vm, value)?;
            
            // If explicit type annotation exists, verify compatibility
            let final_type = if let Some(type_expr) = r#type {
                let annotated_type = type_check_expr(vm, type_expr)?;
                
                // Verify value type matches annotation
                if !value_type.can_cast_to(&annotated_type, &mut vm.types) {
                    return Err(map_err(VmErrorType::TypeMismatch(
                        "Value type does not match type annotation"
                    )));
                }
                annotated_type
            } else {
                value_type
            };
            
            // Store type in AST metadata (temporarily disabled - needs mutable access)
            // TODO: Consider using interior mutability for CodeMetaData
            // if let Err(e) = stat_node.data().update_type(final_type) {
            //     return Err(map_err(VmErrorType::InvalidTypeDefination));
            // }
            
            // Insert variable with type-only info
            vm.insert_variable_type_only(target.clone(), final_type);
            
            Ok(())
        }
        
        Statement::Comptime { statements } => {
            // First: type check the block
            for stmt in statements.statements.iter_mut() {
                type_check_statement(vm, stmt)?;
            }
            
            // Then: execute the block (produces real values)
            vm.run_block(statements)?;
            
            Ok(())
        }
        
        Statement::Statements(block) => {
            for stmt in block.statements.iter_mut() {
                type_check_statement(vm, stmt)?;
            }
            Ok(())
        }
        
        Statement::StatementBlock(block) => {
            vm.create_scope();
            let result = (|| {
                for stmt in block.statements.iter_mut() {
                    type_check_statement(vm, stmt)?;
                }
                Ok(())
            })();
            vm.drop_scope();
            result
        }
        
        Statement::MutableAssignment { target, value } => {
            // Type check both sides
            let _target_type = type_check_expr(vm, target)?;
            let _value_type = type_check_expr(vm, value)?;
            
            // TODO: Verify type compatibility
            Ok(())
        }
        
        Statement::If { condition, on_true } => {
            let cond_type = type_check_expr(vm, condition)?;
            
            // Verify condition is boolean
            let bool_type = vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Bool);
            if cond_type != bool_type {
                return Err(map_err(VmErrorType::TypeMismatch(
                    "If condition must be boolean"
                )));
            }
            
            type_check_statement(vm, on_true)?;
            Ok(())
        }
        
        Statement::IfElse {
            condition,
            on_true,
            on_false,
        } => {
            let cond_type = type_check_expr(vm, condition)?;
            
            // Verify condition is boolean
            let bool_type = vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Bool);
            if cond_type != bool_type {
                return Err(map_err(VmErrorType::TypeMismatch(
                    "If condition must be boolean"
                )));
            }
            
            type_check_statement(vm, on_true)?;
            type_check_statement(vm, on_false)?;
            Ok(())
        }
        
        Statement::Debug { expr_vec } => {
            for expr in expr_vec.iter_mut() {
                type_check_expr(vm, expr)?;
            }
            Ok(())
        }
        
        Statement::Loop { statements } => {
            vm.create_scope();
            let result = (|| {
                for stmt in statements.statements.iter_mut() {
                    type_check_statement(vm, stmt)?;
                }
                Ok(())
            })();
            vm.drop_scope();
            result
        }
        
        Statement::Break | Statement::Continue => Ok(()),
        
        Statement::Return(opt_expr) => {
            if let Some(expr) = opt_expr {
                type_check_expr(vm, expr)?;
            }
            Ok(())
        }
        
        Statement::ForeignCall(_name) => {
            // Foreign calls are assumed to be type-safe
            Ok(())
        }
        
        Statement::Expression(expr) => {
            type_check_expr(vm, expr)?;
            Ok(())
        }
    }
}

/// Type checks an expression and returns its type
pub(super) fn type_check_expr<Backend: VmBackend>(
    vm: &mut Vm<Backend>,
    expr_node: &mut ExpressionNode,
) -> Result<TypeId, VmError> {
    let node_data = expr_node.data().get_position().clone();
    let map_err = |e| VmError::new(e, node_data.clone());
    
    let type_id = match &mut expr_node.exp {
        Expression::Literal(literal) => {
            match literal {
                crate::ast::Literal::Identifier(id) => {
                    // Look up variable type
                    let var_data = vm.variables.get_variable_data(id)
                        .ok_or_else(|| map_err(VmErrorType::UndefinedIdentifier(id.clone())))?;
                    var_data.type_id
                }
                crate::ast::Literal::Bool(_) => vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Bool),
                crate::ast::Literal::Integer(_) => vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Int),
                crate::ast::Literal::Float(_) => vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Float),
                crate::ast::Literal::String(_) => {
                    // TODO: Implement string type
                    return Err(map_err(VmErrorType::InvalidOperation(
                        "String type not yet implemented".to_string()
                    )));
                }
            }
        }
        
        Expression::Binary { left, operator, right } => {
            let left_type = type_check_expr(vm, left)?;
            let right_type = type_check_expr(vm, right)?;
            
            // TODO: Implement proper type checking for binary operators
            // For now, assume both operands must have the same type
            if left_type != right_type {
                return Err(map_err(VmErrorType::TypeMismatch(
                    "Binary operator operands must have the same type"
                )));
            }
            
            // Result type depends on operator
            use crate::ast::BinaryOperator::*;
            match operator {
                Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual | And | Or => {
                    vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Bool)
                }
                _ => left_type,
            }
        }
        
        Expression::Unary { operator, operand } => {
            use crate::ast::UnaryOperator::*;
            match operator {
                Ref => {
                    let inner_type = type_check_expr(vm, operand)?;
                    let ref_type_def = UnifiedTypeDefinition::reference(
                        UnifiedTypeDefinition::TypeId(inner_type)
                    );
                    vm.types.store_unified_type(ref_type_def)
                }
                Deref => {
                    let _operand_type = type_check_expr(vm, operand)?;
                    // TODO: Extract inner type from reference
                    return Err(map_err(VmErrorType::InvalidOperation(
                        "Deref type checking not yet implemented".to_string()
                    )));
                }
                _ => {
                    let operand_type = type_check_expr(vm, operand)?;
                    operand_type
                }
            }
        }
        
        Expression::Grouping { expression } => {
            type_check_expr(vm, expression)?
        }
        
        Expression::Product { data } => {
            let mut fields = std::collections::BTreeMap::new();
            for (key, value) in data.iter_mut() {
                let field_type = type_check_expr(vm, value)?;
                fields.insert(key.clone(), UnifiedTypeDefinition::TypeId(field_type));
            }
            let product_type = UnifiedTypeDefinition::product(fields);
            vm.types.store_unified_type(product_type)
        }
        
        Expression::Sum { data: _ } => {
            // TODO: Implement sum type checking
            return Err(map_err(VmErrorType::InvalidOperation(
                "Sum type checking not yet implemented".to_string()
            )));
        }
        
        Expression::MemberAccess { object, member } => {
            let obj_type = type_check_expr(vm, object)?;
            
            // Get the product type definition
            let type_def = vm.types.get_type(&obj_type)
                .ok_or_else(|| map_err(VmErrorType::InvalidTypeDefination))?;
            
            match &type_def.0 {
                crate::types::CompTimeTypeGeneric::Product(fields) => {
                    let member_type_id = fields.get(member)
                        .ok_or_else(|| map_err(VmErrorType::UndefinedIdentifier(member.clone())))?;
                    
                    *member_type_id
                }
                _ => return Err(map_err(VmErrorType::TypeMismatch(
                    "Member access requires a product type"
                ))),
            }
        }
        
        Expression::Function { input, output, statements: _ } => {
            let _input_type = type_check_expr(vm, input)?;
            
            let _output_type = if let Some(output_expr) = output {
                Some(type_check_expr(vm, output_expr)?)
            } else {
                None
            };
            
            // TODO: Type check function body in a new scope
            // For now, just return function type
            vm.types.get_builtin_type_id(crate::types::CompTimeBuiltinType::Func)
        }
        
        Expression::FnCall { caller, callee } => {
            let _caller_type = type_check_expr(vm, caller)?;
            let _callee_type = type_check_expr(vm, callee)?;
            
            // TODO: Implement proper function call type checking
            // For now, return a placeholder
            vm.types.get_unit_type()
        }
    };
    
    // Store type in AST metadata (AST metadata is immutable from here)
    // We skip storing because we can't mutate through the method
    // TODO: Consider making CodeMetaData mutable or using interior mutability
    // if let Err(_e) = expr_node.data().update_type(type_id) {
    //     return Err(map_err(VmErrorType::InvalidTypeDefination));
    // }
    
    Ok(type_id)
}
