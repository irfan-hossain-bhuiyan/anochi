//! Virtual Machine for the Anochi programming language.

use std::collections::{HashMap, VecDeque};
mod vm_value;
pub use vm_value::{ValuePrimitive, VmValue};

use crate::{
    ast::{
        BinaryOperator, Expression, ExpressionNode, Identifier, Literal, Statement, StatementNode,
        UnaryOperator,
    },
    typing::{TypeId, UnifiedTypeDefinition},
    vm::backend::{IoBackend, VmBackend},
};

use thiserror::Error;

/// Error types for VM evaluation.
#[derive(Error, Debug, PartialEq)]
pub enum VmError {
    /// Division by zero error
    #[error("Division by zero")]
    DivisionByZero,
    /// Type mismatch error
    #[error("Type is mismatched,{0:?}")]
    TypeMismatch(&'static str),
    /// Undefined identifier error
    #[error("Undefined identifier: {0}")]
    UndefinedIdentifier(Identifier),
    /// Invalid operation error
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
    #[error("Unsupproted Operation {0:?}")]
    Unsupported(String),
    #[error("Invalid type defination")]
    InvalidTypeDefination,
}

impl VmError {
    /// Returns `true` if the vm error is [`TypeMismatch`].
    ///
    /// [`TypeMismatch`]: VmError::TypeMismatch
    #[must_use]
    pub fn is_type_mismatch(&self) -> bool {
        matches!(self, Self::TypeMismatch(..))
    }

    /// Returns `true` if the vm error is [`InvalidTypeDefination`].
    ///
    /// [`InvalidTypeDefination`]: VmError::InvalidTypeDefination
    #[must_use]
    pub fn is_invalid_type_defination(&self) -> bool {
        matches!(self, Self::InvalidTypeDefination)
    }
}

/// Result type for VM evaluation operations.
pub type VmResult = Result<VmValue, VmError>;
pub type VmResultMut<'a> = Result<&'a mut VmValue, VmError>;
/// Variable entry storing both value and type information
#[derive(Debug, Clone)]
pub struct VariableEntry {
    pub value: VmValue,
    pub type_id: crate::typing::TypeId,
}

/// Stack-based scope management for variables
#[derive(Debug)]
pub struct ScopeStack {
    scopes: VecDeque<HashMap<Identifier, VariableEntry>>,
}

impl ScopeStack {
    /// Creates a new scope stack with global scope
    pub fn new() -> Self {
        let mut scopes = VecDeque::new();
        scopes.push_back(HashMap::new()); // Global scope
        Self { scopes }
    }

    /// Creates a new scope (pushes new HashMap to stack)
    pub fn create_scope(&mut self) {
        self.scopes.push_back(HashMap::new());
    }

    /// Removes current scope (pops from stack)
    pub fn drop_scope(&mut self) {
        if self.scopes.len() <= 1 {
            // Keep at least global scope
            unreachable!("scope shouldn't get called when there is already 1 scope")
        }
        self.scopes.pop_back();
    }

    /// Inserts a variable in current scope, automatically inferring its type
    pub fn insert_variable(
        &mut self,
        identifier: Identifier,
        value: VmValue,
        type_container: &mut crate::typing::TypeContainer,
    ) {
        if let Some(type_def) = value.get_type_of_value() {
            let type_id = type_container.store_unified_type(type_def);
            if let Some(current_scope) = self.scopes.back_mut() {
                current_scope.insert(identifier, VariableEntry { value, type_id });
            }
        }
        // Note: If type cannot be inferred, we could panic or handle this case differently
    }

    /// Inserts a variable with type checking against expected type
    pub fn insert_variable_check(
        &mut self,
        identifier: Identifier,
        value: VmValue,
        expected_type_id: crate::typing::TypeId,
        type_container: &mut crate::typing::TypeContainer,
    ) -> Result<(), VmError> {
        if let Some(value_type_def) = value.get_type_of_value() {
            let value_type_id = type_container.store_unified_type(value_type_def);

            if value_type_id == expected_type_id {
                if let Some(current_scope) = self.scopes.back_mut() {
                    current_scope.insert(
                        identifier,
                        VariableEntry {
                            value,
                            type_id: expected_type_id,
                        },
                    );
                }
                Ok(())
            } else {
                Err(VmError::TypeMismatch(
                    "Value type does not match expected type",
                ))
            }
        } else {
            Err(VmError::TypeMismatch("Cannot infer type of value"))
        }
    }

    /// Sets/updates an existing variable with type checking
    pub fn set_variable(
        &mut self,
        identifier: &Identifier,
        value: VmValue,
        type_container: &mut crate::typing::TypeContainer,
    ) -> Result<(), VmError> {
        // First, find the existing variable to get its expected type
        let Some(existing_entry) = self.get_variable_entry(identifier) else {
            return Err(VmError::UndefinedIdentifier(identifier.clone()));
        };
        let expected_type_id = existing_entry.type_id;

        // Check if the new value matches the expected type
        let Some(value_type_id) = value.get_type_id_of_value(type_container) else {
            return Err(VmError::TypeMismatch("Cannot infer type of value"));
        };
        if value_type_id != expected_type_id {
            return Err(VmError::TypeMismatch(
                "Value type does not match variable type",
            ));
        }

        // Find and update the variable
        for scope in self.scopes.iter_mut().rev() {
            if let Some(entry) = scope.get_mut(identifier) {
                entry.value = value;
                return Ok(());
            }
        }
        Err(VmError::UndefinedIdentifier(identifier.clone()))
    }

    /// Gets a variable by searching from current scope to global
    pub fn get_variable(&self, identifier: &Identifier) -> Option<&VmValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(entry) = scope.get(identifier) {
                return Some(&entry.value);
            }
        }
        None
    }

    /// Gets a variable entry (with type) by searching from current scope to global
    pub fn get_variable_entry(&self, identifier: &Identifier) -> Option<&VariableEntry> {
        for scope in self.scopes.iter().rev() {
            if let Some(entry) = scope.get(identifier) {
                return Some(entry);
            }
        }
        None
    }
    pub fn get_variable_or_err(&self, identifier: &Identifier) -> VmResult {
        self.get_variable(identifier)
            .cloned()
            .ok_or(VmError::UndefinedIdentifier(identifier.clone()))
    }

    /// Checks if variable exists in any scope
    pub fn has_variable(&self, identifier: &Identifier) -> bool {
        self.get_variable(identifier).is_some()
    }
}

impl Default for ScopeStack {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default)]
pub struct Vm<Backend = IoBackend> {
    variables: ScopeStack,
    types: crate::typing::TypeContainer,
    backend: Backend,
}

type ExpNode<'a> = ExpressionNode<'a>;
type StmtNode<'a> = StatementNode<'a>;

impl<Backend: VmBackend> Vm<Backend> {
    pub fn new(backend: Backend) -> Self {
        let mut vm = Self {
            variables: ScopeStack::new(),
            types: crate::typing::TypeContainer::new(),
            backend,
        };
        vm.load_builtin_types();
        vm
    }

    fn load_builtin_types(&mut self) {
        use crate::typing::{BuiltinKind, TypeDefinition};

        let builtin_types = [
            ("i64", BuiltinKind::I64),
            ("f64", BuiltinKind::F64),
            ("bool", BuiltinKind::Bool),
            ("usize", BuiltinKind::Usize),
            ("type", BuiltinKind::Type),
        ];

        for (name, builtin_kind) in builtin_types {
            let type_def = TypeDefinition::Builtin(builtin_kind);
            let unified = type_def.to_unified();
            let type_id = self.types.store_unified_type(unified);
            self.variables
                .insert_variable(Identifier::new(name.to_string()), VmValue::Type(type_id), &mut self.types);
        }
    }

    pub fn evaluate_expr(&mut self, expression_node: &ExpNode) -> VmResult {
        let expression = &expression_node.node;
        match expression {
            Expression::Literal(literal) => match literal {
                Literal::Identifier(x) => self.variables.get_variable_or_err(x),
                Literal::Bool(_) | Literal::Float(_) | Literal::Integer(_) => Ok(
                    VmValue::ValuePrimitive(ValuePrimitive::from(literal.clone())),
                ),
                Literal::String(_) => {
                    // TODO: Handle strings as arrays when array implementation is ready
                    Err(VmError::Unsupported(
                        "String literals not yet supported as arrays".to_string(),
                    ))
                }
            },
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;

                vm_value::evaluate_binary_op(&left_val, operator, &right_val)
            }
            Expression::Unary { operator, operand } => {
                let operand_val = self.evaluate_expr(operand)?;
                vm_value::evaluate_unary_op(operator, &operand_val)
            }
            Expression::Grouping { expression } => self.evaluate_expr(expression),
            Expression::Product { data } => {
                let mut product = HashMap::new();
                for (key, value) in data.iter() {
                    product.insert(key.clone(), self.evaluate_expr(value)?);
                }
                Ok(VmValue::Product(product))
            }
            Expression::Sum { data } => {
                use std::collections::BTreeSet;

                let mut type_set = BTreeSet::new();
                for expr in data.iter() {
                    match self.evaluate_expr(expr)? {
                        VmValue::Type(type_id) => {
                            type_set.insert(type_id);
                        }
                        _ => {
                            return Err(VmError::InvalidOperation(
                                "Sum types can only contain type values".to_string(),
                            ));
                        }
                    }
                }

                // For sum types, we need to convert TypeIds back to TypeDefinitions to create the sum type
                let mut variants = BTreeSet::new();
                for type_id in type_set {
                    if let Some(_optimized_def) = self.types.get_type(&type_id) {
                        // Store the TypeId directly as UnifiedTypeDefinition::TypeId
                        variants.insert(UnifiedTypeDefinition::TypeId(type_id));
                    } else {
                        return Err(VmError::InvalidOperation(
                            "Type not found in container".to_string(),
                        ));
                    }
                }

                let unified = crate::typing::UnifiedTypeDefinition::Sum { variants };
                let type_id = self.types.store_unified_type(unified);
                Ok(VmValue::Type(type_id))
            }
            Expression::MemberAccess {
                object: _,
                member: _,
            } => todo!(),
        }
    }
    fn to_type(&mut self, value: VmValue) -> Result<TypeId, VmError> {
        value
            .into_type_id(&mut self.types)
            .ok_or(VmError::InvalidTypeDefination)
    }
    pub fn execute_statement(&mut self, stat_node:& StmtNode) -> Result<(), VmError> {
        let stmt = &stat_node.node;
        match stmt {
            Statement::Assignment {
                target,
                r#type,
                value,
            } => {
                let value = self.evaluate_expr(value)?;
                if let Some(type_expr) = r#type {
                    let type_value = self.evaluate_expr(type_expr)?;
                    let expected_type_id = type_value
                        .into_type_id(&mut self.types)
                        .ok_or(VmError::InvalidTypeDefination)?;
                    if !value.of_type(expected_type_id,&mut self.types){return Err(VmError::TypeMismatch(""));}
                    // Use insert_variable_check for type verification
                }
                    self.variables.insert_variable(target.clone(), value, &mut self.types);
                    // Use insert_variable for automatic type inference
                Ok(())
            }
            Statement::MutableAssignment { target, value } => {
                match &target.node {
                    Expression::Literal(Literal::Identifier(identifier)) => {
                        let evaluated_value = self.evaluate_expr(value)?;
                        self.variables.set_variable(identifier, evaluated_value, &mut self.types)?;
                        Ok(())
                    }
                    _ => {
                        // For member access and other complex assignments,
                        // return an error for now until type system is implemented
                        Err(VmError::InvalidOperation(
                            "Complex assignment not yet supported".to_string(),
                        ))
                    }
                }
            }
            Statement::StatementBlock { statements } => {
                self.variables.create_scope();
                for stmt in statements.iter() {
                    self.execute_statement(stmt)?;
                }
                self.variables.drop_scope();
                Ok(())
            }
            Statement::If { condition, on_true } => {
                let VmValue::ValuePrimitive(ValuePrimitive::Bool(x)) =
                    self.evaluate_expr(condition)?
                else {
                    return Err(VmError::TypeMismatch(
                        "The expression in if should be boolean",
                    ));
                };
                if x {
                    self.execute_statement(on_true)?;
                }
                Ok(())
            }
            Statement::IfElse {
                condition,
                on_true,
                on_false,
            } => {
                let VmValue::ValuePrimitive(ValuePrimitive::Bool(x)) =
                    self.evaluate_expr(condition)?
                else {
                    return Err(VmError::TypeMismatch(
                        "The expression on ifelse should be bool",
                    ));
                };
                if x {
                    self.execute_statement(on_true)?;
                } else {
                    self.execute_statement(on_false)?;
                }
                Ok(())
            }
            Statement::Debug { expr_vec } => {
                for expr in expr_vec.iter() {
                    let expr = self.evaluate_expr(expr)?;
                    self.backend.debug_print(&expr.to_string()).unwrap();
                }
                Ok(())
            }
        }
    }
}

//#[cfg(test)]
//mod scope_stack_tests;
#[cfg(test)]
mod vm_tests;
