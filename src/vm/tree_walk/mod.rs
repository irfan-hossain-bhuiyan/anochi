//! Virtual Machine for the Anochi programming language.

use std::collections::{HashMap, VecDeque};
mod vm_value;
pub use vm_value::{ValuePrimitive, VmValue};

use crate::{
    ast::{
        BinaryOperator, Expression, ExpressionNode, Identifier, Literal, Statement, StatementNode,
        UnaryOperator,
    },
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

}

/// Result type for VM evaluation operations.
pub type VmResult = Result<VmValue, VmError>;
pub type VmResultMut<'a>= Result<&'a mut VmValue,VmError>;
/// Stack-based scope management for variables
#[derive(Debug)]
pub struct ScopeStack {
    scopes: VecDeque<HashMap<Identifier, VmValue>>,
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
        if self.scopes.len() <= 1 { // Keep at least global scope
            unreachable!("scope shouldn't get called when there is already 1 scope")
        }
        self.scopes.pop_back();
    }
    
    /// Sets a variable in current scope
    pub fn set_variable(&mut self, identifier: Identifier, value: VmValue) {
        if let Some(current_scope) = self.scopes.back_mut() {
            current_scope.insert(identifier, value);
        }
    }
    
    /// Gets a variable by searching from current scope to global
    pub fn get_variable(&self, identifier: &Identifier) -> Option<&VmValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(identifier) {
                return Some(value);
            }
        }
        None
    }
    pub fn get_variable_or_err(&self,identifier: &Identifier)->VmResult{
        self.get_variable(identifier).cloned().ok_or(VmError::UndefinedIdentifier(identifier.clone()))
    }
    
    /// Gets mutable reference to variable by searching from current scope to global
    pub fn get_variable_mut(&mut self, identifier: &Identifier) -> Option<&mut VmValue> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(value) = scope.get_mut(identifier) {
                return Some(value);
            }
        }
        None
    }
    pub fn get_variable_mut_or_err(&mut self,identifier: &Identifier) ->VmResultMut {
        self.get_variable_mut(identifier).ok_or(VmError::UndefinedIdentifier(identifier.clone()))
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
                .set_variable(Identifier::new(name.to_string()), VmValue::Type(type_id));
        }
    }

    /// Evaluates an expression using tree walking.
    ///
    /// This method recursively walks through the AST and evaluates expressions.
    /// Currently supports integer and float arithmetic operations, comparison
    /// operations, and logical operations. String operations and identifier
    /// resolution are not yet implemented.
    ///
    /// # Arguments
    ///
    /// * `expression` - The expression to evaluate
    ///
    /// # Returns
    ///
    /// A `Result` containing the evaluated `Literal` value, or a `VmError`.
    ///
    ///
    ///
    pub fn evaluate_expr(&mut self, expression_node: &ExpNode) -> VmResult {
        let expression = &expression_node.node;
        match expression {
            Expression::Literal(literal) => match literal {
                Literal::Identifier(x) => self
                    .variables
                    .get_variable_or_err(x),
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
                        // We just need to store the TypeId for the optimized version
                        // Create a TypeRef::Reference for each TypeId
                        variants.insert(crate::typing::TypeRef::Reference(type_id));
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
    pub fn execute_statement(&mut self, stat_node: &StmtNode) -> Result<(), VmError> {
        let stmt = &stat_node.node;
        match stmt {
            Statement::Assignment { target, r#type, value } =>{
                let value=self.evaluate_expr(value)?;
                self.variables.set_variable(target.clone(), value);
                Ok(())
            }
            Statement::MutableAssignment {
                target,
                value,
            } => {
                match &target.node {
                    Expression::Literal(Literal::Identifier(identifier)) => {
                        let evaluated_value = self.evaluate_expr(value)?;
                        *self.variables.get_variable_mut_or_err(identifier)?= evaluated_value;
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
                    return Err(VmError::TypeMismatch("The expression in if should be boolean"));
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
                    return Err(VmError::TypeMismatch("The expression on ifelse should be bool"));
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

    /// Maps a VmValue to its corresponding TypeId
    ///
    /// # Arguments
    /// * `value` - The VmValue to get the type for
    ///
    /// # Returns
    /// * `Ok(TypeId)` - The TypeId representing the type of the value
    /// * `Err(VmError)` - If the operation fails (e.g., mixed type/value products)
    ///
    /// # Behavior
    /// - If value is `Type(type_id)` → returns "type of type" TypeId
    /// - If value is `ValuePrimitive` → creates corresponding builtin TypeId
    /// - If value is `Product` with all values → creates product type TypeId
    /// - If product contains mixed types/values → returns error (future implementation)
    pub fn value_to_type(&mut self, value: &VmValue) -> Result<crate::typing::TypeId, VmError> {
        use crate::typing::{BuiltinKind, TypeRef, UnifiedTypeDefinition};
        use std::collections::BTreeMap;

        match value {
            VmValue::Type(_) => {
                // Return "type of type" TypeId (meta-type)
                let type_def = UnifiedTypeDefinition::Builtin(BuiltinKind::Type);
                Ok(self.types.store_unified_type(type_def))
            }
            VmValue::ValuePrimitive(primitive) => {
                let builtin_kind = match primitive {
                    ValuePrimitive::Bool(_) => BuiltinKind::Bool,
                    ValuePrimitive::Integer(_) => BuiltinKind::I64,
                    ValuePrimitive::Float(_) => BuiltinKind::F64,
                };
                let type_def = UnifiedTypeDefinition::Builtin(builtin_kind);
                Ok(self.types.store_unified_type(type_def))
            }
            VmValue::Product(fields) => {
                // Check if product contains mixed types and values
                let mut has_types = false;
                let mut has_values = false;

                for field_value in fields.values() {
                    match field_value {
                        VmValue::Type(_) => has_types = true,
                        VmValue::ValuePrimitive(_) | VmValue::Product(_) => has_values = true,
                    }
                }

                if has_types && has_values {
                    return Err(VmError::InvalidOperation(
                        "Mixed type/value products not yet implemented".to_string(),
                    ));
                }

                // Create product type from field types
                let mut type_fields = BTreeMap::new();
                for (field_name, field_value) in fields {
                    let field_type_id = self.value_to_type(field_value)?;
                    type_fields.insert(field_name.clone(), TypeRef::Reference(field_type_id));
                }

                let product_type = UnifiedTypeDefinition::Product {
                    fields: type_fields,
                };
                Ok(self.types.store_unified_type(product_type))
            }
        }
    }
}

#[cfg(test)]
mod scope_stack_tests;
#[cfg(test)]
mod vm_tests;

