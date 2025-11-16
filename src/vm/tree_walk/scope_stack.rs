use std::collections::{HashMap, VecDeque};
use std::fmt::Display;

use thiserror::Error;

use crate::{
    ast::Identifier,
    types::{TypeContainer, TypeId},
    vm::tree_walk::{VmError, VmResult, VmValue},
};

#[derive(Debug,Clone,Error)]
enum Error{
    #[error("You can't mutate a constant variable")]
    MutatingConstant,
}

#[derive(Debug, Clone)]
enum VariableState {
    Constant,
    Immutable,
    Mutable,
}

impl Display for VariableState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant => write!(f, "const"),
            Self::Immutable => write!(f, "let"),
            Self::Mutable => write!(f, "mut"),
        }
    }
}
#[derive(Debug, Clone)]
pub struct VariableEntry {
    pub value: VmValue,
    pub type_id: TypeId,
    pub var_state: VariableState,
}

impl VariableEntry {
    pub fn new(
        value: VmValue,
        var_state: VariableState,
        type_container: &mut TypeContainer,
    ) -> Self {
        let type_id=value.get_type_id_of_value(type_container);
        unsafe{Self::new_unchecked(value, type_id, var_state)}
    }
    pub fn new_checked(
        value: VmValue,
        type_id: TypeId,
        var_state: VariableState,
        type_container: &mut TypeContainer,
    ) -> Option<Self> {
        if !value.of_type(type_id, type_container) {
            return None;
        }
        Some(Self {
            value,
            type_id,
            var_state,
        })
    }
    pub unsafe fn new_unchecked(value: VmValue, type_id: TypeId, var_state: VariableState) -> Self {
        Self {
            value,
            type_id,
            var_state,
        }
    }
}
type ScopeValues=HashMap<Identifier,VariableEntry>;
/// Stack-based scope management for variables
#[derive(Debug)]
pub struct ScopeStack {
    scopes: VecDeque<ScopeValues>,
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
        type_container: &mut crate::types::TypeContainer,
    ) {
        let entry = VariableEntry::new(value, VariableState::Immutable, type_container);
        if let Some(current_scope) = self.scopes.back_mut() {
            current_scope.insert(identifier, entry);
        }
    }

    /// Inserts a variable with type checking against expected type
    pub fn insert_variable_check(
        &mut self,
        identifier: Identifier,
        value: VmValue,
        expected_type_id: TypeId,
        type_container: &mut crate::types::TypeContainer,
    ) -> Result<(), VmError> {
        let Some(entry) = VariableEntry::new_checked(value, expected_type_id, VariableState::Immutable, type_container) else {
            return Err(VmError::TypeMismatch("Value type does not match expected type"));
        };
        
        if let Some(current_scope) = self.scopes.back_mut() {
            current_scope.insert(identifier, entry);
        }
        Ok(())
    }

    /// Sets/updates an existing variable with type checking
    pub fn set_variable(
        &mut self,
        identifier: &Identifier,
        value: VmValue,
        type_container: &mut crate::types::TypeContainer,
    ) -> Result<(), VmError> {
        // First, find the existing variable to get its expected type
        let Some(existing_entry) = self.get_variable_entry(identifier) else {
            return Err(VmError::UndefinedIdentifier(identifier.clone()));
        };
        let expected_type_id = existing_entry.type_id;

        // Check if the new value matches the expected type
        let value_type_id = value.get_type_id_of_value(type_container);
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
    /// Check if variable exists in current scope
    pub(crate) fn has_variable_current(&self, target: &Identifier) -> bool {
        self.current_scope().get(target).is_some()
    }

    fn current_scope(&self) -> &ScopeValues {
        self.scopes.front().unwrap()
    }
}

impl Display for ScopeStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (scope_index, scope) in self.scopes.iter().enumerate() {
            if scope_index > 0 {
                writeln!(f, "==scope==")?;
            }
            
            for (identifier, entry) in scope {
                writeln!(f, "{} {} = {};", entry.var_state, identifier, entry.value)?;
            }
        }
        Ok(())
    }
}

impl Default for ScopeStack {
    fn default() -> Self {
        Self::new()
    }
}
