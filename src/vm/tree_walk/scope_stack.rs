use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display};

use thiserror::Error;


use crate::prelude::{Allocator, IndexPtr, SizedArray};
use crate::{
    ast::Identifier,
    types::{TypeContainer, TypeId},
    vm::tree_walk::{VmErrorType, VmVal, VmValue},
};
type ExprResult=Result<VmValue,VmErrorType>;
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
    var_state: VariableState,
}

impl VariableEntry {
    fn new(
        value: VmValue,
        var_state: VariableState,
        type_container: &mut TypeContainer,
    ) -> Self {
        let type_id=value.get_type_id_of_value(type_container);
        unsafe{Self::new_unchecked(value, type_id, var_state)}
    }
    fn new_checked(
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
    unsafe fn new_unchecked(value: VmValue, type_id: TypeId, var_state: VariableState) -> Self {
        Self {
            value,
            type_id,
            var_state,
        }
    }
}
type ScopeValues=HashMap<Identifier,IndexPtr<VariableEntry>>;

#[derive(Debug)]
struct ScopeState {
    variables: ScopeValues,
    stack_size_at_creation: usize,
}

impl ScopeState {
    fn new(stack_size: usize) -> Self {
        Self {
            variables: HashMap::new(),
            stack_size_at_creation: stack_size,
        }
    }
}
/// Stack-based scope management for variables
#[derive(Debug)]
pub struct ScopeStack {
    scopes: VecDeque<ScopeState>,
    stack:SizedArray<VariableEntry>
}

impl ScopeStack {
    /// Creates a new scope stack with global scope
    pub fn new() -> Self {
        let mut scopes = VecDeque::new();
        let stack=SizedArray::default();
        scopes.push_back(ScopeState::new(stack.len()));
        Self { scopes,stack }
    }

    pub fn create_scope(&mut self) {
        self.scopes.push_back(ScopeState::new(self.stack.len()));
    }

    pub fn drop_scope(&mut self) {
        if self.scopes.len() <= 1 {
            unreachable!("scope shouldn't get called when there is already 1 scope")
        }
        if let Some(scope) = self.scopes.pop_back() {
            let _ = self.stack.shrink_size(scope.stack_size_at_creation);
        }
    }

    /// Inserts a variable in current scope, automatically inferring its type
    pub fn insert_variable(
        &mut self,
        identifier: Identifier,
        value: VmValue,
        type_container: &mut crate::types::TypeContainer,
    ) {
        let entry = VariableEntry::new(value, VariableState::Immutable, type_container);
        self.insert_var_entry(identifier, entry);
    }

    fn insert_var_entry(&mut self, identifier: Identifier, entry: VariableEntry) {
        let index = self.stack.push_back(entry);
        if let Some(current_scope) = self.scopes.back_mut() {
            current_scope.variables.insert(identifier, index);
        }
    }

    /// Inserts a variable with type checking against expected type
    pub fn insert_variable_check(
        &mut self,
        identifier: Identifier,
        value: VmValue,
        expected_type_id: TypeId,
        type_container: &mut crate::types::TypeContainer,
    ) -> Result<(), VmErrorType> {
        let Some(entry) = VariableEntry::new_checked(value, expected_type_id, VariableState::Immutable, type_container) else {
            return Err(VmErrorType::TypeMismatch("Value type does not match expected type"));
        };
        self.insert_var_entry(identifier, entry);
        //current_scope.insert(identifier,);
        Ok(())
    }

    pub fn set_value_from_index(
        &mut self,
        ptr: IndexPtr<VariableEntry>,
        value: VmValue,
        type_container: &mut TypeContainer,
    ) -> Result<(), VmErrorType> {
        let expected_type_id = self.get_variable_entry_from_index(ptr).type_id;
        let value_type_id = value.get_type_id_of_value(type_container);
        if value_type_id != expected_type_id {
            return Err(VmErrorType::TypeMismatch(
                "Value type does not match variable type",
            ));
        }
        self.get_mut_from_index(ptr).value = value;
        Ok(())
    }

    pub fn set_value_from_name(
        &mut self,
        identifier: &Identifier,
        value: VmValue,
        type_container: &mut TypeContainer,
    ) -> Result<(), VmErrorType> {
        let Some(ptr) = self.get_index_from_name(identifier) else {
            return Err(VmErrorType::UndefinedIdentifier(identifier.clone()));
        };
        self.set_value_from_index(ptr, value, type_container)
    }

    pub fn get_variable_entry_from_index(&self, ptr: IndexPtr<VariableEntry>) -> &VariableEntry {
        self.stack.get_from_ptr(ptr)
    }

    pub fn get_mut_from_index(&mut self, ptr: IndexPtr<VariableEntry>) -> &mut VariableEntry {
        self.stack.get_mut_from_ptr(ptr)
    }

    pub fn get_value_from_index(&self, ptr: IndexPtr<VariableEntry>) -> &VmValue {
        &self.get_variable_entry_from_index(ptr).value
    }

    pub fn get_index_from_name(&self, identifier: &Identifier) -> Option<IndexPtr<VariableEntry>> {
        for scope in self.scopes.iter().rev() {
            if let Some(ptr) = scope.variables.get(identifier) {
                return Some(*ptr);
            }
        }
        None
    }

    pub fn get_variable_entry_from_name(&self, identifier: &Identifier) -> Option<&VariableEntry> {
        let ptr = self.get_index_from_name(identifier)?;
        Some(self.get_variable_entry_from_index(ptr))
    }

    pub fn get_value_from_name(&self, identifier: &Identifier) -> Option<&VmValue> {
        let ptr = self.get_index_from_name(identifier)?;
        Some(self.get_value_from_index(ptr))
    }
    pub fn get_value_or_err(&self, identifier: &Identifier) -> ExprResult {
        let ptr = self.get_index_from_name(identifier)
            .ok_or_else(|| VmErrorType::UndefinedIdentifier(identifier.clone()))?;
        Ok(self.get_value_from_index(ptr).clone())
    }

    pub fn has_variable(&self, identifier: &Identifier) -> bool {
        self.get_index_from_name(identifier).is_some()
    }
    /// Check if variable exists in current scope
    pub(crate) fn has_variable_current(&self, target: &Identifier) -> bool {
        self.current_scope().variables.contains_key(target)
    }

    fn current_scope(&self) -> &ScopeState {
        self.scopes.back().unwrap()
    }
}


impl Default for ScopeStack {
    fn default() -> Self {
        Self::new()
    }
}
impl Display for ScopeStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ScopeStack {{")?;
        writeln!(f, "  Stack (bottom to top):")?;
        for (idx, entry) in self.stack.iter().enumerate() {
            writeln!(f, "    [{}] {} = {} (type: {:?})", idx, entry.var_state, entry.value, entry.type_id)?;
        }
        writeln!(f, "  Scopes:")?;
        for (scope_idx, scope) in self.scopes.iter().enumerate() {
            writeln!(f, "    Scope {scope_idx} (stack_size: {}): {:?}", scope.stack_size_at_creation, scope.variables)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}
