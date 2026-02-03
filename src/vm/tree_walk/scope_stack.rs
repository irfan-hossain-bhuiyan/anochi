use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display};
use macros::generate_unchecked;

use crate::prelude::{IndexPtr, SizedArray};
use crate::vm::tree_walk::ParsedValueType;
use crate::vm::tree_walk::vm_value::VmValueSimplified;
use crate::vm::tree_walk::vm_value::{Reference, VmValueGeneralized};
use crate::{
    ast::Identifier,
    types::{TypeContainer, TypeId},
    vm::tree_walk::{VmErrorType, VmUnit},
};
type ExprResult = Result<VmValueGeneralized, VmErrorType>;

#[derive(Debug, Clone)]
pub enum VariableState {
    #[allow(dead_code)]
    Constant,
    Immutable,
    #[allow(dead_code)]
    Mutable,
}
pub type StackPtr = IndexPtr<VmUnit>;
impl Display for VariableState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Constant => write!(f, "const"),
            Self::Immutable => write!(f, "let"),
            Self::Mutable => write!(f, "mut"),
        }
    }
}

pub type VmPtr = IndexPtr<VmUnit>;
#[derive(Debug, Clone)]
pub struct VariableData {
    pub type_id: TypeId,
    #[allow(dead_code)]
    var_state: VariableState,
    stack_position: VmPtr,
}

impl VariableData {
    fn new(type_id: TypeId, var_state: VariableState, stack_position: IndexPtr<VmUnit>) -> Self {
        Self {
            type_id,
            var_state,
            stack_position,
        }
    }

    fn get_ptr(&self) -> StackPtr {
        self.stack_position
    }

    fn get_index(&self) -> usize {
        self.get_ptr().as_index()
    }
}

type ScopeValues = HashMap<Identifier, VariableData>;

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
    stack: SizedArray<VmUnit>,
}

impl ScopeStack {
    /// Creates a new scope stack with global scope
    pub fn new() -> Self {
        let mut scopes = VecDeque::new();
        let stack = SizedArray::default();
        scopes.push_back(ScopeState::new(stack.len()));
        Self { scopes, stack }
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
    pub fn insert_variable_default(&mut self, identifier: Identifier, value: VmValueGeneralized) {
        self.insert_variable(identifier, value, VariableState::Immutable);
    }
    pub fn insert_variable(
        &mut self,
        identifier: Identifier,
        value: VmValueGeneralized,
        var_state: VariableState,
    ) {
        let type_id = value.r#type;
        let ptr = self.flatten_and_push(value);
        let var_data = VariableData::new(type_id, var_state, ptr);
        if let Some(current_scope) = self.scopes.back_mut() {
            current_scope.variables.insert(identifier, var_data);
        }
    }
    pub fn insert_variable_simple(&mut self,
        identifier: Identifier,
        value: VmValueSimplified,
        type_container: &mut TypeContainer
    ){
        self.insert_variable_default(identifier, value.into_vm_value_generalized(type_container));
    }

    pub fn insert_variable_check(
        &mut self,
        identifier: Identifier,
        value: VmValueGeneralized,
        expected_type_id: TypeId,
        type_container: &mut crate::types::TypeContainer,
    ) -> Result<(), VmErrorType> {
        if !value.r#type.can_cast_to(&expected_type_id, type_container) {
            return Err(VmErrorType::TypeMismatch(
                "Value type does not match expected type",
            ));
        }
        self.insert_variable_default(identifier, value);
        Ok(())
    }

    /// Inserts a variable with only type information (no value) during type checking phase
    /// Uses VmUnknown as a placeholder for the value
    pub fn insert_variable_type_only(
        &mut self,
        identifier: Identifier,
        type_id: TypeId,
        _type_container: &mut TypeContainer,
    ) {
        // Create a VmValueGeneralized with empty bits and the given type
        let value = VmValueGeneralized::new(vec![], type_id);
        self.insert_variable_default(identifier, value);
    }



    pub fn set_value_from_name(
        &mut self,
        identifier: &Identifier,
        value: VmValueGeneralized,
    ) -> Result<(), VmErrorType> {
        let var_data = self
            .get_variable_data(identifier)
            .ok_or_else(|| VmErrorType::UndefinedIdentifier(identifier.clone()))?;
        let expected_type_id = var_data.type_id;
        let value_type_id = value.r#type;
        if value_type_id != expected_type_id {
            return Err(VmErrorType::TypeMismatch(
                "Value type does not match variable type",
            ));
        }
        let stack_position = var_data.stack_position;
        self.overwrite_at_position(stack_position.as_index(), value);
        Ok(())
    }

    pub fn get_variable_data(&self, identifier: &Identifier) -> Option<&VariableData> {
        for scope in self.scopes.iter().rev() {
            if let Some(var_data) = scope.variables.get(identifier) {
                return Some(var_data);
            }
        }
        None
    }
    pub fn get_index_from_name(&self, identifier: &Identifier) -> Option<IndexPtr<VmUnit>> {
        Some(self.get_variable_data(identifier)?.get_ptr())
    }

    pub fn get_value_from_name(
        &self,
        identifier: &Identifier,
        type_container: &TypeContainer,
    ) -> ExprResult {
        let variable_data = self
            .get_variable_data(identifier)
            .ok_or(VmErrorType::UndefinedIdentifier(identifier.clone()))?;
        let type_id = variable_data.type_id;
        let index = variable_data.get_index();
        let size = type_container.get_metadata(&type_id).unwrap().size;
        let slice = &self.stack[index..index + size];
        Ok(VmValueGeneralized {
            bits: slice.to_vec(),
            r#type: type_id,
        })
    }

    pub fn has_variable(&self, identifier: &Identifier) -> bool {
        self.get_index_from_name(identifier).is_some()
    }
    /// Check if variable exists in current scope
    pub fn has_variable_current(&self, target: &Identifier) -> bool {
        self.current_scope().variables.contains_key(target)
    }
    pub fn get_reference_from_name(&self, id: &Identifier) -> Option<Reference> {
        let data=self.get_variable_data(id)?;
        Some(Reference { ptr: data.stack_position, type_id: data.type_id})
    }
    pub fn get_value_from_reference(&self, reference:&Reference,type_container: &TypeContainer) -> VmValueGeneralized {
        let size=type_container.get_metadata(&reference.type_id).unwrap().size;
        let bytes=self.get_slice(reference.ptr,size);
        VmValueGeneralized { bits: bytes, r#type: reference.type_id }
    }
    /// Overwrite middle of a stack given the index,
    /// It is useful if you want to edit the middle of a stack.
    unsafe fn set_value_from_index(
        &mut self,
        ptr: StackPtr,
        value: VmValueGeneralized,
    ) -> Result<(), VmErrorType> {
        self.overwrite_at_position_checked(ptr.as_index(), value)
    }
    pub unsafe fn set_value_from_reference(&mut self,r#ref:&Reference,value:VmValueGeneralized)->Result<(),VmErrorType>{
        if r#ref.type_id!=value.r#type{return Err(VmErrorType::TypeMismatch("reference has type mismatch"));}
        unsafe { self.set_value_from_index(r#ref.ptr, value) }
    }
    #[generate_unchecked]
    fn overwrite_at_position_checked(
        &mut self,
        position: usize,
        value: VmValueGeneralized,
    ) -> Result<(), VmErrorType> {
        let units = value.bits;
        if self.stack.len() < position + units.len() {
            return Err(VmErrorType::InvalidStackAccess);
        }
        for (i, unit) in units.into_iter().enumerate() {
            let slot = &mut self.stack[position + i];
            *slot = unit;
        }
        Ok(())
    }

    fn current_scope(&self) -> &ScopeState {
        self.scopes.back().unwrap()
    }

    fn flatten_and_push(&mut self, value: VmValueGeneralized) -> IndexPtr<VmUnit> {
        //TODO:I need to do somethinng for union type,example expanding it.
        self.stack.append(value.bits)
    }

    fn get_slice(&self, ptr: IndexPtr<VmUnit>, size: usize) -> Vec<VmUnit> {
        let index=ptr.as_index();
        self.stack[index..index+size].to_vec()
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
        for (idx, prim) in self.stack.iter().enumerate() {
            writeln!(f, "    [{}] {}", idx, prim)?;
        }
        writeln!(f, "  Scopes:")?;
        for (scope_idx, scope) in self.scopes.iter().enumerate() {
            writeln!(
                f,
                "    Scope {scope_idx} (stack_size: {}): {:?}",
                scope.stack_size_at_creation, scope.variables
            )?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}
