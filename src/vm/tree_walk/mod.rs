//! Virtual Machine for the Anochi programming language.

use std::collections::HashMap;
mod vm_value;
pub use vm_value::{StructValue, ValuePrimitive, VmValue};

use crate::{
    ast::{
         ExprNode, Identifier, StatNode, StatementBlock
    }, prelude::IndexCons, token::tokenizer::HasPosition, types::{TypeContainer, TypeId, UnifiedTypeDefinition}, vm::{
        backend::{IoBackend, VmBackend},
        tree_walk::{vm_error::VmErrorType, vm_value::{FuncId, VmFunc}},
        
    }
};
mod vm_error;
pub use vm_error::{VmError};

/// Result type for VM evaluation operations.
pub type VmExprResult = Result<VmValue, VmError>;
pub type VmResultMut<'a>= Result<&'a mut VmValue,VmError >;
/// Result type for statement execution
pub type StatementResult = Result<StatementEvent, VmError>;

/// Statement execution events for control flow
#[derive(Debug, Clone, PartialEq)]
pub enum StatementEvent {
    Break,
    Continue,
    Return(VmValue),
    None,
}

pub type FunctionContainer = IndexCons<vm_value::VmFunc>;
/// Variable entry storing both value and type information
mod scope_stack;
use scope_stack::ScopeStack;
#[derive(Debug, Default)]
pub struct Vm<Backend = IoBackend> {
    pub(super) variables: ScopeStack,
    pub(super) types: TypeContainer,
    pub(super) funcs: FunctionContainer,
    pub(super) backend: Backend,
}

type ExpNode<T> = ExprNode<T>;
type StmtNode<T> = StatNode<T>;

mod evaluation;
mod execution;

impl<Backend: VmBackend> Vm<Backend> {
    pub fn new(backend: Backend) -> Self {
        let mut vm = Self {
            variables: ScopeStack::new(),
            types: crate::types::TypeContainer::new(),
            funcs: FunctionContainer::new(),
            backend,
        };
        vm.load_builtin_types();
        vm
    }
    fn get_type_def(&self, id: &TypeId) -> Option<crate::types::TypeDefinition> {
        self.types.get_type_def(id)
    }
    fn load_builtin_types(&mut self) {
        use crate::types::BuiltinKind;

        let builtin_types = [
            ("i64", BuiltinKind::I64),
            ("f64", BuiltinKind::F64),
            ("bool", BuiltinKind::Bool),
            ("usize", BuiltinKind::Usize),
        ];

        for (name, builtin_kind) in builtin_types {
            let type_def = UnifiedTypeDefinition::builtin(builtin_kind);
            let type_id = self.types.store_unified_type(type_def);
            self.variables.insert_variable(
                Identifier::new(name.to_string()),
                VmValue::Type(type_id),
                &mut self.types,
            );
        }
    }
    pub fn extract_struct(&mut self, strct: StructValue) -> Result<(), VmErrorType> {
        for (k, v) in strct.into_iter() {
            self.insert_variable(k, v)?;
        }
        Ok(())
    }
    pub fn evaluate_expr<T:Clone+HasPosition>(&mut self, expression_node: &ExpNode<T>) -> VmExprResult {
        evaluation::evaluate_expr(self, expression_node)
    }

    pub(super) fn to_type(&mut self, value: VmValue) -> Result<TypeId, VmErrorType> {
        value
            .into_type_id(&mut self.types)
            .ok_or(VmErrorType::InvalidTypeDefination)
    }
    pub(super) fn insert_variable(&mut self, target: Identifier, value: VmValue) -> Result<(), VmErrorType> {
        if self.variables.has_variable_current(&target) {
            return Err(VmErrorType::SameVariableName);
        }
        self.variables
            .insert_variable(target, value, &mut self.types);
        Ok(())
    }
    pub fn execute_statement<T:Clone+HasPosition>(&mut self, stat_node: &StmtNode<T>) -> StatementResult {
        execution::execute_statement(self, stat_node)
    }

    pub(crate) fn print_stack(&self) {
        println!("{}", self.variables);
    }

    pub fn insert_variable_check(
        &mut self,
        identifier: Identifier,
        value: VmValue,
        expected_type_id: TypeId,
        type_container: &mut crate::types::TypeContainer,
    ) -> Result<(), VmErrorType> {
        self.variables
            .insert_variable_check(identifier, value, expected_type_id, type_container)
    }

    pub(super) fn run_block<T:Clone+HasPosition>(
        &mut self,
        stmtblock: &StatementBlock<T>,
    ) -> Result<StatementEvent, VmError> {
        self.create_scope();
        let mut inner_code = || {
            for stmt in stmtblock.statements.iter() {
                let output = self.execute_statement(stmt)?;
                match output {
                    StatementEvent::None => {}
                    _ => return Ok(output),
                }
            }
            Ok(StatementEvent::None)
        };
        let output = inner_code();
        self.drop_scope();
        output
    }

    fn create_scope(&mut self) {
        self.variables.create_scope();
    }

    fn drop_scope(&mut self) {
        self.variables.drop_scope();
    }

    pub(super) fn add_function(&mut self, func: VmFunc) -> FuncId {
        self.funcs.push(func)
    }
    /// It type check the function that is currently passed,and execute it.
    fn execute_function<T: HasPosition>(&mut self, _func_id: &FuncId, _inputs: VmValue) -> VmExprResult {
        todo!()
    }
    fn get_func(&self, func_id: &FuncId) -> &VmFunc {
        self.funcs.get(func_id).unwrap()
    }
    fn get_func_mut(&mut self, func_id: &FuncId) -> &mut VmFunc {
        self.funcs.get_mut(func_id).unwrap()
    }

    fn type_match(&mut self, r#type: TypeId, object: VmValue) -> Result<(), VmErrorType> {
        if self.to_type(object)? == r#type {
            return Ok(());
        }
        Err(VmErrorType::TypeMismatch(""))
    }
}

//#[cfg(test)]
//mod scope_stack_tests;
//#[cfg(test)]
//mod vm_tests;
