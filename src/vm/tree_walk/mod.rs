//! Virtual Machine for the Anochi programming language.

use std::collections::BTreeMap;

use crate::{
    prelude::HashValue, vm::backend::ForeignFuncSignature,
};
use enum_as_inner::EnumAsInner;
use num_bigint::BigInt;
use num_rational::BigRational;
use vm_value::VmValueGeneralized;
/// Untyped VM unit - the fundamental storage unit on the VM stack
/// All type information is tracked separately via TypeContainer and VariableData
#[derive(Debug, Clone, PartialEq,EnumAsInner)]
pub enum VmUnit {
    Bool(bool),
    Integer(BigInt),
    Float(BigRational),
    Usize(usize),
    HashValue(HashValue),
}

impl std::fmt::Display for VmUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmUnit::Bool(b) => write!(f, "{b}"),
            VmUnit::Integer(i) => write!(f, "{i}"),
            VmUnit::Float(fl) => write!(f, "{fl}"),
            VmUnit::Usize(u) => write!(f, "@{u}"),
            VmUnit::HashValue(hash) => write!(f, "{hash}"),
        }
    }
}

pub mod vm_value;
pub use vm_value::{ParsedValueType, StructValue, ValuePrimitive, VmValueSimplified};

use crate::{
    ast::{ExpressionNode, Identifier, StatementNode, StatmentBlockNode},
    prelude::IndexCons,
    token::tokenizer::HasPosition,
    types::{TypeContainer, TypeId, UnifiedTypeDefinition},
    vm::{
        backend::{IoBackend, VmBackend},
        tree_walk::vm_value::{FuncId, VmFunc},
    },
};
pub mod raylib_backend;
pub mod vm_error;
pub use vm_error::{VmError, VmErrorType};

/// Result type for VM evaluation operations.
pub type VmExprResult = Result<VmValueGeneralized, VmError>;
pub type VmExprResultType = Result<VmValueGeneralized, VmErrorType>;
pub type VmResultMut<'a> = Result<&'a mut VmValueGeneralized, VmError>;
/// Result type for statement execution
pub type StatementResult = Result<StatementEvent, VmError>;

/// Statement execution events for control flow
#[derive(Debug, Clone, PartialEq)]
pub enum StatementEvent {
    Break,
    Continue,
    Return(VmValueGeneralized),
    None,
}

pub type FunctionContainer = IndexCons<vm_value::VmFunc>;
/// Variable entry storing both value and type information
pub mod scope_stack;
use scope_stack::ScopeStack;

#[derive(Debug, Default)]
pub struct Vm<Backend = IoBackend> {
    pub(super) variables: ScopeStack,
    pub(super) types: TypeContainer,
    pub(super) funcs: FunctionContainer,
    pub(super) backend: Backend,
}


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

    pub fn initialize(&mut self) -> Result<(), VmErrorType> {
         self.backend.initialize().map_err(|e| VmErrorType::ForeignError(e.to_string()))?;
         self.init_foreign_functions();
         Ok(())
    }

    fn init_foreign_functions(&mut self) {
        let signatures = self.backend.get_foreign_signatures();
        for sig in signatures {
            self.bind_foreign_function(sig);
        }
    }

    fn bind_foreign_function(&mut self, sig: ForeignFuncSignature) {
         use crate::ast::{Identifier, Statement};
         use crate::types::CompTimeTypeGeneric;
         
         // 1. Create argument structure type
         let mut fields: BTreeMap<Identifier, UnifiedTypeDefinition> = BTreeMap::new();
         for (name, type_name) in &sig.params {
             if let Some(type_id) = self.get_type_id_by_name(type_name) {
                 fields.insert(Identifier::new(name.clone()), UnifiedTypeDefinition::TypeId(type_id));
             } else {
                 eprintln!("Warning: Unknown type '{}' for param '{}' in foreign function '{}'. Skipping binding.", type_name, name, sig.name);
                 return;
             }
         }
         
         let param_type_def = UnifiedTypeDefinition::TypeDef(
             CompTimeTypeGeneric::Product(fields)
         );
         let param_type_id = self.types.store_unified_type(param_type_def);
         
         // 2. Resolve return type
         let return_type_id = if let Some(rt) = sig.return_type {
             if let Some(tid) = self.get_type_id_by_name(&rt) {
                 Some(tid)
             } else {
                 eprintln!("Warning: Unknown return type '{}' for foreign function '{}'. Skipping binding.", rt, sig.name);
                 return;
             }
         } else {
             None
         };

         // 3. Create StatementNode (ForeignCall)
         let stmt = Statement::ForeignCall(sig.name.clone());
         let stmt_node = stmt.to_node(crate::ast::CodeMetaData::default());
         
         // 4. Create VmFunc
         if let Some(vm_func) = VmFunc::new_checked(param_type_id, return_type_id, stmt_node, &self.types) {
             let func_id = self.add_function(vm_func);
             // 5. Register in variables scope as a Function value
             use crate::vm::tree_walk::vm_value::VmValueSimplified;
             let val = VmValueSimplified::FuncId(func_id).into_vm_value_generalized(&mut self.types);
             // We insert into scope. `insert_variable_default` works on current scope.
             self.variables.insert_variable_default(Identifier::new(sig.name), val);
         }
    }
    
    fn get_type_id_by_name(&self, name: &str) -> Option<crate::types::TypeId> {
        let ident = crate::ast::Identifier::new(name.to_string());
        if let Ok(val) = self.variables.get_value_from_name(&ident, &self.types) {
            let simplified = val.into_simplified_value(&self.types);
             use crate::vm::tree_walk::vm_value::VmValueSimplified;
             if let VmValueSimplified::TypeId(tid) = simplified {
                 return Some(tid);
             }
        }
        None
    }

    fn load_builtin_types(&mut self) {
        use crate::types::CompTimeBuiltinType;

        let builtin_types = [
            ("int", CompTimeBuiltinType::Int),
            ("float", CompTimeBuiltinType::Float),
            ("bool", CompTimeBuiltinType::Bool),
        ];

        for (name, builtin_kind) in builtin_types {
            let type_def = UnifiedTypeDefinition::builtin(builtin_kind);
            let type_id = self.types.store_unified_type(type_def);
            self.variables.insert_variable_default(
                Identifier::new(name.to_string()),
                VmValueSimplified::TypeId(type_id).into_vm_value_generalized(&mut self.types),
            );
        }
    }
    pub fn extract_struct(&mut self, strct: StructValue) -> Result<(), VmErrorType> {
        for (k, v) in strct.value {
            self.insert_variable(k, v)?;
        }
        Ok(())
    }
    
    pub(super) fn insert_variable(
        &mut self,
        target: Identifier,
        value: VmValueGeneralized,
    ) -> Result<(), VmErrorType> {
        if self.variables.has_variable_current(&target) {
            return Err(VmErrorType::SameVariableName);
        }
        self.variables
            .insert_variable_default(target, value );
        Ok(())
    }
    pub fn execute_statement(&mut self, stat_node: &StatementNode) -> StatementResult {
        execution::execute_statement(self, stat_node)
    }
    pub fn evaluate_expr(&mut self, expr_node: &ExpressionNode) -> VmExprResult {
        evaluation::evaluate_expr(self, expr_node)
    }

    pub fn get_type_container(&self) -> &TypeContainer {
        &self.types
    }

    pub(crate) fn print_stack(&self) {
        println!("{}", self.variables);
    }

    pub fn insert_variable_check(
        &mut self,
        identifier: Identifier,
        value: VmValueGeneralized,
        expected_type_id: TypeId,
        type_container: &mut crate::types::TypeContainer,
    ) -> Result<(), VmErrorType> {
        self.variables
            .insert_variable_check(identifier, value, expected_type_id, type_container)
    }

    pub(super) fn run_block(
        &mut self,
        stmtblock: &StatmentBlockNode,
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
    fn execute_function(&mut self, func_id: FuncId, inputs: VmValueSimplified) -> VmExprResult {
        let func = self.get_func(func_id);
        let param_type = func.get_param();
        if !inputs.of_type(param_type, &mut self.types) {
            panic!("The validation should checked before");
        }
        let inputs = inputs.into_struct_value().unwrap();
        let body = self.get_func(func_id).get_statement() as *const StatementNode;
        self.create_scope();
        let result = (|| {
            self.extract_struct(inputs).unwrap();
            //TODO:Unsafe here is ok,as I am only mutating the new scope,So it guarentee's that
            //the const ptr isn't mutating elsewhere.
            match self.execute_statement(unsafe { body.as_ref().unwrap() })? {
                StatementEvent::Return(value) => Ok(value),
                _ => Ok(VmValueGeneralized::create_unit(&mut self.types)),
            }
        })();
        self.drop_scope();
        result
    }
    fn get_func(&self, func_id: FuncId) -> &VmFunc {
        self.funcs.get_checked(func_id).unwrap()
    }

    fn into_type(&mut self, input: VmValueGeneralized) -> Result<TypeId,VmErrorType > {
        let simplified = input.into_simplified_value(&self.types);
        ParsedValueType::into_type_id(simplified, &mut self.types)
            .ok_or(VmErrorType::InvalidTypeDefination)
    }


}

#[cfg(test)]
mod scope_stack_tests;
#[cfg(test)]
mod vm_tests;
