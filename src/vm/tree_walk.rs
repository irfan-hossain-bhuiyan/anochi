//! Virtual Machine for the Anochi programming language.

use std::{collections::HashMap, fmt::Display};
mod vm_value_operation;
use crate::{
    ast::{
        BinaryOperator, Expression, ExpressionNode, Identifier, Literal, Statement, StatementNode,
        UnaryOperator,
    },
    vm::backend::{IoBackend, VmBackend},
};

use thiserror::Error;

use num_bigint::BigInt;
use num_rational::BigRational;

/// Primitive values that can be stored in the VM
#[derive(Debug, Clone, PartialEq)]
pub enum ValuePrimitive {
    Bool(bool),
    Integer(BigInt),
    Float(BigRational),
}

impl Display for ValuePrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Integer(i) => write!(f, "{i}"),
            Self::Float(fl) => write!(f, "{fl}"),
        }
    }
}

impl From<Literal> for ValuePrimitive {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Bool(b) => Self::Bool(b),
            Literal::Integer(i) => Self::Integer(i),
            Literal::Float(f) => Self::Float(f),
            Literal::String(_) => panic!("String literals should be handled as arrays, not primitives"),
            Literal::Identifier(_) => panic!("Identifiers should be resolved before conversion to primitive"),
        }
    }
}

/// Error types for VM evaluation.
#[derive(Error, Debug, PartialEq)]
pub enum VmError {
    /// Division by zero error
    #[error("Division by zero")]
    DivisionByZero,
    /// Type mismatch error
    #[error("Type mismatch: cannot perform operation on different types")]
    TypeMismatch,
    /// Undefined identifier error
    #[error("Undefined identifier: {0}")]
    UndefinedIdentifier(String),
    /// Invalid operation error
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
    #[error("Unsupproted Operation {0:?}")]
    UnsupportedOperation(String)
}
#[derive(Debug,Clone,PartialEq)]
pub enum VmValue{
    ValuePrimitive(ValuePrimitive),
    Product(HashMap<Identifier,VmValue>),
   Type(crate::typing::TypeId),
}
impl Display for VmValue{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            Self::ValuePrimitive(x)=>Display::fmt(x, f),
            Self::Product(x)=>write!(f,"Product{x:?}"),
           Self::Type(_)=>write!(f,"Type"),
        }
    }
}

impl From<Literal> for VmValue {
    fn from(v: Literal) -> Self {
        match v {
            Literal::String(_) => panic!("String literals should be handled as arrays, not primitives"),
            Literal::Identifier(_) => panic!("Identifiers should be resolved before conversion"),
            _ => Self::ValuePrimitive(ValuePrimitive::from(v)),
        }
    }
}

impl VmValue {
    /// Create VmValue from i64
    pub fn from_i64(value: i64) -> Self {
        Self::ValuePrimitive(ValuePrimitive::Integer(BigInt::from(value)))
    }
    
    /// Create VmValue from f64
    pub fn from_f64(value: f64) -> Self {
        let rational = BigRational::from_float(value)
            .unwrap_or_else(|| BigRational::from(BigInt::from(0)));
        Self::ValuePrimitive(ValuePrimitive::Float(rational))
    }
    
    /// Create VmValue from bool
    pub fn from_bool(value: bool) -> Self {
        Self::ValuePrimitive(ValuePrimitive::Bool(value))
    }
    
    /// Create VmValue from BigInt
    pub fn from_bigint(value: BigInt) -> Self {
        Self::ValuePrimitive(ValuePrimitive::Integer(value))
    }
    
    /// Create VmValue from BigRational
    pub fn from_bigrational(value: BigRational) -> Self {
        Self::ValuePrimitive(ValuePrimitive::Float(value))
    }
}

/// Result type for VM evaluation operations.
pub type VmResult = Result<VmValue, VmError>;
#[derive(Debug, Default)]
pub struct Vm<Backend= IoBackend> {
    variable: HashMap<Identifier, VmValue>,
   types: crate::typing::TypeContainer,
    backend: Backend,
}
type ExpNode<'a> = ExpressionNode<'a>;
type StmtNode<'a> = StatementNode<'a>;

impl<Backend: VmBackend> Vm<Backend> {
    pub fn new(backend: Backend) -> Self {
        let mut vm = Self {
            variable: HashMap::new(),
           types: crate::typing::TypeContainer::new(),
            backend,
        };
        vm.load_builtin_types();
        vm
    }
   
   fn load_builtin_types(&mut self) {
       use crate::typing::{TypeDefinition, BuiltinKind};
       
       let builtin_types = [
           ("i64", BuiltinKind::I64),
           ("f64", BuiltinKind::F64),
           ("bool", BuiltinKind::Bool),
           ("usize", BuiltinKind::Usize),
       ];
       
       for (name, builtin_kind) in builtin_types {
           let type_def = TypeDefinition::Builtin(builtin_kind);
           let unified = type_def.to_unified();
           let optimized = unified.to_optimized(&mut self.types);
           let type_id = self.types.store_type(optimized);
           self.variable.insert(name.to_string(), VmValue::Type(type_id));
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
                            .variable
                            .get(x)
                            .cloned()
                            .ok_or(VmError::UndefinedIdentifier(format!("{x} doesn't exist."))),
                        Literal::Bool(_) | Literal::Float(_) | Literal::Integer(_) => {
                            Ok(VmValue::ValuePrimitive(ValuePrimitive::from(literal.clone())))
                        }
                        Literal::String(_) => {
                            // TODO: Handle strings as arrays when array implementation is ready
                            Err(VmError::UnsupportedOperation("String literals not yet supported as arrays".to_string()))
                        }
                    },
            Expression::Binary {
                        left,
                        operator,
                        right,
                    } => {
                        let left_val = self.evaluate_expr(left)?;
                        let right_val = self.evaluate_expr(right)?;

                        vm_value_operation::evaluate_binary_op(&left_val, operator, &right_val)
                    }
            Expression::Unary { operator, operand } => {
                        let operand_val = self.evaluate_expr(operand)?;
                        vm_value_operation::evaluate_unary_op(operator, &operand_val)
                    }
            Expression::Grouping { expression } => self.evaluate_expr(expression),
            Expression::Product { data } => {
                        let mut product=HashMap::new();
                        for (key,value) in data.iter(){
                            product.insert(key.to_string(),self.evaluate_expr(value)?);
                        }
                        Ok(VmValue::Product(product))
                    },
           Expression::Sum { data } => {
               use std::collections::BTreeSet;
               
               let mut type_set = BTreeSet::new();
               for expr in data.iter() {
                   match self.evaluate_expr(expr)? {
                       VmValue::Type(type_id) => {
                           type_set.insert(type_id);
                       }
                       _ => return Err(VmError::InvalidOperation("Sum types can only contain type values".to_string())),
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
                       return Err(VmError::InvalidOperation("Type not found in container".to_string()));
                   }
               }
               
               let unified = crate::typing::UnifiedTypeDefinition::Sum { variants };
               let optimized = unified.to_optimized(&mut self.types);
               let type_id = self.types.store_type(optimized);
               Ok(VmValue::Type(type_id))
           },
            Expression::MemberAccess { object: _, member: _ } => todo!(),
        }
    }
    pub fn execute_statement(&mut self, stat_node: &StmtNode) -> Result<(), VmError> {
        let stmt = &stat_node.node;
        match stmt {
            Statement::Assignment { target, value, r#type: _ } => {
                // For now, only handle simple identifier assignments
                // TODO: Add support for member access assignments later
                match &target.node {
                    Expression::Literal(Literal::Identifier(identifier)) => {
                        let evaluated_value = self.evaluate_expr(value)?;
                        self.variable.insert(identifier.clone(), evaluated_value);
                        Ok(())
                    }
                    _ => {
                        // For member access and other complex assignments, 
                        // return an error for now until type system is implemented
                        Err(VmError::UnsupportedOperation("Complex assignment not yet supported".to_string()))
                    }
                }
            }
            Statement::StatementBlock { statements } => {
                for stmt in statements.iter() {
                    self.execute_statement(stmt)?;
                }
                Ok(())
            }
            Statement::If { condition, on_true } => {
                let VmValue::ValuePrimitive(ValuePrimitive::Bool(x)) = self.evaluate_expr(condition)? else {
                    return Err(VmError::TypeMismatch);
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
                let VmValue::ValuePrimitive(ValuePrimitive::Bool(x)) = self.evaluate_expr(condition)? else {
                    return Err(VmError::TypeMismatch);
                };
                if x {
                    self.execute_statement(on_true)?;
                } else {
                    self.execute_statement(on_false)?;
                }
                Ok(())
            }
            Statement::Debug { expr_vec }=>{
                for expr in expr_vec.iter(){
                    let expr=self.evaluate_expr(expr)?;
                    self.backend.debug_print(&expr.to_string()).unwrap();
                }
                Ok(())
            }   
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AstNode, BinaryOperator, Expression, Statement};

    #[test]
    fn test_vm_comprehensive_operations() {
        let mut vm: Vm = Vm::default();

        // Test integer literal
        let integer = AstNode::new_temp(Expression::from_i64(42));
        let result = vm.evaluate_expr(&integer).unwrap();
        assert_eq!(result, VmValue::ValuePrimitive(ValuePrimitive::Integer(BigInt::from(42))));


        
        // Test boolean operations: true && false
        let bool_expr = Expression::binary(Expression::from_bool(true), BinaryOperator::And, Expression::from_bool(false));
        let bool_node = AstNode::new_temp(bool_expr);
        let result = vm.evaluate_expr(&bool_node).unwrap();
        assert_eq!(result, VmValue::ValuePrimitive(ValuePrimitive::Bool(false)));

    }




    #[test]
    fn test_debug_statement_single_value() {
         use crate::vm::backend::TestBackend;
         let backend = TestBackend::new();
         let mut vm: Vm<TestBackend> = Vm::new(backend);
         
         // Create a debug statement with a single integer expression
           let expr = AstNode::new_temp(Expression::from_i64(42));
         let debug_stmt = AstNode::new_temp(Statement::debug(vec![expr]));
         
         // Execute the debug statement
         vm.execute_statement(&debug_stmt).unwrap();
         
         // Verify the debug output
         let debug_output = vm.backend.get_debug_output();
         assert_eq!(debug_output, "42");
     }




}
