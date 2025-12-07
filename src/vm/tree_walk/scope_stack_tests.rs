    use crate::{ast::Identifier, types::TypeContainer, vm::tree_walk::{ScopeStack, ValuePrimitive, VmErrorType, VmValue}};
    use num_bigint::BigInt;

    /// Test basic scope operations: create variable, create scope, access from inner scope
    #[test]
    fn test_scope_variable_access_across_scopes() {
        let mut stack = ScopeStack::new();
        let mut type_container=TypeContainer::default();
        let var_name = Identifier::new("test_var".to_string());
        let value = VmValue::ValuePrimitive(ValuePrimitive::Integer(BigInt::from(42)));

        // Set variable in global scope
        stack.insert_variable(var_name.clone(), value.clone(),&mut type_container);
        
        // Create new scope
        stack.create_scope();
        
        // Should be able to access variable from outer scope
        let retrieved = stack.get_variable(&var_name);
        assert!(retrieved.is_some());
        assert_eq!(*retrieved.unwrap(), value);
        
        // Drop scope and verify variable still accessible in global scope
        stack.drop_scope();
        let retrieved = stack.get_variable(&var_name);
        assert!(retrieved.is_some());
        assert_eq!(*retrieved.unwrap(), value);
    }

    /// Test mutable access and modification of variables across scopes
    #[test]
    fn test_mutable_variable_access_and_modification() {
        let mut stack = ScopeStack::new();
        let mut type_container=TypeContainer::default();
        let var_name = Identifier::new("mutable_var".to_string());
        let initial_value = VmValue::ValuePrimitive(ValuePrimitive::Integer(BigInt::from(10)));
        let modified_value = VmValue::ValuePrimitive(ValuePrimitive::Integer(BigInt::from(20)));

        // Set variable in global scope
        stack.insert_variable(var_name.clone(), initial_value.clone(),&mut type_container);
        
        // Create new scope
        stack.create_scope();
        
        // Get mutable reference and modify from inner scope
        {
            let mutable_ref = stack.get_variable_mut(&var_name);
            assert!(mutable_ref.is_some());
            *mutable_ref.unwrap() = modified_value.clone();
        }
        
        // Verify modification is visible
        let retrieved = stack.get_variable(&var_name);
        assert_eq!(*retrieved.unwrap(), modified_value);
        
        // Drop scope and verify modification persisted in global scope
        stack.drop_scope();
        let retrieved = stack.get_variable(&var_name);
        assert_eq!(*retrieved.unwrap(), modified_value);
    }

    /// Test variable shadowing: inner scope variable hides outer scope variable
    #[test]
    fn test_variable_shadowing() {
        let mut stack = ScopeStack::new();
        let var_name = Identifier::new("shadowed_var".to_string());
        let outer_value = VmValue::ValuePrimitive(ValuePrimitive::Bool(true));
        let inner_value = VmValue::ValuePrimitive(ValuePrimitive::Bool(false));

        // Set variable in global scope
        stack.insert_variable(var_name.clone(), outer_value.clone());
        
        // Create new scope and set variable with same name
        stack.create_scope();
        stack.insert_variable(var_name.clone(), inner_value.clone());
        
        // Should access inner scope variable (shadowing)
        let retrieved = stack.get_variable(&var_name);
        assert_eq!(*retrieved.unwrap(), inner_value);
        
        // Drop inner scope, should access outer scope variable again
        stack.drop_scope();
        let retrieved = stack.get_variable(&var_name);
        assert_eq!(*retrieved.unwrap(), outer_value);
    }

    /// Test error handling for undefined variables
    #[test]
    fn test_undefined_variable_access() {
        let stack = ScopeStack::new();
        let nonexistent_var = Identifier::new("nonexistent".to_string());
        
        // Should return None for undefined variable
        assert!(stack.get_variable(&nonexistent_var).is_none());
        
        // Test error method
        let result = stack.get_value_or_err(&nonexistent_var);
        assert!(matches!(result, Err(VmErrorType::UndefinedIdentifier(_))));
    }

    /// Test mutable access error handling for undefined variables
    #[test]
    fn test_undefined_variable_mutable_access() {
        let mut stack = ScopeStack::new();
        let nonexistent_var = Identifier::new("nonexistent".to_string());
        
        // Should return None for undefined variable
        assert!(stack.get_variable_mut(&nonexistent_var).is_none());
        
        // Test error method
        let result = stack.get_variable_mut_or_err(&nonexistent_var);
        assert!(matches!(result, Err(VmErrorType::UndefinedIdentifier(_))));
    }

    /// Test has_variable method across multiple scopes
    #[test]
    fn test_has_variable_across_scopes() {
        let mut stack = ScopeStack::new();
        let var1 = Identifier::new("var1".to_string());
        let var2 = Identifier::new("var2".to_string());
        let value = VmValue::ValuePrimitive(ValuePrimitive::Integer(BigInt::from(1)));

        // Initially no variables
        assert!(!stack.has_variable(&var1));
        assert!(!stack.has_variable(&var2));

        // Set variable in global scope
        stack.insert_variable(var1.clone(), value.clone());
        assert!(stack.has_variable(&var1));
        assert!(!stack.has_variable(&var2));

        // Create scope and add variable
        stack.create_scope();
        stack.insert_variable(var2.clone(), value.clone());
        assert!(stack.has_variable(&var1)); // From outer scope
        assert!(stack.has_variable(&var2)); // From current scope

        // Drop scope, var2 should no longer exist
        stack.drop_scope();
        assert!(stack.has_variable(&var1));
        assert!(!stack.has_variable(&var2));
    }

    /// Test nested scope operations with multiple variables
    #[test]
    fn test_nested_scopes_complex() {
        let mut stack = ScopeStack::new();
        let global_var = Identifier::new("global".to_string());
        let scope1_var = Identifier::new("scope1".to_string());
        let scope2_var = Identifier::new("scope2".to_string());
        
        let global_value = VmValue::ValuePrimitive(ValuePrimitive::Integer(BigInt::from(100)));
        let scope1_value = VmValue::ValuePrimitive(ValuePrimitive::Integer(BigInt::from(200)));
        let scope2_value = VmValue::ValuePrimitive(ValuePrimitive::Integer(BigInt::from(300)));

        // Set global variable
        stack.insert_variable(global_var.clone(), global_value.clone());

        // Create first nested scope
        stack.create_scope();
        stack.insert_variable(scope1_var.clone(), scope1_value.clone());
        
        // Create second nested scope
        stack.create_scope();
        stack.insert_variable(scope2_var.clone(), scope2_value.clone());
        
        // All variables should be accessible from deepest scope
        assert!(stack.has_variable(&global_var));
        assert!(stack.has_variable(&scope1_var));
        assert!(stack.has_variable(&scope2_var));
        
        // Drop second scope
        stack.drop_scope();
        assert!(stack.has_variable(&global_var));
        assert!(stack.has_variable(&scope1_var));
        assert!(!stack.has_variable(&scope2_var));
        
        // Drop first scope
        stack.drop_scope();
        assert!(stack.has_variable(&global_var));
        assert!(!stack.has_variable(&scope1_var));
        assert!(!stack.has_variable(&scope2_var));
    }
