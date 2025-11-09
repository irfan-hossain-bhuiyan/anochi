use std::error::Error;

use crate::{vm::tree_walk::{VmError, VmValue}, CodeRunner};

#[test]
fn test_basic_functionality() {
    let mut runner = CodeRunner::default();

    // Test basic arithmetic and variables
    runner.run_statement("let a = 15;").unwrap();
    runner.run_statement("let b = 3;").unwrap();
    let result = runner.evaluate_expr("(a + b) * 2 - 5").unwrap();
    assert_eq!(result, VmValue::from_i64(31));

    // Test boolean operations
    let result = runner.evaluate_expr("a > b").unwrap();
    assert_eq!(result, VmValue::from_bool(true));
}

#[test]
fn test_control_flow() {
    let mut runner = CodeRunner::default();

    // Test if statement
    runner.run_statement("let x = 0;").unwrap();
    runner.run_statement("if (2 > 1) { x = 42; }").unwrap();
    let result = runner.evaluate_expr("x").unwrap();
    assert_eq!(result, VmValue::from_i64(42));
}
#[test]
fn test_scope() {
    let mut runner = CodeRunner::default();
    runner.run_statement("let y=0;").unwrap();
    runner.run_statement("let x=0;").unwrap();
    runner.run_statement("{let x=10;y=x;}").unwrap();
    let x = runner.evaluate_expr("x").unwrap();
    let y = runner.evaluate_expr("y").unwrap();
    assert_eq!(x, VmValue::from_i64(0));
    assert_eq!(y, VmValue::from_i64(10));
}
#[test]
fn test_type_check(){
    let mut runner = CodeRunner::default();
    runner.run_statement("let y:{x=i64,y=i64}={x=50,y=50};").unwrap();
    let output=runner.run_statement("let y:{x=i64,y=bool}={x=50,y=50};");
    assert!(output.is_err())


}
