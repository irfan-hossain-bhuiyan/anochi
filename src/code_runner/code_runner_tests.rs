
use crate::{vm::tree_walk::VmValue, code_runner::CodeRunner};

#[test]
fn test_basic_functionality() {
    let mut runner = CodeRunner::default();

    // Test basic arithmetic and variables
    runner.run_statements("let a = 15;
        let b = 3;").unwrap();
    //runner.run_statements("let b = 3;").unwrap();
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
    runner.run_statements("let x = 0;if (2>1){x=42;}").unwrap();
    //runner.run_statements("if (2 > 1) { x = 42; }").unwrap();
    let result = runner.evaluate_expr("x").unwrap();
    assert_eq!(result, VmValue::from_i64(42));
}
#[test]
fn test_scope() {
    let mut runner = CodeRunner::default();
    runner.run_statements("let y=0;let x=10;{let x=20;y=x;}").unwrap();
    //runner.run_statements("let x=0;").unwrap();
    //runner.run_statements("{let x=10;y=x;}").unwrap();
    let x = runner.evaluate_expr("x").unwrap();
    let y = runner.evaluate_expr("y").unwrap();
    assert_eq!(x, VmValue::from_i64(10));
    assert_eq!(y, VmValue::from_i64(20));
}
#[test]
fn test_type_check(){
    let mut runner = CodeRunner::default();
    let output=runner.run_statements(r"let vec2={x=i64,y=i64};
let y={x=50,y=50};
let y:{x=i64,y=i64}={x=50,y=50};
let y:{x=i64,y=bool}={x=50,y=50};
let y:{x=i64,y=i64}={x=50,y=50};");
    let yes=output.is_err_and(|x|x.is_runtime_error_and(|x|x.is_type_mismatch()));
    assert!(yes);
    runner.run_statements("let now_type={x=164,y=bool};").unwrap();
}
#[test]
fn test_loop(){
    let mut runner = CodeRunner::default();
    runner.run_statements(r"let x=10;loop{
        if (x>5) {break;}
        x=x+1;
        }").unwrap();
    let output=runner.evaluate_expr("x").unwrap();
    assert_eq!(output,VmValue::from_i64(51))
}
