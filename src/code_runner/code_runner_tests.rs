use crate::{
    code_runner::CodeRunner,
    vm::tree_walk::ValuePrimitive,
};

#[test]
fn test_basic_functionality() {
    let mut runner = CodeRunner::default();

    runner
        .run_statements(
            "let a = 15;
        let b = 3;",
        )
        .unwrap();
    let result = runner.evaluate_expr_simple("(a + b) * 2 - 5").unwrap();
    assert_eq!(result, ValuePrimitive::from_i64(31).into());

    let result = runner.evaluate_expr_simple("a > b").unwrap();
    assert_eq!(result, ValuePrimitive::from_bool(true).into());
}

#[test]
fn test_control_flow() {
    let mut runner = CodeRunner::default();

    runner.run_statements("let x = 0;if (2>1){x=42;}").unwrap();
    let result = runner.evaluate_expr_simple("x").unwrap();
    assert_eq!(result, ValuePrimitive::from_i64(42).into());
}
#[test]
fn test_scope() {
    let mut runner = CodeRunner::default();
    runner
        .run_statements("let y=0;let x=10;{let x=20;y=x;}")
        .unwrap();
    let x = runner.evaluate_expr_simple("x").unwrap();
    let y = runner.evaluate_expr_simple("y").unwrap();
    assert_eq!(x, ValuePrimitive::from_i64(10).into());
    assert_eq!(y, ValuePrimitive::from_i64(20).into());
}

#[test]
fn test_type_check() {
    let mut runner = CodeRunner::default();
    let output = runner.run_statements(
        r"let vec2={x=int,y=int};
let y={x=50,y=50};
let y3:{x=int,y=int}={x=50,y=50};
let y1:{x=int,y=bool}={x=50,y=50};
let y2:{x=int,y=int}={x=50,y=50};",
    );
    let yes = output.is_err_and(|x| x.is_runtime_error_and(|x| x.is_type_mismatch()));
    assert!(yes);
    runner
        .run_statements("let now_type={x=164,y=bool};")
        .unwrap();
}
#[test]
fn test_if() {
    let mut runner = CodeRunner::default();
    runner
        .run_statements(
            r"
        let x=10;
        if (x==10){x=20;}
    ",
        )
        .unwrap();
    let output = runner.evaluate_expr_simple("x").unwrap();
    assert_eq!(output, ValuePrimitive::from_i64(20).into())
}

#[test]
fn test_loop() {
    let mut runner = CodeRunner::default();
    runner
        .run_statements(
            r"
    let x=10;
    loop{
        x=x+1;
        if (x>30) {break;}
    }",
        )
        .unwrap();
    let output = runner.evaluate_expr_simple("x").unwrap();
    assert_eq!(output, ValuePrimitive::from_i64(31).into())
}
#[test]
fn test_function_call() {
    let mut runner = CodeRunner::default();
    runner
        .run_statements(
            r"let add = fn {x=int} -> int { return x*x; };
    let result = add!{x=5};
    ",
        )
        .unwrap();
    let output = runner.evaluate_expr_simple("result").unwrap();
    assert_eq!(output, ValuePrimitive::from_i64(25).into())
}

#[test]
fn test_reference_and_dereference() {
    let mut runner = CodeRunner::default();
    runner
        .run_statements(
            r"
    let x = 10;
    let r = &x;
    let y = *r;
    *r = 20;
    let z = x;
    ",
        )
        .unwrap();
    
    let y = runner.evaluate_expr_simple("y").unwrap();
    assert_eq!(y, ValuePrimitive::from_i64(10).into());
    
    let x = runner.evaluate_expr_simple("x").unwrap();
    assert_eq!(x, ValuePrimitive::from_i64(20).into());
    
    let z = runner.evaluate_expr_simple("z").unwrap();
    assert_eq!(z, ValuePrimitive::from_i64(20).into());
}

#[test]
fn test_struct_member_access() {
    let mut runner = CodeRunner::default();
    runner.run_statements(r"
        let p = {x=10, y=20};
        let x = p.x;
        let y = p.y;
    ").unwrap();
    assert_eq!(
        runner.evaluate_expr_simple("x").unwrap(),
        ValuePrimitive::from_i64(10).into()
    );
    assert_eq!(
        runner.evaluate_expr_simple("y").unwrap(),
        ValuePrimitive::from_i64(20).into()
    );
}

#[test]
fn test_reference_member_access() {
    let mut runner = CodeRunner::default();
    runner
        .run_statements(
            r"
        let p = {x=10, y=20};
        let r = &p;
        let rx_ref = r.x;
        *rx_ref = 30;
    ",
        )
        .unwrap();
    // Check p.x is 30.
    // To check p.x, we can just eval p.x (which returns value since p is struct)
    assert_eq!(
        runner.evaluate_expr_simple("p.x").unwrap(),
        ValuePrimitive::from_i64(30).into()
    );
}

#[test]
fn test_nested_struct_access() {
    let mut runner = CodeRunner::default();
    runner
        .run_statements(
            r"
        let p = {pos={x=10, y=20}, z=30};
        let x = p.pos.x;
        let r = &p;
        let ry_ref = r.pos.y;
        *ry_ref = 50;
    ",
        )
        .unwrap();
    assert_eq!(
        runner.evaluate_expr_simple("x").unwrap(),
        ValuePrimitive::from_i64(10).into()
    );
    assert_eq!(
        runner.evaluate_expr_simple("p.pos.y").unwrap(),
        ValuePrimitive::from_i64(50).into()
    );
}

