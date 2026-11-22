use std::sync::{Arc, Mutex};

use crust_backend::{Engine, value::Value};
use crust_frontend::typed::{HostFnDecl, Type};

fn engine_with_output() -> (Engine, Arc<Mutex<Vec<String>>>) {
    let lines = Arc::new(Mutex::new(Vec::new()));
    let sink = Arc::clone(&lines);
    let engine = Engine::with_output(move |line| sink.lock().unwrap().push(line.to_string()));
    (engine, lines)
}

fn output(lines: &Arc<Mutex<Vec<String>>>) -> Vec<String> {
    lines.lock().unwrap().clone()
}

#[test]
fn runs_arithmetic_and_prints() {
    let (engine, lines) = engine_with_output();
    let result = engine
        .run("fn main() -> void { let total = 2 + 3 * 4; print(\"total = {}\", total); }")
        .unwrap();

    assert_eq!(result, Value::Unit);
    assert_eq!(output(&lines), vec!["total = 14"]);
}

#[test]
fn runs_float_arithmetic() {
    let (engine, lines) = engine_with_output();
    engine
        .run("fn main() -> void { let x = 1.5 + 2.25; print(\"{}\", x); }")
        .unwrap();

    assert_eq!(output(&lines), vec!["3.75"]);
}

#[test]
fn runs_for_loop() {
    let (engine, lines) = engine_with_output();
    engine
        .run("fn main() -> void { for (let i = 0; i < 3; i++) { print(\"i = {}\", i); } }")
        .unwrap();

    assert_eq!(output(&lines), vec!["i = 0", "i = 1", "i = 2"]);
}

#[test]
fn runs_recursive_functions() {
    let (engine, lines) = engine_with_output();
    let source = r#"
        fn factorial(n: i64) -> i64 {
            if n == 0 { return 1; }
            return n * factorial(n - 1);
        }
        fn main() -> void { print("{}", factorial(5)); }
    "#;
    engine.run(source).unwrap();

    assert_eq!(output(&lines), vec!["120"]);
}

#[test]
fn calls_custom_host_functions() {
    let (mut engine, lines) = engine_with_output();
    engine
        .register_host(
            HostFnDecl {
                name: "double".to_string(),
                params: vec![Type::I64],
                variadic: false,
                return_type: Type::I64,
            },
            |args| match &args[0] {
                Value::Int(value) => Value::Int(value * 2),
                other => panic!("expected int, got {other:?}"),
            },
        )
        .unwrap();

    engine
        .run("fn main() -> void { print(\"{}\", double(21)); }")
        .unwrap();

    assert_eq!(output(&lines), vec!["42"]);
}

#[test]
fn rejects_type_errors() {
    let (engine, _) = engine_with_output();
    let error = engine
        .run("fn main() -> void { let count: i64 = 3.14; }")
        .unwrap_err();

    assert!(matches!(error, crust_backend::engine::Error::Type(_)));
}

#[test]
fn jit_compiles_hot_integer_functions() {
    let (mut engine, _) = engine_with_output();
    engine.set_jit_threshold(1);
    let module = engine
        .compile("fn triple(n: i64) -> i64 { return n * 3; } fn main() -> void {}")
        .unwrap();

    for n in 0..8 {
        assert_eq!(
            engine.call(&module, "triple", &[Value::Int(n)]).unwrap(),
            Value::Int(n * 3)
        );
    }
}

#[test]
fn jit_recursion_matches_interpreter() {
    let source = r#"
        fn fib(n: i64) -> i64 {
            if n < 2 { return n; }
            return fib(n - 1) + fib(n - 2);
        }
        fn main() -> void {}
    "#;

    let (mut interpreted, _) = engine_with_output();
    interpreted.set_jit_threshold(u32::MAX);
    let module = interpreted.compile(source).unwrap();

    let (mut jitted, _) = engine_with_output();
    jitted.set_jit_threshold(1);
    let jit_module = jitted.compile(source).unwrap();

    for n in [5, 10, 20] {
        let args = [Value::Int(n)];
        let expected = interpreted.call(&module, "fib", &args).unwrap();
        assert_eq!(jitted.call(&jit_module, "fib", &args).unwrap(), expected);
    }
}

#[test]
fn jit_compiles_hot_float_functions() {
    let (mut engine, _) = engine_with_output();
    engine.set_jit_threshold(1);
    let module = engine
        .compile("fn half(x: f64) -> f64 { return x / 2.0; } fn main() -> void {}")
        .unwrap();

    for _ in 0..6 {
        assert_eq!(
            engine.call(&module, "half", &[Value::Float(9.0)]).unwrap(),
            Value::Float(4.5)
        );
    }
}

#[test]
fn jit_compiles_hot_loops() {
    let (mut engine, _) = engine_with_output();
    engine.set_jit_threshold(1);
    let module = engine
        .compile(
            "fn countdown(n: i64) -> i64 { for (let i = n; i > 0; i--) {} return n; } fn main() -> void {}",
        )
        .unwrap();

    assert_eq!(
        engine.call(&module, "countdown", &[Value::Int(5)]).unwrap(),
        Value::Int(5)
    );
}

#[test]
fn disassembles_register_instructions() {
    let (engine, _) = engine_with_output();
    let ir = engine
        .disassemble("fn main() -> void { let x = 1; print(\"{}\", x); }")
        .unwrap();

    assert!(ir.contains("LOAD_CONST"));
    assert!(ir.contains("CALL"));
    assert!(ir.contains("fn main"));
}
