use std::sync::{Arc, Mutex};

use ef3r::ast::{Expr, Statement};
use ef3r::interpreter::interpret;
use ef3r::stdlib::{ef3r_stdlib, get_stdlib_functions};

#[test]
fn variable_assignment() {
    let context = Arc::new(Mutex::new(ef3r_stdlib()));

    let cloned_ctx = context.clone();

    let expression = Expr::Int(3).traced();
    let statement = Statement::Var("x".to_string(), expression.clone());

    interpret(context, &vec![statement.clone()]);

    assert_eq!(
        cloned_ctx
            .lock()
            .unwrap()
            .expression_context
            .variables
            .get("x")
            .unwrap()
            .evaluated,
        expression.evaluated
    );
}

#[test]
fn reassignment_of_statement() {
    let context = Arc::new(Mutex::new(ef3r_stdlib()));

    let cloned_ctx = context.clone();

    let statement1 = Statement::Var("x".to_string(), Expr::Int(2).traced());
    let statement2 = Statement::Var("x".to_string(), Expr::Int(3).traced());

    let evaluated = interpret(context, &vec![statement1, statement2]);

    assert_eq!(
        cloned_ctx
            .lock()
            .unwrap()
            .expression_context
            .variables
            .get("x")
            .unwrap()
            .evaluated,
        Expr::Int(3)
    );
}

#[test]
fn execute_example_program() {
    let program = r#"println("Hello, world!");

        let f = { x -> uppercase(x) };

        let y = f("test");

        launch {
            let x = "test";

            println(x.uppercase());
            println(x.f());

            println(2 + 2 * 3);

            println(2 / 2);

            println(y);
        };
    "#;

    let context = Arc::new(Mutex::new(ef3r_stdlib()));
    let mut parsed_program = ef3r::parser::parse(&program).unwrap();

    let context_lock = context.lock().unwrap();
    let stdlib_functions = get_stdlib_functions(&context_lock);

    parsed_program = ef3r::stdlib::resolve_builtin_functions(
        parsed_program,
        &stdlib_functions,
    );

    drop(context_lock);
    interpret(context, parsed_program.as_slice());
}
