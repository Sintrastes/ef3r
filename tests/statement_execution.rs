use std::sync::{Arc, Mutex};

use ef3r::ast::{Expr, Statement};
use ef3r::interpreter::interpret;
use ef3r::stdlib::{ef3r_stdlib, get_stdlib_functions};

#[test]
fn variable_assignment() {
    let context = Arc::new(Mutex::new(ef3r_stdlib()));

    let cloned_ctx = context.clone();

    let expression = Expr::Int(3);
    let statement = Statement {
        var: Some("x".to_string()),
        expr: expression.clone(),
    };

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
        expression
    );
}

#[test]
fn reassignment_of_statement() {
    let context = Arc::new(Mutex::new(ef3r_stdlib()));

    let cloned_ctx = context.clone();

    let statement1 = Statement {
        var: Some("x".to_string()),
        expr: Expr::Int(2),
    };
    let statement2 = Statement {
        var: Some("x".to_string()),
        expr: Expr::Int(3),
    };

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

        let node = new_node(Int, 0);

        let set_node = node.second();

        let current_value = node.first().current_value();

        println(current_value);

        node.first().set_node(2);

        println(node.first().current_value());

        println(pair(1,2));

        new_node(Bool, true);
        new_node(String, "test");
        new_node(Float, 4.2);
        new_node(Type, Int);
        new_node(Pair(Int, Int), pair(2, 3));

        launch {
            let x = "test";

            println("Hello " ++ x.uppercase());

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

#[test]
fn pair_accessors_fail_on_nonpair() {
    let context = Arc::new(Mutex::new(ef3r_stdlib()));

    let program = r#"
        first(42);
    "#;

    let parsed_program = ef3r::parser::parse(&program).unwrap();
    let result = interpret(context.clone(), parsed_program.as_slice());

    assert!(result.is_err());

    let program2 = r#"
        second(42);
    "#;

    let parsed_program2 = ef3r::parser::parse(&program2).unwrap();
    let result2 = interpret(context, parsed_program2.as_slice());
    assert!(result2.is_err());
}
