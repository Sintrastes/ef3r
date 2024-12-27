use std::sync::{Arc, Mutex};

use ef3r::ast::{Expr, RawExpr, Statement};
use ef3r::debugging::NoOpDebugger;
use ef3r::interpreter::interpret;
use ef3r::parser::CodeLocation;
use ef3r::stdlib::{
    ef3r_stdlib, get_stdlib_functions, get_stdlib_polymorphic_functions,
    resolve_builtin_functions,
};

#[test]
fn variable_assignment() {
    let context = Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new())));

    let cloned_ctx = context.clone();

    let expression = RawExpr::Int(3);
    let statement = Statement {
        location: CodeLocation {
            line: 0,
            column: 0,
            offset: 0,
        },
        var: Some("x".to_string()),
        expr: expression.clone(),
    };

    interpret(context, &vec![statement.clone()]).unwrap();

    assert_eq!(
        cloned_ctx
            .lock()
            .unwrap()
            .expression_context
            .variables
            .get("x")
            .unwrap()
            .evaluated,
        expression.from_raw()
    );
}

#[test]
fn reassignment_of_statement() {
    let context = Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new())));

    let cloned_ctx = context.clone();

    let statement1 = Statement {
        location: CodeLocation {
            line: 0,
            column: 0,
            offset: 0,
        },
        var: Some("x".to_string()),
        expr: RawExpr::Int(2),
    };
    let statement2 = Statement {
        location: CodeLocation {
            line: 0,
            column: 0,
            offset: 0,
        },
        var: Some("x".to_string()),
        expr: RawExpr::Int(3),
    };

    interpret(context, &vec![statement1, statement2]).unwrap();

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

        let concat = { xs -> fold(xs, "", { x, y -> x + y }) };

        let join_to_string = { strs, sep ->
            strs
                .intersperse(sep)
                .concat()
        };

        println(join_to_string(list("hello", "world"), ", "));

        let y = f("test");

        let node = new_node(Int, 0);

        let current_value = node.current_value();

        println(current_value);

        node.update_node(2);

        println(node.current_value());

        println(pair(1,2));

        // Test executing new nodes.
        new_node(Bool, true);
        new_node(String, "test");
        new_node(Type, Int);
        new_node(Float, 4.2);
        new_node(Unit, ());
        new_node(Pair(Int, Int), pair(2, 3));

        // Test list processing
        let xs = list(1, 2, 3);

        let ys = xs
            .map({ x -> x + 2 })
            .filter({ x -> x % 2 == 0 });

        // Test floating point arithmetic

        let foo = 3.2 * 5.6 + 0.2 - 0.01;

        /* Test launcing a thread. */
        launch {
            let x = "test";

            println("Hello " + x.uppercase());

            println(x.f());

            println(2 + 2 * 3);

            println(2 / 2);

            println(y);
        };
    "#;

    let context = Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new())));
    let mut parsed_program = ef3r::parser::parse(&program).unwrap();

    let context_lock = context.lock().unwrap();
    let stdlib_functions = get_stdlib_functions(&context_lock);
    let polymorphic_functions = get_stdlib_polymorphic_functions(&context_lock);

    parsed_program = ef3r::stdlib::resolve_builtin_functions(
        parsed_program,
        &polymorphic_functions,
        &stdlib_functions,
    );

    drop(context_lock);

    let result = interpret(context, parsed_program.as_slice());

    dbg!(&result);

    assert!(result.is_ok());
}

#[test]
fn pair_accessors_fail_on_nonpair() {
    let context = Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new())));

    let program = r#"
        let x = 42;
        x.first();
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

#[test]
fn arithmetic_operators_wrong_args() {
    let context = Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new())));

    let program = r#"
        let x = "test" + 42;
    "#;

    let parsed_program = ef3r::parser::parse(&program).unwrap();

    let result = interpret(context.clone(), parsed_program.as_slice());
    assert!(result.is_err());

    let program2 = r#"
        let y = "hello" * 3;
    "#;

    let parsed_program2 = ef3r::parser::parse(&program2).unwrap();
    let result2 = interpret(context.clone(), parsed_program2.as_slice());
    assert!(result2.is_err());

    let program3 = r#"
        let z = true / 2;
    "#;

    let parsed_program3 = ef3r::parser::parse(&program3).unwrap();
    let result3 = interpret(context, parsed_program3.as_slice());
    assert!(result3.is_err());
}
