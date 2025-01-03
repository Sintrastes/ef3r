use std::sync::{Arc, Mutex};

use bimap::BiMap;
use ef3r::ast::expr::Expr;
use ef3r::ast::raw_expr::RawExpr;
use ef3r::ast::traced_expr::TracedExprRec;
use ef3r::ast::Statement;
use ef3r::debugging::NoOpDebugger;
use ef3r::executable::load_efrs_source;
use ef3r::interpreter::interpret;
use ef3r::stdlib::ef3r_stdlib;

#[test]
fn variable_assignment() {
    let context =
        Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new(), BiMap::new())));

    let cloned_ctx = context.clone();

    let expression = RawExpr::int(3);
    let statement = Statement {
        location: None,
        var: Some(0),
        expr: expression.clone(),
    };

    interpret(context, &vec![statement.clone()]).unwrap();

    assert_eq!(
        cloned_ctx
            .lock()
            .unwrap()
            .expression_context
            .variables
            .get(&0)
            .unwrap()
            .evaluated,
        expression.from_raw().evaluated
    );
}

#[test]
fn reassignment_of_statement() {
    let context =
        Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new(), BiMap::new())));

    let cloned_ctx = context.clone();

    let statement1 = Statement {
        location: None,
        var: Some(0),
        expr: RawExpr::int(2),
    };
    let statement2 = Statement {
        location: None,
        var: Some(0),
        expr: RawExpr::int(3),
    };

    interpret(context, &vec![statement1, statement2]).unwrap();

    assert_eq!(
        cloned_ctx
            .lock()
            .unwrap()
            .expression_context
            .variables
            .get(&0)
            .unwrap()
            .evaluated,
        TracedExprRec::Int(3)
    );
}

#[test]
fn execute_example_program() {
    let program = r#"print("Hello, world!");

        f = { x -> uppercase(x) };

        concat = { xs -> fold(xs, "", { x, y -> x + y }) };

        join_to_string = { strs, sep ->
            strs
                .intersperse(sep)
                .concat()
        };

        print(join_to_string(list("hello", "world"), ", "));

        y = f("test");

        node = reactive(Int, 0);

        current_value = node.current_value();

        print(current_value);

        node.update_node(2);

        print(node.current_value());

        print(pair(1,2));

        // Test executing new reactive variables.
        reactive(Bool, true);
        reactive(String, "test");
        reactive(Type, Int);
        reactive(Float, 4.2);
        reactive(Unit, ());
        reactive(Pair(Int, Int), pair(2, 3));

        // Test list processing
        xs = list(1, 2, 3);

        ys = xs
            .map { x -> x + 2 }
            .filter { x -> x % 2 == 0 };

        // Test floating point arithmetic

        foo = 3.2 * 5.6 + 0.2 - 0.01;

        // Test infix function syntax.

        op = { x, y -> 2 * x + y };

        applied = 2 `op` 3;

        /* Test launcing a thread. */
        launch {
            x = "test";

            print("Hello " + x.uppercase());

            print(x.f());

            print(2 + 2 * 3);

            print(2 / 2);

            print(y);
        };
    "#
    .to_string();

    let (context, program) =
        load_efrs_source(NoOpDebugger::new(), program).unwrap();
    let result = interpret(Arc::new(Mutex::new(context)), program.as_slice());

    dbg!(&result);

    assert!(result.is_ok());
}

#[test]
fn pair_accessors_fail_on_nonpair() {
    let program = r#"
        let x = 42;
        x.first();
    "#
    .to_string();

    let (context, program) =
        load_efrs_source(NoOpDebugger::new(), program).unwrap();
    let result = interpret(Arc::new(Mutex::new(context)), program.as_slice());

    assert!(result.is_err());

    let program2 = r#"
        second(42);
    "#
    .to_string();

    let (context, program2) =
        load_efrs_source(NoOpDebugger::new(), program2).unwrap();
    let result2 = interpret(Arc::new(Mutex::new(context)), program2.as_slice());
    assert!(result2.is_err());
}

#[test]
fn arithmetic_operators_wrong_args() {
    let program = r#"
        let x = "test" + 42;
    "#
    .to_string();

    let (context, program) =
        load_efrs_source(NoOpDebugger::new(), program).unwrap();
    let result = interpret(Arc::new(Mutex::new(context)), program.as_slice());
    assert!(result.is_err());

    let program = r#"
        let y = "hello" * 3;
    "#
    .to_string();

    let (context, program) =
        load_efrs_source(NoOpDebugger::new(), program).unwrap();
    let result = interpret(Arc::new(Mutex::new(context)), program.as_slice());
    assert!(result.is_err());

    let program = r#"
        let z = true / 2;
    "#
    .to_string();

    let (context, program) =
        load_efrs_source(NoOpDebugger::new(), program).unwrap();
    let result = interpret(Arc::new(Mutex::new(context)), program.as_slice());
    assert!(result.is_err());
}
