use ef3r::{
    debugging::NoOpDebugger,
    executable::load_efrs_source,
    typechecking::{typecheck, TypingContext},
};

#[test]
fn test_illegal_op() {
    let src = r#"x = 2 + "3";"#.to_string();

    let (expr_context, statements) =
        load_efrs_source(NoOpDebugger {}, src).unwrap();

    let errors = typecheck(
        &expr_context.expression_context.read(),
        &mut TypingContext::new(),
        statements,
    );

    assert!(!errors.is_empty());
}

#[test]
fn test_legal_op() {
    let src = r#"x = 2 + 3;"#.to_string();

    let (expr_context, statements) =
        load_efrs_source(NoOpDebugger {}, src).unwrap();

    let errors = typecheck(
        &expr_context.expression_context.read(),
        &mut TypingContext::new(),
        statements,
    );

    assert!(errors.is_empty());
}

#[test]
fn test_complex_program() {
    let src = r#"
        x = 42;
        y = x + 10;
        z = { n -> n + y };
        result = z(5);"#
        .to_string();

    let (expr_context, statements) =
        load_efrs_source(NoOpDebugger {}, src).unwrap();

    let errors = typecheck(
        &expr_context.expression_context.read(),
        &mut TypingContext::new(),
        statements,
    );

    assert!(errors.is_empty());
}

#[test]
fn test_complex_program_with_error() {
    let src = r#"
        x = 42;
        y = x + "not a number";
        z = { n -> n + n };
        result = z(5);"#
        .to_string();

    let (expr_context, statements) =
        load_efrs_source(NoOpDebugger {}, src).unwrap();

    let errors = typecheck(
        &expr_context.expression_context.read(),
        &mut TypingContext::new(),
        statements,
    );

    assert!(!errors.is_empty());
}
