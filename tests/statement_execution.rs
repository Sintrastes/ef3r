use ef3r::ast::{Expr, Statement};
use ef3r::interpreter::interpret;
use ef3r::stdlib::ef3r_stdlib;

#[test]
fn variable_assignment() {
    let mut context = ef3r_stdlib();

    let expression = Expr::Int(3).traced();
    let statement = Statement::Var("x".to_string(), expression.clone());

    interpret(&mut context, &vec![statement.clone()]);

    assert_eq!(
        context
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
    let mut context = ef3r_stdlib();

    let statement1 = Statement::Var("x".to_string(), Expr::Int(2).traced());
    let statement2 = Statement::Var("x".to_string(), Expr::Int(3).traced());

    let evaluated = interpret(&mut context, &vec![statement1, statement2]);

    assert_eq!(
        context
            .expression_context
            .variables
            .get("x")
            .unwrap()
            .evaluated,
        Expr::Int(3)
    );
}
