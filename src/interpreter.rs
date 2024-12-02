use std::collections::HashMap;

use daggy::Dag;

use crate::{
    ast::{ActionID, Expr, FunctionID, Statement, TracedExpr, VariableID},
    frp::Node,
};

///
/// Local state needed to evaluate expressions and
///  update the FRP event loop.
///
#[derive(Clone)]
pub struct Context {
    pub expression_context: ExpressionContext,
    pub graph: Dag<Node<TracedExpr>, (), u32>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum EvaluationError {
    TypeError,
    NotAFunction,
    VariableNotFound,
    Unimplemented,
    WrongNumberOfArguments,
}

#[derive(Clone)]
pub struct InvokableDefinition {
    pub definition:
        fn(&mut Context, &[TracedExpr]) -> Result<Expr, EvaluationError>,
    pub name: String,
    pub infix: bool,
}

#[derive(Clone)]
pub struct ExpressionContext {
    pub functions: HashMap<FunctionID, InvokableDefinition>,
    pub actions: HashMap<ActionID, InvokableDefinition>,
    pub variables: HashMap<VariableID, TracedExpr>,
}

/// Ensure an expression is being traced before we evaluate it.
pub fn trace(expr: TracedExpr) -> TracedExpr {
    match expr {
        TracedExpr {
            evaluated: expr,
            stored_trace: _,
        } => TracedExpr {
            evaluated: expr.clone(),
            stored_trace: Option::Some(expr),
        },
    }
}

pub fn unwind_trace(expr: TracedExpr) -> TracedExpr {
    match expr {
        TracedExpr {
            evaluated,
            stored_trace,
        } => {
            let actual_trace = stored_trace.unwrap_or(evaluated);
            TracedExpr {
                evaluated: match actual_trace {
                    Expr::Apply(function, arguments) => Expr::Apply(
                        Box::new(unwind_trace(*function)),
                        arguments
                            .iter()
                            .map(|x| unwind_trace(x.to_owned()))
                            .collect(),
                    ),
                    _ => actual_trace,
                },
                stored_trace: None,
            }
        }
    }
}

// Apply an expression to a list of arguments in a traced manner.
pub fn apply_traced(
    ctx: &mut Context,
    expr: TracedExpr,
    args: &[TracedExpr],
) -> Result<TracedExpr, EvaluationError> {
    let evaluated = evaluate(
        ctx,
        Expr::Apply(
            Box::new(expr.evaluated.clone().traced()),
            args.iter().map(|x| x.evaluated.clone().traced()).collect(),
        ),
    );

    let trace = Expr::Apply(
        Box::new(expr.clone()),
        args.iter().map(|x| x.get_trace().traced()).collect(),
    );

    Ok(TracedExpr {
        evaluated: evaluated?.evaluated,
        stored_trace: Some(trace),
    })
}

pub fn evaluate_traced(
    ctx: &mut Context,
    expr: TracedExpr,
) -> Result<TracedExpr, EvaluationError> {
    match expr {
        TracedExpr {
            evaluated,
            stored_trace,
        } => Ok(TracedExpr {
            evaluated: evaluate_traced_rec(
                ctx,
                evaluated.clone(),
                stored_trace.clone(),
            )?
            .evaluated,
            stored_trace: Some(stored_trace.unwrap_or(evaluated)),
        }),
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Expr, interpreter::evaluate, stdlib::ef3r_stdlib};

    quickcheck! {
        fn evaluation_is_idempotent(expr: Expr) -> bool {
            let context = ef3r_stdlib();
            println!("Evaluating: {}", expr.clone());
            let evaluated = evaluate(&mut context, expr);
            match evaluated {
                Err(_) => true, // Property does not apply if expression is malformed.
                Ok(inner) => Ok(inner.clone()) == evaluate(&mut context, inner.evaluated),
            }
        }
    }
}

pub fn evaluate(
    ctx: &mut Context,
    expr: Expr,
) -> Result<TracedExpr, EvaluationError> {
    evaluate_traced_rec(ctx, expr, None)
}

fn evaluate_traced_rec(
    ctx: &mut Context,
    expr: Expr,
    trace: Option<Expr>,
) -> Result<TracedExpr, EvaluationError> {
    match expr {
        // Literals evaluate to themselves.
        Expr::None => Ok(expr.traced()),
        Expr::Unit => Ok(expr.traced()),
        Expr::Int(_) => Ok(expr.traced()),
        Expr::Bool(_) => Ok(expr.traced()),
        Expr::String(_) => Ok(expr.traced()),
        Expr::Float(_) => Ok(expr.traced()),
        Expr::Action(_) => Ok(expr.traced()),
        Expr::Pair(_, _) => Ok(expr.traced()),
        Expr::Type(_) => Ok(expr.traced()),
        Expr::Lambda(_, _, _) => Ok(expr.traced()),
        Expr::BuiltinFunction(_) => Ok(expr.traced()),
        Expr::Node(_) => Ok(expr.traced()),
        // For now, we'll just treat this like an opaque expression.
        // Evaluating it is different from a normal expression since we'd need
        // access to the FRP graph.
        Expr::MapNode(_, _) => Ok(expr.traced()),
        // Function applications need to be reduced.
        Expr::Apply(f, xs) => match f.evaluated {
            // Builtin functions can just be looked up in the evaluator context.
            Expr::BuiltinFunction(id) => {
                let implemntation = ctx
                    .expression_context
                    .functions
                    .get(&id)
                    .ok_or(EvaluationError::NotAFunction)?
                    .definition;

                let evaluated_args: Result<Vec<TracedExpr>, _> = xs
                    .iter()
                    .map(|x| {
                        evaluate_traced_rec(
                            ctx,
                            x.evaluated.clone(),
                            Some(x.get_trace()),
                        )
                    })
                    .into_iter()
                    .collect();

                Ok(TracedExpr::build(
                    implemntation(ctx, evaluated_args?.as_mut_slice())?,
                    trace,
                ))
            }
            Expr::Lambda(_, _, _) => Err(EvaluationError::Unimplemented)?,
            // If it's an application itself, try to evaluate it first.
            Expr::Apply(_, _) => {
                println!("Evaluating apply on {:?} and {:?}", f, xs);
                let f_evaluated = evaluate(ctx, f.evaluated)?;

                evaluate(ctx, Expr::Apply(Box::new(f_evaluated), xs))
            }
            // If it's a variable, see if we can look it up in the context.
            Expr::Var(var) => evaluate(
                ctx,
                ctx.expression_context
                    .variables
                    .get(&var)
                    .ok_or(EvaluationError::VariableNotFound)?
                    .evaluated
                    .clone(),
            ),
            _ => Err(EvaluationError::NotAFunction)?,
        },
        // Variables are looked up in the current context.
        Expr::Var(x) => Ok(ctx
            .expression_context
            .variables
            .get(&x)
            .ok_or(EvaluationError::VariableNotFound)?
            .clone()),
    }
}

pub fn interpret(ctx: &mut Context, statements: &[Statement]) {
    for statement in statements {
        match statement {
            Statement::Var(x, expr) => {
                let evaluated = evaluate_traced(ctx, expr.clone()).unwrap();

                ctx.expression_context
                    .variables
                    .insert(x.to_string(), evaluated);
            }
            Statement::Execute(result_var, action_expr) => {
                match &action_expr.evaluated {
                    Expr::Apply(action, args) => {
                        let action = action;
                        match action.evaluated {
                            Expr::Action(action_id) => {
                                let action = ctx.expression_context.actions
                                    [&action_id]
                                    .definition;

                                let evaluated_args: Result<Vec<TracedExpr>, _> =
                                    args.into_iter()
                                        .map(|arg| {
                                            evaluate_traced(ctx, arg.clone())
                                        })
                                        .collect();

                                let result = TracedExpr::build(
                                    (action)(
                                        ctx,
                                        &evaluated_args.unwrap().as_mut_slice(),
                                    )
                                    .unwrap(),
                                    Some(action_expr.get_trace()),
                                );

                                match result_var {
                                    Some(result_var) => {
                                        ctx.expression_context
                                            .variables
                                            .insert(result_var.clone(), result);
                                    }
                                    _ => {}
                                }
                            }
                            _ => Err(EvaluationError::NotAFunction).unwrap(),
                        }
                    }
                    _ => Err(EvaluationError::NotAFunction).unwrap(),
                }
            }
        };
    }
}
