use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use daggy::Dag;

use crate::{
    ast::{substitute, Expr, FunctionID, Statement, TracedExpr, VariableID},
    frp::{with_lock, Node},
    types::ExprType,
};

///
/// Local state needed to evaluate expressions and
///  update the FRP event loop.
///
#[derive(Clone)]
pub struct Context<'a>
where
    'a: 'static,
{
    pub expression_context: ExpressionContext,
    pub graph: Dag<Node<'a>, (), u32>,
}

unsafe impl<'a> Send for Context<'a> {}

#[derive(PartialEq, Debug)]
pub enum EvaluationError {
    TypeError {
        expected: ExprType,
        actual: ExprType,
        at_loc: String,
    },
    NotAFunction(Expr),
    VariableNotFound(String),
    Unimplemented,
    WrongNumberOfArguments {
        expected: usize,
        actual: usize,
        for_function: String,
    },
}

#[derive(Clone)]
pub struct InvokableDefinition {
    pub definition:
        fn(Arc<Mutex<Context>>, &[TracedExpr]) -> Result<Expr, EvaluationError>,
    pub name: String,
    pub infix: bool,
}

#[derive(Clone)]
pub struct ExpressionContext {
    pub functions: HashMap<FunctionID, InvokableDefinition>,
    pub variables: HashMap<VariableID, TracedExpr>,
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
            let mut context = ef3r_stdlib();
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
        Expr::Pair(_, _) => Ok(expr.traced()),
        Expr::Type(_) => Ok(expr.traced()),
        Expr::Lambda(_, _, _) => Ok(expr.traced()),
        Expr::BuiltinFunction(_) => Ok(expr.traced()),
        Expr::Node(_) => Ok(expr.traced()),
        // Function applications need to be reduced.
        Expr::Apply(f, xs) => match f.evaluated {
            // Builtin functions can just be looked up in the evaluator context.
            Expr::BuiltinFunction(id) => {
                let implementation = ctx
                    .expression_context
                    .functions
                    .get(&id)
                    .ok_or(EvaluationError::NotAFunction(f.evaluated.clone()))?
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
                    implementation(
                        Arc::new(Mutex::new(ctx.clone())),
                        evaluated_args?.as_mut_slice(),
                    )?,
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
                    .ok_or(EvaluationError::VariableNotFound(var.to_string()))?
                    .evaluated
                    .clone(),
            ),
            _ => Err(EvaluationError::NotAFunction(f.evaluated.clone()))?,
        },
        // Variables are looked up in the current context.
        Expr::Var(x) => Ok(ctx
            .expression_context
            .variables
            .get(&x)
            .ok_or(EvaluationError::VariableNotFound(x.to_string()))?
            .clone()),
    }
}

/// Entrypoint for the ef3r interpreter. Takes a list of a statements
///  and executes them.
pub fn interpret(ctx: Arc<Mutex<Context>>, statements: &[Statement]) {
    for statement in statements {
        let cloned_ctx = ctx.clone();
        match statement {
            Statement::Var(x, expr) => with_lock(cloned_ctx.as_ref(), |lock| {
                let evaluated = evaluate_traced(lock, expr.clone()).unwrap();

                lock.expression_context
                    .variables
                    .insert(x.to_string(), evaluated);
            }),
            Statement::Execute(result_var, action_expr) => {
                let result = invoke_action_expression(cloned_ctx, action_expr);

                if let Some(var) = result_var {
                    ctx.lock()
                        .unwrap()
                        .expression_context
                        .variables
                        .insert(var.clone(), result);
                }
            }
        };
    }
}

pub fn invoke_action_expression(
    ctx: Arc<Mutex<Context>>,
    action_expr: &TracedExpr,
) -> TracedExpr {
    match &action_expr.evaluated {
        Expr::Apply(action, args) => {
            let action = action;
            match &action.evaluated {
                Expr::BuiltinFunction(action_id) => {
                    let (action, evaluated_args) =
                        with_lock(ctx.as_ref(), |lock| {
                            println!(
                                "DBG - Got builtin function {}",
                                action_id
                            );

                            let action = lock.expression_context.functions
                                [&action_id]
                                .definition;

                            let evaluated_args: Result<Vec<TracedExpr>, _> =
                                args.into_iter()
                                    .map(|arg| {
                                        evaluate_traced(lock, arg.clone())
                                    })
                                    .collect();

                            (action, evaluated_args)
                        });

                    TracedExpr::build(
                        (action)(
                            ctx.clone(),
                            &evaluated_args.unwrap().as_mut_slice(),
                        )
                        .unwrap(),
                        Some(action_expr.get_trace()),
                    )
                }
                Expr::Var(x) => {
                    let cloned = ctx.clone();
                    // Lookup the function reference
                    let resolved = with_lock(ctx.as_ref(), |lock| {
                        lock.expression_context.variables.get(x).cloned()
                    });

                    if let Some(resolved) = resolved {
                        let resolved_function =
                            function_from_expression(ctx, resolved);

                        let evaluated_args =
                            with_lock(cloned.as_ref(), |lock| {
                                let mut evaluated = Vec::new();
                                for arg in args {
                                    evaluated.push(
                                        evaluate_traced(lock, arg.clone())
                                            .unwrap(),
                                    );
                                }
                                evaluated
                            });

                        TracedExpr::build(
                            (resolved_function)(
                                cloned,
                                &evaluated_args.as_slice(),
                            )
                            .unwrap(),
                            Some(action_expr.get_trace()),
                        )
                    } else {
                        return Err(EvaluationError::VariableNotFound(
                            x.to_string(),
                        ))
                        .unwrap();
                    }
                }
                _ => with_lock(ctx.as_ref(), |lock| {
                    dbg!(lock.expression_context.variables.clone());

                    Err(EvaluationError::NotAFunction(action.evaluated.clone()))
                        .unwrap()
                }),
            }
        }
        _ => Err(EvaluationError::NotAFunction(action_expr.evaluated.clone()))
            .unwrap(),
    }
}

fn function_from_expression(
    ctx: Arc<Mutex<Context>>,
    resolved: TracedExpr,
) -> Box<
    dyn Fn(Arc<Mutex<Context>>, &[TracedExpr]) -> Result<Expr, EvaluationError>,
> {
    return match &resolved.evaluated {
        Expr::BuiltinFunction(action_id) => Box::new(
            ctx.lock()
                .unwrap()
                .expression_context
                .functions
                .get(action_id)
                .ok_or(EvaluationError::NotAFunction(
                    resolved.evaluated.clone(),
                ))
                .unwrap()
                .definition,
        ),
        Expr::Lambda(vars, statements, result) => {
            let vars = vars.clone();
            let statements = statements.clone();
            let result = result.clone();
            Box::new(
                move |ctx: Arc<Mutex<Context>>, var_values: &[TracedExpr]| {
                    with_lock(ctx.as_ref(), |lock| {
                        if vars.len() != var_values.len() {
                            return Err(
                                EvaluationError::WrongNumberOfArguments {
                                    expected: vars.len(),
                                    actual: var_values.len(),
                                    for_function: "[lambda]".to_string(),
                                },
                            );
                        }

                        // Substitute variables in statements with their corresponding values
                        let substituted_statements: Vec<Statement> = statements
                            .iter()
                            .map(|statement| match statement {
                                Statement::Var(var_name, expr) => {
                                    let substituted_expr = vars
                                        .iter()
                                        .zip(var_values.iter())
                                        .fold(
                                            expr.evaluated.clone(),
                                            |acc, (var, var_value)| {
                                                substitute(
                                                    var.clone(),
                                                    var_value.evaluated.clone(),
                                                    acc,
                                                )
                                            },
                                        )
                                        .traced();
                                    Statement::Var(
                                        var_name.clone(),
                                        substituted_expr,
                                    )
                                }
                                Statement::Execute(optional_var, expr) => {
                                    let substituted_expr = vars
                                        .iter()
                                        .zip(var_values.iter())
                                        .fold(
                                            expr.evaluated.clone(),
                                            |acc, (var, var_value)| {
                                                substitute(
                                                    var.clone(),
                                                    var_value.evaluated.clone(),
                                                    acc,
                                                )
                                            },
                                        )
                                        .traced();
                                    Statement::Execute(
                                        optional_var.clone(),
                                        substituted_expr,
                                    )
                                }
                            })
                            .collect();

                        // Run the substituted statements
                        interpret(ctx.clone(), &substituted_statements);

                        // Substitute variables in result expression
                        let substituted_result_expr =
                            vars.iter().zip(var_values.iter()).fold(
                                result.evaluated.clone(),
                                |acc, (var, var_value)| {
                                    substitute(
                                        var.clone(),
                                        var_value.evaluated.clone(),
                                        acc,
                                    )
                                },
                            );

                        // Run the result to get the return value
                        let result: Result<Expr, EvaluationError> =
                            evaluate(lock, substituted_result_expr)
                                .map(|x| x.evaluated);

                        result
                    })
                },
            )
        }
        _ => {
            return Err(EvaluationError::NotAFunction(
                resolved.evaluated.clone(),
            ))
            .unwrap();
        }
    };
}
