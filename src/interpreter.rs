use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use daggy::Dag;

use crate::{
    ast::{substitute, Expr, FunctionID, Statement, TracedExpr, VariableID},
    debugging::{Debugger, NoOpDebugger, StepDebugger},
    frp::{with_lock, Node},
    types::ExprType,
};

///
/// Local state needed to evaluate expressions and
///  update the FRP event loop.
///
#[derive(Clone)]
pub struct Context<'a, T: Debugger + 'static>
where
    'a: 'static,
{
    pub expression_context: ExpressionContext<T>,
    pub graph: Dag<Node<'a, T>, (), u32>,
    pub debugger: T,
}

unsafe impl<'a, T: Debugger> Send for Context<'a, T> {}

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
pub struct FunctionDefinition<T: Debugger + 'static> {
    pub argument_types: Vec<ExprType>,
    pub result_type: ExprType,
    pub definition: fn(
        Arc<Mutex<Context<T>>>,
        &[TracedExpr],
    ) -> Result<Expr, EvaluationError>,
    pub name: String,
}

#[derive(Clone)]
pub struct ExpressionContext<T: Debugger + 'static> {
    pub functions: HashMap<FunctionID, FunctionDefinition<T>>,
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
pub fn apply_traced<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    expr: TracedExpr,
    args: &[TracedExpr],
) -> Result<TracedExpr, EvaluationError> {
    let evaluated = evaluate::<T>(
        ctx.clone(),
        Expr::Apply(
            Box::new(expr.evaluated.clone().traced()),
            args.iter().map(|x| x.evaluated.clone().traced()).collect(),
        ),
    );

    // Before building the trace, expand variables in args
    let expanded_args: Vec<TracedExpr> = args
        .iter()
        .map(|arg| match &arg.evaluated {
            Expr::Var(var_name) => {
                if let Some(value) = ctx
                    .lock()
                    .unwrap()
                    .expression_context
                    .variables
                    .get(var_name)
                {
                    value.clone()
                } else {
                    arg.clone()
                }
            }
            _ => arg.clone(),
        })
        .collect();

    let trace = Expr::Apply(
        Box::new(expr.clone()),
        expanded_args
            .iter()
            .map(|x| x.get_trace().traced())
            .collect(),
    );

    Ok(TracedExpr {
        evaluated: evaluated?.evaluated,
        stored_trace: Some(trace),
    })
}

pub fn evaluate_traced<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    expr: TracedExpr,
) -> Result<TracedExpr, EvaluationError> {
    evaluate_traced_rec::<T>(ctx, expr.evaluated.clone())
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use crate::{
        ast::Expr, debugging::NoOpDebugger, interpreter::evaluate,
        stdlib::ef3r_stdlib,
    };

    quickcheck! {
        fn evaluation_is_idempotent(expr: Expr) -> bool {
            let context = Arc::new(Mutex::new(ef3r_stdlib()));

            println!("Evaluating: {}", expr.clone());
            let evaluated = evaluate::<NoOpDebugger>(context.clone(), expr);
            match evaluated {
                Err(_) => true, // Property does not apply if expression is malformed.
                Ok(inner) => Ok(inner.clone()) == evaluate::<NoOpDebugger>(context, inner.evaluated),
            }
        }
    }
}

pub fn evaluate<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    expr: Expr,
) -> Result<TracedExpr, EvaluationError> {
    evaluate_traced_rec::<T>(ctx, expr)
}

fn evaluate_traced_rec<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    expr: Expr,
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
        Expr::List(_) => Ok(expr.traced()),
        // Function applications need to be reduced.
        Expr::Apply(_, _) => {
            evaluate_function_application::<T>(ctx, &(expr.clone()))
        }
        // Variables are looked up in the current context.
        Expr::Var(x) => Ok(ctx
            .lock()
            .unwrap()
            .expression_context
            .variables
            .get(&x)
            .ok_or(EvaluationError::VariableNotFound(x.to_string()))?
            .clone()),
    }
}

/// Entrypoint for the ef3r interpreter. Takes a list of a statements
///  and executes them.
pub fn interpret<T>(
    ctx: Arc<Mutex<Context<T>>>,
    statements: &[Statement],
) -> Result<(), EvaluationError>
where
    T: Debugger + 'static,
{
    for statement in statements {
        with_lock(ctx.as_ref(), |locked| {
            T::suspend(statement.location, locked)
        });

        let evaluated =
            evaluate::<T>(ctx.clone(), statement.expr.clone().from_raw())?;

        if let Some(var) = &statement.var {
            ctx.lock()
                .unwrap()
                .expression_context
                .variables
                .insert(var.clone(), evaluated);
        };
    }

    Ok(())
}

pub fn evaluate_function_application<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    action_expr: &Expr,
) -> Result<TracedExpr, EvaluationError> {
    match &action_expr {
        Expr::Apply(action, args) => {
            let evaluated_args: Result<Vec<TracedExpr>, _> = args
                .into_iter()
                .map(|arg| evaluate_traced::<T>(ctx.clone(), arg.clone()))
                .collect();

            let evaluated_fn =
                evaluate::<T>(ctx.clone(), action.clone().evaluated)?
                    .evaluated
                    .clone();

            let expanded_args = args
                .iter()
                .map(|arg| {
                    if let Expr::Var(var_name) = &arg.evaluated {
                        if let Some(value) = ctx
                            .lock()
                            .unwrap()
                            .expression_context
                            .variables
                            .get(var_name)
                        {
                            value.get_trace().traced()
                        } else {
                            arg.clone()
                        }
                    } else {
                        arg.clone()
                    }
                })
                .collect();

            let action_fn =
                function_from_expression::<T>(ctx.clone(), evaluated_fn)?;

            Ok(TracedExpr::build(
                (action_fn)(ctx, &evaluated_args?.as_mut_slice())?,
                Some(Expr::Apply(action.clone(), expanded_args)),
            ))
        }
        _ => Err(EvaluationError::NotAFunction(action_expr.clone())),
    }
}

fn function_from_expression<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    resolved: Expr,
) -> Result<
    Box<
        dyn Fn(
            Arc<Mutex<Context<T>>>,
            &[TracedExpr],
        ) -> Result<Expr, EvaluationError>,
    >,
    EvaluationError,
> {
    return Ok(match &resolved {
        Expr::BuiltinFunction(action_id) => Box::new(
            ctx.lock()
                .unwrap()
                .expression_context
                .functions
                .get(action_id)
                .ok_or(EvaluationError::NotAFunction(resolved.clone()))?
                .definition,
        ),
        Expr::Lambda(vars, statements, result) => {
            let vars = vars.clone();
            let statements = statements.clone();
            let result = result.clone();
            Box::new(
                move |ctx: Arc<Mutex<Context<T>>>,
                      var_values: &[TracedExpr]| {
                    if vars.len() != var_values.len() {
                        return Err(EvaluationError::WrongNumberOfArguments {
                            expected: vars.len(),
                            actual: var_values.len(),
                            for_function: "[lambda]".to_string(),
                        });
                    }

                    // Substitute variables in statements with their corresponding values
                    let substituted_statements: Vec<Statement> = statements
                        .iter()
                        .map(|statement| {
                            let substituted_expr =
                                vars.iter().zip(var_values.iter()).fold(
                                    statement.expr.clone(),
                                    |acc, (var, var_value)| {
                                        substitute(
                                            var.clone(),
                                            var_value
                                                .evaluated
                                                .clone()
                                                .to_raw(),
                                            acc,
                                        )
                                    },
                                );
                            Statement {
                                location: statement.location,
                                var: statement.var.clone(),
                                expr: substituted_expr,
                            }
                        })
                        .collect();

                    // Run the substituted statements
                    interpret::<T>(ctx.clone(), &substituted_statements)?;

                    // Substitute variables in result expression
                    let substituted_result_expr =
                        vars.iter().zip(var_values.iter()).fold(
                            result.evaluated.clone(),
                            |acc, (var, var_value)| {
                                substitute(
                                    var.clone(),
                                    var_value.evaluated.clone().to_raw(),
                                    acc.to_raw(),
                                )
                                .from_raw()
                            },
                        );

                    // Run the result to get the return value
                    let result: Result<Expr, EvaluationError> =
                        evaluate::<T>(ctx, substituted_result_expr)
                            .map(|x| x.evaluated);

                    result
                },
            )
        }
        _ => {
            return Err(EvaluationError::NotAFunction(resolved.clone()));
        }
    });
}
