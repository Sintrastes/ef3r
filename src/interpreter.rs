use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use daggy::Dag;

use crate::{
    ast::{substitute, Expr, FunctionID, Statement, TracedExpr, VariableID},
    frp::Node,
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
    pub argument_types: Vec<ExprType>,
    pub result_type: ExprType,
    pub definition:
        fn(Arc<Mutex<Context>>, &[TracedExpr]) -> Result<Expr, EvaluationError>,
    pub name: String,
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
    ctx: Arc<Mutex<Context>>,
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
    ctx: Arc<Mutex<Context>>,
    expr: TracedExpr,
) -> Result<TracedExpr, EvaluationError> {
    match expr {
        TracedExpr {
            evaluated,
            stored_trace,
        } => Ok(TracedExpr {
            evaluated: evaluate_traced_rec(ctx, evaluated.clone())?.evaluated,
            stored_trace: Some(stored_trace.unwrap_or(evaluated)),
        }),
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use crate::{ast::Expr, interpreter::evaluate, stdlib::ef3r_stdlib};

    quickcheck! {
        fn evaluation_is_idempotent(expr: Expr) -> bool {
            let context = Arc::new(Mutex::new(ef3r_stdlib()));

            println!("Evaluating: {}", expr.clone());
            let evaluated = evaluate(context.clone(), expr);
            match evaluated {
                Err(_) => true, // Property does not apply if expression is malformed.
                Ok(inner) => Ok(inner.clone()) == evaluate(context, inner.evaluated),
            }
        }
    }
}

pub fn evaluate(
    ctx: Arc<Mutex<Context>>,
    expr: Expr,
) -> Result<TracedExpr, EvaluationError> {
    evaluate_traced_rec(ctx, expr)
}

fn evaluate_traced_rec(
    ctx: Arc<Mutex<Context>>,
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
            evaluate_function_application(ctx, &(expr.clone()))
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
pub fn interpret(
    ctx: Arc<Mutex<Context>>,
    statements: &[Statement],
) -> Result<(), EvaluationError> {
    for statement in statements {
        let evaluated = evaluate(ctx.clone(), statement.expr.clone())?;

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

pub fn evaluate_function_application(
    ctx: Arc<Mutex<Context>>,
    action_expr: &Expr,
) -> Result<TracedExpr, EvaluationError> {
    match &action_expr {
        Expr::Apply(action, args) => {
            let evaluated_args: Result<Vec<TracedExpr>, _> = args
                .into_iter()
                .map(|arg| evaluate_traced(ctx.clone(), arg.clone()))
                .collect();

            let evaluated_fn = evaluate(ctx.clone(), action.clone().evaluated)?
                .evaluated
                .clone();

            let action = function_from_expression(ctx.clone(), evaluated_fn)?;

            Ok(TracedExpr::build(
                (action)(ctx, &evaluated_args?.as_mut_slice())?,
                Some(action_expr.clone()),
            ))
        }
        _ => Err(EvaluationError::NotAFunction(action_expr.clone())),
    }
}

fn function_from_expression(
    ctx: Arc<Mutex<Context>>,
    resolved: Expr,
) -> Result<
    Box<
        dyn Fn(
            Arc<Mutex<Context>>,
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
                move |ctx: Arc<Mutex<Context>>, var_values: &[TracedExpr]| {
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
                                            var_value.evaluated.clone(),
                                            acc,
                                        )
                                    },
                                );
                            Statement {
                                var: statement.var.clone(),
                                expr: substituted_expr,
                            }
                        })
                        .collect();

                    // Run the substituted statements
                    interpret(ctx.clone(), &substituted_statements)?;

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
                        evaluate(ctx, substituted_result_expr)
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
