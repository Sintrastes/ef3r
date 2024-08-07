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
    pub definition: fn(&[TracedExpr]) -> Result<TracedExpr, EvaluationError>,
    pub name: String,
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
            trace: _,
        } => TracedExpr {
            evaluated: expr.clone(),
            trace: Option::Some(expr),
        },
    }
}

// Apply an expression to a list of arguments in a traced manner.
pub fn apply_traced(
    ctx: &ExpressionContext,
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
        args.iter()
            .map(|x| x.trace.clone().unwrap_or(x.evaluated.clone()).traced())
            .collect(),
    );

    Ok(TracedExpr {
        evaluated: evaluated?.evaluated,
        trace: Some(trace),
    })
}

pub fn evaluate_traced(
    ctx: &ExpressionContext,
    expr: TracedExpr,
) -> Result<TracedExpr, EvaluationError> {
    match expr {
        TracedExpr {
            evaluated: expr,
            trace,
        } => Ok(TracedExpr {
            evaluated: evaluate(&ctx, expr)?.evaluated,
            trace: trace,
        }),
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::Expr, interpreter::evaluate, stdlib::ef3r_stdlib};

    quickcheck! {
        fn prop(expr: Expr) -> bool {
            let context = ef3r_stdlib().expression_context;
            println!("Evaluating: {}", expr.clone());
            let evaluated = evaluate(&context, expr);
            match evaluated {
                Err(_) => true, // Property does not apply if expression is malformed.
                Ok(inner) => Ok(inner.clone()) == evaluate(&context, inner.evaluated),
            }
        }
    }
}

pub fn evaluate(
    ctx: &ExpressionContext,
    expr: Expr,
) -> Result<TracedExpr, EvaluationError> {
    match expr {
        // Literals evaluate to themselves.
        Expr::None => Ok(expr.traced()),
        Expr::Int(_) => Ok(expr.traced()),
        Expr::String(_) => Ok(expr.traced()),
        Expr::Float(_) => Ok(expr.traced()),
        Expr::Action(_) => Ok(expr.traced()),
        Expr::Lambda(_, _) => Ok(expr.traced()),
        Expr::BuiltinFunction(_) => Ok(expr.traced()),
        Expr::Node(_) => Ok(expr.traced()),
        // For now, we'll just treat this like an opaque expression.
        // Evaluating it is different from a normal expression since we'd need
        // access to the FRP graph.
        Expr::MapNode(_, _) => Ok(expr.traced()),
        // Function applications need to be reduced.
        Expr::Apply(f, x) => match f.evaluated {
            // Builtin functions can just be looked up in the evaluator context.
            Expr::BuiltinFunction(id) => {
                let implemntation = ctx
                    .functions
                    .get(&id)
                    .ok_or(EvaluationError::NotAFunction)?
                    .definition;
                implemntation(x.as_ref())
            }
            Expr::Lambda(_, _) => Err(EvaluationError::Unimplemented)?,
            // If it's an application itself, try to evaluate it first.
            Expr::Apply(_, _) => {
                println!("Evaluating apply on {:?} and {:?}", f, x);
                evaluate(
                    ctx,
                    Expr::Apply(Box::new(evaluate(ctx, f.evaluated)?), x),
                )
            }
            // If it's a variable, see if we can look it up in the context.
            Expr::Var(var) => evaluate(
                ctx,
                ctx.variables
                    .get(&var)
                    .ok_or(EvaluationError::VariableNotFound)?
                    .evaluated
                    .clone(),
            ),
            _ => Err(EvaluationError::NotAFunction)?,
        },
        // Variables are looked up in the current context.
        Expr::Var(x) => Ok(ctx
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
                let evaluated =
                    evaluate_traced(&ctx.expression_context, expr.clone())
                        .unwrap();

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

                                let evaluated_args: Vec<TracedExpr> = args
                                    .into_iter()
                                    .map(|arg| {
                                        evaluate_traced(
                                            &ctx.expression_context,
                                            arg.clone(),
                                        )
                                        .unwrap()
                                    })
                                    .collect();

                                let result = (action)(&evaluated_args).unwrap();

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
