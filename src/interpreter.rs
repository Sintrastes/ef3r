use std::collections::HashMap;

use daggy::Dag;

use crate::{
    ast::{ActionID, Expr, FunctionID, TracedExpr, VariableID},
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
}

#[derive(Clone)]
pub struct ExpressionContext {
    pub functions:
        HashMap<FunctionID, fn(&[Expr]) -> Result<Expr, EvaluationError>>,
    pub actions: HashMap<ActionID, fn() -> ()>,
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
            Box::new(expr.evaluated.clone()),
            args.iter().map(|x| x.evaluated.clone()).collect(),
        ),
    );

    let trace = Expr::Apply(
        Box::new(expr.trace.clone().unwrap_or(expr.evaluated.clone())),
        args.iter()
            .map(|x| x.trace.clone().unwrap_or(x.evaluated.clone()))
            .collect(),
    );

    Ok(TracedExpr {
        evaluated: evaluated?,
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
            evaluated: evaluate(&ctx, expr)?,
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
                Ok(inner) => Ok(inner.clone()) == evaluate(&context, inner),
            }
        }
    }
}

pub fn evaluate(
    ctx: &ExpressionContext,
    expr: Expr,
) -> Result<Expr, EvaluationError> {
    match expr {
        // Literals evaluate to themselves.
        Expr::None => Ok(expr),
        Expr::Int(_) => Ok(expr),
        Expr::String(_) => Ok(expr),
        Expr::Float(_) => Ok(expr),
        Expr::Action(_) => Ok(expr),
        Expr::Lambda(_, _) => Ok(expr),
        Expr::BuiltinFunction(_) => Ok(expr),
        // Function applications need to be reduced.
        Expr::Apply(f, x) => match *f {
            // Builtin functions can just be looked up in the evaluator context.
            Expr::BuiltinFunction(id) => {
                let implemntation = ctx
                    .functions
                    .get(&id)
                    .ok_or(EvaluationError::NotAFunction)?;
                implemntation(x.as_ref())
            }
            Expr::Lambda(_, _) => Err(EvaluationError::Unimplemented)?,
            // If it's an application itself, try to evaluate it first.
            Expr::Apply(_, _) => {
                println!("Evaluating apply on {:?} and {:?}", f, x);
                evaluate(ctx, Expr::Apply(Box::new(evaluate(ctx, *f)?), x))
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
            .evaluated
            .clone()),
    }
}
