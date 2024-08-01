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
    pub expressionContext: ExpressionContext,
    pub graph: Dag<Node<TracedExpr>, (), u32>,
}

#[derive(Clone)]
pub struct ExpressionContext {
    pub functions: HashMap<FunctionID, fn(&[Expr]) -> Expr>,
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
) -> TracedExpr {
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

    TracedExpr {
        evaluated,
        trace: Some(trace),
    }
}

pub fn evaluate_traced(
    ctx: &ExpressionContext,
    expr: TracedExpr,
) -> TracedExpr {
    match expr {
        TracedExpr {
            evaluated: expr,
            trace,
        } => TracedExpr {
            evaluated: evaluate(&ctx, expr),
            trace: trace,
        },
    }
}

pub fn evaluate(ctx: &ExpressionContext, expr: Expr) -> Expr {
    match expr {
        // Literals evaluate to themselves.
        Expr::None => expr,
        Expr::Int(_) => expr,
        Expr::String(_) => expr,
        Expr::Float(_) => expr,
        Expr::Action(_) => expr,
        Expr::Lambda(_, _) => expr,
        Expr::BuiltinFunction(_) => expr,
        // Function applications need to be reduced.
        Expr::Apply(f, x) => match *f {
            // Builtin functions can just be looked up in the evaluator context.
            Expr::BuiltinFunction(id) => {
                let implemntation = ctx.functions.get(&id).unwrap();
                implemntation(x.as_ref())
            }
            Expr::Lambda(_, _) => todo!(),
            // If it's an application itself, try to evaluate it first.
            Expr::Apply(_, _) => {
                evaluate(ctx, Expr::Apply(Box::new(evaluate(ctx, *f)), x))
            }
            // If it's a variable, see if we can look it up in the context.
            Expr::Var(var) => evaluate(
                ctx,
                ctx.variables.get(&var).unwrap().evaluated.clone(),
            ),
            _ => panic!("Not a function"),
        },
        // Variables are looked up in the current context.
        Expr::Var(x) => ctx.variables.get(&x).unwrap().evaluated.clone(),
    }
}
