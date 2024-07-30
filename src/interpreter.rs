
use std::collections::HashMap;

use crate::ast::{ActionID, Expr, FunctionID, TracedExpr, VariableID};

pub struct Context {
    pub functions: HashMap<FunctionID, fn (&[Expr]) -> Expr>,
    pub actions: HashMap<ActionID, fn () -> ()>,
    pub variables: HashMap<VariableID, TracedExpr>
}

/// Ensure an expression is being traced before we evaluate it.
pub fn trace(expr: TracedExpr) -> TracedExpr {
    match expr {
        TracedExpr { evaluated: expr, trace: _ } => TracedExpr { 
            evaluated: expr.clone(),
            trace: Option::Some(expr) 
        }
    }
}

// Apply an expression to a list of arguments in a traced manner.
pub fn apply_traced(expr: TracedExpr, args: &[TracedExpr]) -> TracedExpr {
    todo!()
}

pub fn evaluate_traced(ctx: Context, expr: TracedExpr) -> TracedExpr {
    match expr {
        TracedExpr { evaluated: expr, trace } => TracedExpr { 
            evaluated: evaluate(ctx, expr), 
            trace: trace 
        }
    }
}

pub fn evaluate(ctx: Context, expr: Expr) -> Expr {
    todo!()
}