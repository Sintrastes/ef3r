use std::collections::HashMap;

use daggy::Dag;

use crate::{
    ast::{Expr, TracedExpr},
    interpreter::{Context, EvaluationError, ExpressionContext},
};

// Function IDs
pub const MUL_ID: u32 = 0;
pub const ADD_ID: u32 = 1;
pub const DIV_ID: u32 = 2;
pub const SUB_ID: u32 = 3;

// Action IDs
pub const PRINT_ID: u32 = 0;

pub fn ef3r_stdlib() -> Context {
    let mul: fn(&[TracedExpr]) -> Result<TracedExpr, EvaluationError> =
        |xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();
            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x * y).traced()),
                _ => Err(EvaluationError::TypeError)?,
            }
        };

    let add: fn(&[TracedExpr]) -> Result<TracedExpr, EvaluationError> =
        |xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();
            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x + y).traced()),
                _ => Err(EvaluationError::TypeError)?,
            }
        };

    let div: fn(&[TracedExpr]) -> Result<TracedExpr, EvaluationError> =
        |xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();
            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x / y).traced()),
                _ => Err(EvaluationError::TypeError)?,
            }
        };

    let print_fn: fn(&[TracedExpr]) -> Result<TracedExpr, EvaluationError> =
        |xs: &[TracedExpr]| {
            let first = xs.get(0).unwrap().clone();

            match first.evaluated {
                Expr::String(str) => {
                    println!("{}", str);
                    Ok(Expr::None.traced())
                }
                _ => Err(EvaluationError::TypeError)?,
            }
        };

    // Lookup table for the interpreter
    Context {
        expression_context: ExpressionContext {
            functions: HashMap::from([
                (MUL_ID, mul),
                (ADD_ID, add),
                (DIV_ID, div),
            ]),
            actions: HashMap::from([(PRINT_ID, print_fn)]),
            variables: HashMap::new(),
        },
        graph: Dag::new(),
    }
}
