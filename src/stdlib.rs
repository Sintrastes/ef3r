use std::collections::HashMap;

use daggy::Dag;

use crate::{
    ast::Expr,
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
    let mul: fn(&[Expr]) -> Result<Expr, EvaluationError> = |xs: &[Expr]| {
        let first = xs.get(0).unwrap().clone();
        let second = xs.get(1).unwrap().clone();

        match (first, second) {
            (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x * y)),
            _ => Err(EvaluationError::TypeError)?,
        }
    };

    let add: fn(&[Expr]) -> Result<Expr, EvaluationError> = |xs: &[Expr]| {
        let first = xs.get(0).unwrap().clone();
        let second = xs.get(1).unwrap().clone();

        match (first, second) {
            (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x + y)),
            _ => Err(EvaluationError::TypeError)?,
        }
    };

    let div: fn(&[Expr]) -> Result<Expr, EvaluationError> = |xs: &[Expr]| {
        let first = xs.get(0).unwrap().clone();
        let second = xs.get(1).unwrap().clone();

        match (first, second) {
            (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x / y)),
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
            actions: HashMap::new(),
            variables: HashMap::new(),
        },
        graph: Dag::new(),
    }
}
