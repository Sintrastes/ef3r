use std::collections::HashMap;

use daggy::Dag;

use crate::{
    ast::Expr,
    interpreter::{Context, ExpressionContext},
};

// Function IDs
pub const MUL_ID: u32 = 0;
pub const ADD_ID: u32 = 1;
pub const DIV_ID: u32 = 2;
pub const SUB_ID: u32 = 3;

// Action IDs
pub const PRINT_ID: u32 = 0;

pub fn ef3r_stdlib() -> Context {
    // Placeholder implementation.
    let id: fn(&[Expr]) -> Expr = |x| x.first().unwrap().clone();

    let mul: fn(&[Expr]) -> Expr = |xs: &[Expr]| {
        let first = xs.get(0).unwrap().clone();
        let second = xs.get(1).unwrap().clone();

        match (first, second) {
            (Expr::Int(x), Expr::Int(y)) => Expr::Int(x * y),
            _ => panic!(),
        }
    };

    let add: fn(&[Expr]) -> Expr = |xs: &[Expr]| {
        let first = xs.get(0).unwrap().clone();
        let second = xs.get(1).unwrap().clone();

        match (first, second) {
            (Expr::Int(x), Expr::Int(y)) => Expr::Int(x + y),
            _ => panic!(),
        }
    };

    let div: fn(&[Expr]) -> Expr = |xs: &[Expr]| {
        let first = xs.get(0).unwrap().clone();
        let second = xs.get(1).unwrap().clone();

        match (first, second) {
            (Expr::Int(x), Expr::Int(y)) => Expr::Int(x / y),
            _ => panic!(),
        }
    };

    // Lookup table for the interpreter
    Context {
        expressionContext: ExpressionContext {
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
