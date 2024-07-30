use std::{collections::HashMap, fs::File, io::Write};
use interpreter::Context;
use ast::{Expr, Statement};

pub mod ast;
pub mod frp;
pub mod debugging;
pub mod interpreter;

fn main() {
    // Examples

    let mulID = 0;
    let addID = 1;
    let divID = 2;

    let printID = 0;

    // Placeholder implementation.
    let id: fn(&[Expr]) -> Expr = |x| { 
        x.first().unwrap().clone()
    };

    // Lookup table for the interpreter
    let context = Context {
        functions: HashMap::from([
            (mulID, id),
            (addID, id),
            (divID, id)
        ]),
        actions: HashMap::new(),
        variables: HashMap::new()
    };

    // Example expression.
    let expression = Expr::Apply(
        Box::new(
            Expr::BuiltinFunction(mulID)
        ),
        Box::new(
            [
              Expr::Int(1), 
              Expr::Int(2)
            ]
        )
    );

    // Example program
    let program = [
        Statement::Var("x".to_string(), Expr::Int(42)),
        Statement::Execute(
            Expr::Apply(
                Box::new(
                    Expr::Action(printID)
                ), 
                Box::new(
                    [Expr::Var("x".to_string())]
                )
            )
        )
    ];

    // Write the bytecode of the example to a file.

    let mut out_file = File::create("out.efr3").unwrap();

    let encoded: Vec<u8> = bincode::serialize(&program).unwrap();

    out_file.write(&encoded).unwrap();
}
