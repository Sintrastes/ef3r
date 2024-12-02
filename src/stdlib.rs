use std::{
    collections::HashMap,
    io::{self, BufRead},
    sync::{atomic::AtomicBool, Arc},
};

use daggy::{Dag, NodeIndex};

use crate::{
    ast::{Expr, Statement, TracedExpr},
    frp::Node,
    interpreter::{
        Context, EvaluationError, ExpressionContext, InvokableDefinition,
    },
    typechecking::type_of,
};

// Function IDs

// Arithmetic
pub const MUL_ID: u32 = 0;
pub const ADD_ID: u32 = 1;
pub const DIV_ID: u32 = 2;
pub const SUB_ID: u32 = 3;

// String processing

pub const APPEND_ID: u32 = 4;
pub const UPPERCASE_ID: u32 = 5;

// Action IDs
pub const PRINT_ID: u32 = 0;
pub const READLN_ID: u32 = 1;
pub const NEW_NODE_ID: u32 = 2;
pub const UPDATE_NODE_ID: u32 = 3;

pub fn ef3r_stdlib() -> Context {
    let mul = InvokableDefinition {
        name: "*".to_string(),
        infix: true,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();
            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x * y)),
                _ => Err(EvaluationError::TypeError)?,
            }
        },
    };

    let add = InvokableDefinition {
        name: "+".to_string(),
        infix: true,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();
            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x + y)),
                _ => Err(EvaluationError::TypeError)?,
            }
        },
    };

    let div = InvokableDefinition {
        name: "/".to_string(),
        infix: true,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();
            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x / y)),
                _ => Err(EvaluationError::TypeError)?,
            }
        },
    };

    let append = InvokableDefinition {
        name: "++".to_string(),
        infix: true,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::String(x), Expr::String(y)) => {
                    Ok(Expr::String(x.to_owned() + y.as_ref()))
                }
                _ => Err(EvaluationError::TypeError)?,
            }
        },
    };

    let uppercase = InvokableDefinition {
        name: "uppercase".to_string(),
        infix: false,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            match first.evaluated {
                Expr::String(x) => Ok(Expr::String(x.to_uppercase())),
                _ => Err(EvaluationError::TypeError)?,
            }
        },
    };

    let print_fn = InvokableDefinition {
        name: "print".to_string(),
        infix: false,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs.get(0).unwrap().clone();

            match first.evaluated {
                Expr::String(str) => {
                    println!("{}", str);
                    Ok(Expr::None)
                }
                _ => Err(EvaluationError::TypeError)?,
            }
        },
    };

    let readln_fn = InvokableDefinition {
        name: "read_ln".to_string(),
        infix: false,
        definition: |_, xs: &[TracedExpr]| {
            let stdin = io::stdin();
            let result = stdin.lock().lines().next().unwrap().unwrap();

            Result::Ok(Expr::String(result))
        },
    };

    let update_node_fn = InvokableDefinition {
        name: "new_node".to_string(),
        infix: false,
        definition: |ctx, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            let second = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            match first.evaluated {
                Expr::Node(node_id) => {
                    ctx.graph
                        .node_weight_mut(NodeIndex::new(node_id))
                        .unwrap()
                        .update(second);

                    Ok(Expr::Unit)
                }
                _ => todo!(),
            }
        },
    };

    let new_node_fn = InvokableDefinition {
        name: "new_node".to_string(),
        infix: false,
        definition: |ctx, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments)?
                .clone();

            match first.evaluated {
                Expr::Type(x) => {
                    if type_of(&second.evaluated) == Some(x) {
                        let update_fn = Expr::Lambda(
                            vec!["x".to_string()],
                            vec![Statement::Execute(
                                None,
                                Expr::Apply(
                                    Box::new(
                                        Expr::Action(UPDATE_NODE_ID).traced(),
                                    ),
                                    Box::new([
                                        Expr::Var("x".to_string()).traced()
                                    ]),
                                )
                                .traced(),
                            )],
                            Box::new(Expr::Unit.traced()),
                        );

                        let fresh_id = Node::new(
                            |_| {},
                            Arc::new(AtomicBool::new(false)),
                            &mut ctx.graph,
                            second,
                        );

                        Ok(Expr::Pair(
                            Box::new(Expr::Node(fresh_id.index()).traced()),
                            Box::new(update_fn.traced()),
                        ))
                    } else {
                        Err(EvaluationError::TypeError)?
                    }
                }
                _ => Err(EvaluationError::TypeError)?,
            }
        },
    };

    // Lookup table for the interpreter
    Context {
        expression_context: ExpressionContext {
            functions: HashMap::from([
                (MUL_ID, mul),
                (ADD_ID, add),
                (DIV_ID, div),
                (APPEND_ID, append),
                (UPPERCASE_ID, uppercase),
            ]),
            actions: HashMap::from([
                (PRINT_ID, print_fn),
                (READLN_ID, readln_fn),
                (NEW_NODE_ID, new_node_fn),
                (UPDATE_NODE_ID, update_node_fn),
            ]),
            variables: HashMap::new(),
        },
        graph: Dag::new(),
    }
}
