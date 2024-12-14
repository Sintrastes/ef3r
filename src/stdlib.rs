use std::{
    collections::HashMap,
    io::{self, BufRead},
    sync::{atomic::AtomicBool, Arc, Mutex},
    thread,
};

use daggy::{Dag, NodeIndex};

use crate::{
    ast::{Expr, Statement, TracedExpr},
    frp::Node,
    interpreter::{
        evaluate_function_application, Context, EvaluationError,
        ExpressionContext, InvokableDefinition,
    },
    typechecking::type_of,
    types::ExprType,
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

// Pairs
pub const PAIR_FIRST_ID: u32 = 6;
pub const PAIR_SECOND_ID: u32 = 7;

// Action IDs
pub const PRINT_ID: u32 = 8;
pub const READLN_ID: u32 = 9;
pub const NEW_NODE_ID: u32 = 10;
pub const UPDATE_NODE_ID: u32 = 11;
pub const NODE_CURRENT_VALUE: u32 = 12;
pub const LAUNCH: u32 = 13;

pub fn ef3r_stdlib<'a>() -> Context<'a> {
    let mul = InvokableDefinition {
        name: "*".to_string(),
        infix: true,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 0,
                    for_function: "*".to_string(),
                })?
                .clone();
            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 1,
                    for_function: "*".to_string(),
                })?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x * y)),
                (actual, _) if !matches!(actual, Expr::Int(_)) => {
                    Err(EvaluationError::TypeError {
                        expected: ExprType::Int,
                        actual: type_of(&actual).unwrap(),
                        at_loc: "*".to_string(),
                    })?
                }
                (_, actual) => Err(EvaluationError::TypeError {
                    expected: ExprType::Int,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "*".to_string(),
                })?,
            }
        },
    };

    let add = InvokableDefinition {
        name: "+".to_string(),
        infix: true,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 0,
                    for_function: "+".to_string(),
                })?
                .clone();
            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 1,
                    for_function: "+".to_string(),
                })?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x + y)),
                (actual, _) if !matches!(actual, Expr::Int(_)) => {
                    Err(EvaluationError::TypeError {
                        expected: ExprType::Int,
                        actual: type_of(&actual).unwrap(),
                        at_loc: "+".to_string(),
                    })?
                }
                (_, actual) => Err(EvaluationError::TypeError {
                    expected: ExprType::Int,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "+".to_string(),
                })?,
            }
        },
    };

    let div = InvokableDefinition {
        name: "/".to_string(),
        infix: true,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 0,
                    for_function: "/".to_string(),
                })?
                .clone();
            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 1,
                    for_function: "/".to_string(),
                })?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x / y)),
                (actual, _) if !matches!(actual, Expr::Int(_)) => {
                    Err(EvaluationError::TypeError {
                        expected: ExprType::Int,
                        actual: type_of(&actual).unwrap(),
                        at_loc: "/".to_string(),
                    })?
                }
                (_, actual) => Err(EvaluationError::TypeError {
                    expected: ExprType::Int,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "/".to_string(),
                })?,
            }
        },
    };

    let append = InvokableDefinition {
        name: "++".to_string(),
        infix: true,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 0,
                    for_function: "++".to_string(),
                })?
                .clone();

            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 1,
                    for_function: "++".to_string(),
                })?
                .clone();

            match (first.evaluated, second.evaluated) {
                (Expr::String(x), Expr::String(y)) => {
                    Ok(Expr::String(x.to_owned() + y.as_ref()))
                }
                (actual, _) if !matches!(actual, Expr::String(_)) => {
                    Err(EvaluationError::TypeError {
                        expected: ExprType::String,
                        actual: type_of(&actual).unwrap(),
                        at_loc: "++".to_string(),
                    })?
                }
                (_, actual) => Err(EvaluationError::TypeError {
                    expected: ExprType::String,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "++".to_string(),
                })?,
            }
        },
    };

    let uppercase = InvokableDefinition {
        name: "uppercase".to_string(),
        infix: false,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: 0,
                    for_function: "uppercase".to_string(),
                })?
                .clone();

            match first.evaluated {
                Expr::String(x) => Ok(Expr::String(x.to_uppercase())),
                actual => Err(EvaluationError::TypeError {
                    expected: ExprType::String,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "uppercase".to_string(),
                })?,
            }
        },
    };

    let pair_first_fn = InvokableDefinition {
        name: "first".to_string(),
        infix: false,
        definition: |_, xs: &[TracedExpr]| {
            let pair = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: 0,
                    for_function: "first".to_string(),
                })?
                .clone();

            match pair.evaluated {
                Expr::Pair(x, _) => Ok(x.evaluated),
                actual => Err(EvaluationError::TypeError {
                    expected: ExprType::Pair(
                        Box::new(ExprType::Any),
                        Box::new(ExprType::Any),
                    ),
                    actual: type_of(&actual).unwrap(),
                    at_loc: "first".to_string(),
                })?,
            }
        },
    };

    let pair_second_fn = InvokableDefinition {
        name: "second".to_string(),
        infix: false,
        definition: |_, xs: &[TracedExpr]| {
            let pair = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: 0,
                    for_function: "second".to_string(),
                })?
                .clone();

            match pair.evaluated {
                Expr::Pair(_, y) => Ok(y.evaluated),
                actual => Err(EvaluationError::TypeError {
                    expected: ExprType::Pair(
                        Box::new(ExprType::Any),
                        Box::new(ExprType::Any),
                    ),
                    actual: type_of(&actual).unwrap(),
                    at_loc: "second".to_string(),
                })?,
            }
        },
    };

    let print_fn = InvokableDefinition {
        name: "println".to_string(),
        infix: false,
        definition: |_, xs: &[TracedExpr]| {
            let first = xs.get(0).unwrap().clone();

            println!("{}", first);
            Ok(Expr::None)
        },
    };

    let readln_fn = InvokableDefinition {
        name: "readln".to_string(),
        infix: false,
        definition: |_, xs: &[TracedExpr]| {
            let stdin = io::stdin();
            let result = stdin.lock().lines().next().unwrap().unwrap();

            Result::Ok(Expr::String(result))
        },
    };

    let update_node_fn = InvokableDefinition {
        name: "update_node".to_string(),
        infix: false,
        definition: |ctx, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 0,
                    for_function: "update_node".to_string(),
                })?
                .clone();

            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 1,
                    for_function: "update_node".to_string(),
                })?
                .clone();

            match first.evaluated {
                Expr::Node(node_id) => {
                    ctx.lock()
                        .unwrap()
                        .graph
                        .node_weight_mut(NodeIndex::new(node_id))
                        .unwrap()
                        .update(second);

                    Ok(Expr::Unit)
                }
                actual => Err(EvaluationError::TypeError {
                    expected: ExprType::Node(Box::new(ExprType::Any)),
                    actual: type_of(&actual).unwrap(),
                    at_loc: "update_node".to_string(),
                })?,
            }
        },
    };

    let node_current_value_fn = InvokableDefinition {
        name: "current_value".to_string(),
        infix: false,
        definition: |ctx, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: 0,
                    for_function: "current_value".to_string(),
                })?
                .clone();

            match first.evaluated {
                Expr::Node(node_id) => {
                    let value = ctx
                        .lock()
                        .unwrap()
                        .graph
                        .node_weight_mut(NodeIndex::new(node_id))
                        .unwrap()
                        .current();

                    Ok(value.evaluated)
                }
                actual => Err(EvaluationError::TypeError {
                    expected: ExprType::Node(Box::new(ExprType::Any)),
                    actual: type_of(&actual).unwrap(),
                    at_loc: "current_value".to_string(),
                })?,
            }
        },
    };

    let new_node_fn = InvokableDefinition {
        name: "new_node".to_string(),
        infix: false,
        definition: |ctx, xs: &[TracedExpr]| {
            println!("Calling new_node");

            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: 0,
                    for_function: "new_node".to_string(),
                })?
                .clone();

            let second = xs
                .get(1)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 2,
                    actual: 1,
                    for_function: "new_node".to_string(),
                })?
                .clone();

            println!("Got new_node arguments");

            match first.evaluated {
                Expr::Type(x) => {
                    if type_of(&second.evaluated) == Some(x.clone()) {
                        println!("Types checked");
                        let update_fn =
                            Expr::BuiltinFunction(UPDATE_NODE_ID).traced();

                        println!("Creating new node");

                        let fresh_id = Node::new(
                            |_| {},
                            Arc::new(AtomicBool::new(false)),
                            &mut ctx.lock().unwrap().graph,
                            second,
                        );

                        println!("Returning from new node");

                        Ok(Expr::Pair(
                            Box::new(Expr::Node(fresh_id.index()).traced()),
                            Box::new(update_fn),
                        ))
                    } else {
                        Err(EvaluationError::TypeError {
                            expected: x,
                            actual: type_of(&second.evaluated).unwrap(),
                            at_loc: "new_node".to_string(),
                        })?
                    }
                }
                actual => Err(EvaluationError::TypeError {
                    expected: ExprType::Type,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "new_node".to_string(),
                })?,
            }
        },
    };

    let launch_fn = InvokableDefinition {
        name: "launch".to_string(),
        infix: false,
        definition: move |ctx: Arc<Mutex<Context>>, xs: &[TracedExpr]| {
            let first = xs
                .get(0)
                .ok_or(EvaluationError::WrongNumberOfArguments {
                    expected: 1,
                    actual: 0,
                    for_function: "launch".to_string(),
                })?
                .clone();

            let thread_ctx = ctx.clone();

            dbg!(first.clone());

            thread::spawn(move || {
                println!("DBG - GOT CTX LOCK, LAUNCHING BODY");
                evaluate_function_application(
                    thread_ctx,
                    &Expr::Apply(Box::new(first), Box::new([])),
                );
                println!("DONE LAUNCHING");
            });
            Ok(Expr::Unit)
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
                (PAIR_FIRST_ID, pair_first_fn),
                (PAIR_SECOND_ID, pair_second_fn),
                (PRINT_ID, print_fn),
                (READLN_ID, readln_fn),
                (NEW_NODE_ID, new_node_fn),
                (UPDATE_NODE_ID, update_node_fn),
                (NODE_CURRENT_VALUE, node_current_value_fn),
                (LAUNCH, launch_fn),
            ]),
            variables: HashMap::new(),
        },
        graph: Dag::new(),
    }
}

pub fn get_stdlib_functions<'a>(stdlib: &'a Context) -> HashMap<&'a str, u32> {
    stdlib
        .expression_context
        .functions
        .iter()
        .map(|(id, invokable)| (invokable.name.as_str(), *id))
        .collect()
}

pub fn resolve_builtin_functions(
    statements: Vec<Statement>,
    stdlib_functions: &HashMap<&str, u32>,
) -> Vec<Statement> {
    statements
        .into_iter()
        .map(|stmt| replace_variables_in_statement(stmt, stdlib_functions))
        .collect()
}

fn replace_variables_in_statement(
    stmt: Statement,
    stdlib_functions: &HashMap<&str, u32>,
) -> Statement {
    Statement {
        var: stmt.var,
        expr: replace_variables_in_expr(stmt.expr, stdlib_functions),
    }
}

fn replace_variables_in_traced_expr(
    traced: TracedExpr,
    stdlib_functions: &HashMap<&str, u32>,
) -> TracedExpr {
    let replaced_eval =
        replace_variables_in_expr(traced.evaluated, stdlib_functions);
    let replaced_trace = traced
        .stored_trace
        .map(|t| replace_variables_in_expr(t, stdlib_functions));

    TracedExpr::build(replaced_eval, replaced_trace)
}

fn replace_variables_in_expr(
    expr: Expr,
    stdlib_functions: &HashMap<&str, u32>,
) -> Expr {
    match expr {
        Expr::Var(var_name) => {
            if let Some(func_id) = stdlib_functions.get(var_name.as_str()) {
                Expr::BuiltinFunction(*func_id)
            } else {
                Expr::Var(var_name)
            }
        }
        Expr::Apply(func, args) => Expr::Apply(
            Box::new(replace_variables_in_traced_expr(*func, stdlib_functions)),
            args.into_vec()
                .into_iter()
                .map(|a| replace_variables_in_traced_expr(a, stdlib_functions))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        ),
        Expr::Pair(first, second) => Expr::Pair(
            Box::new(replace_variables_in_traced_expr(
                *first,
                stdlib_functions,
            )),
            Box::new(replace_variables_in_traced_expr(
                *second,
                stdlib_functions,
            )),
        ),
        Expr::Lambda(vars, stmts, body) => Expr::Lambda(
            vars,
            resolve_builtin_functions(stmts, stdlib_functions),
            Box::new(replace_variables_in_traced_expr(*body, stdlib_functions)),
        ),
        _ => expr,
    }
}
