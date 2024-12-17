use std::{
    collections::HashMap,
    io::{self, BufRead},
    sync::{atomic::AtomicBool, Arc, Mutex},
    thread,
};

use daggy::{Dag, NodeIndex};

use crate::{
    ast::{Expr, Statement, TracedExpr},
    frp::{filter_node, fold_node, map_node, Node},
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
pub const PAIR_ID: u32 = 14;
pub const TYPE_OF_ID: u32 = 15;
pub const AND_ID: u32 = 16;
pub const OR_ID: u32 = 17;
pub const NOT_ID: u32 = 18;
pub const ASSERT_ID: u32 = 19;
pub const MAP_NODE_ID: u32 = 20;
pub const FILTER_NODE_ID: u32 = 21;
pub const FOLD_NODE_ID: u32 = 22;

macro_rules! build_invokable {
    // Pattern for single argument function
    ($name:expr, $infix:expr, |$ctx:ident, $param:ident| $body:expr) => {
        InvokableDefinition {
            name: $name.to_string(),
            infix: $infix,
            definition: |$ctx, xs: &[TracedExpr]| {
                let $param = xs
                    .get(0)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 1,
                        actual: 0,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                $body
            },
        }
    };

    // Pattern for two argument function
    ($name:expr, $infix:expr, |$ctx:ident, $param1:ident, $param2:ident| $body:expr) => {
        InvokableDefinition {
            name: $name.to_string(),
            infix: $infix,
            definition: |$ctx, xs: &[TracedExpr]| {
                let $param1 = xs
                    .get(0)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 2,
                        actual: 0,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                let $param2 = xs
                    .get(1)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 2,
                        actual: 1,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                $body
            },
        }
    };
}

pub fn ef3r_stdlib<'a>() -> Context<'a> {
    let mul = build_invokable!("*", true, |_ctx, first, second| {
        match (first.evaluated, second.evaluated) {
            (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x * y)),
            (actual, _) if !matches!(actual, Expr::Int(_)) => {
                Err(EvaluationError::TypeError {
                    expected: ExprType::Int,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "*".to_string(),
                })
            }
            (_, actual) => Err(EvaluationError::TypeError {
                expected: ExprType::Int,
                actual: type_of(&actual).unwrap(),
                at_loc: "*".to_string(),
            }),
        }
    });

    let and = build_invokable!("&&", true, |_ctx, first, second| {
        match (first.evaluated, second.evaluated) {
            (Expr::Bool(x), Expr::Bool(y)) => Ok(Expr::Bool(x && y)),
            (actual, _) if !matches!(actual, Expr::Int(_)) => {
                Err(EvaluationError::TypeError {
                    expected: ExprType::Bool,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "&&".to_string(),
                })
            }
            (_, actual) => Err(EvaluationError::TypeError {
                expected: ExprType::Bool,
                actual: type_of(&actual).unwrap(),
                at_loc: "&&".to_string(),
            }),
        }
    });

    let or = build_invokable!("||", true, |_ctx, first, second| {
        match (first.evaluated, second.evaluated) {
            (Expr::Bool(x), Expr::Bool(y)) => Ok(Expr::Bool(x || y)),
            (actual, _) if !matches!(actual, Expr::Int(_)) => {
                Err(EvaluationError::TypeError {
                    expected: ExprType::Bool,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "||".to_string(),
                })
            }
            (_, actual) => Err(EvaluationError::TypeError {
                expected: ExprType::Bool,
                actual: type_of(&actual).unwrap(),
                at_loc: "||".to_string(),
            }),
        }
    });

    let not = build_invokable!("not", false, |_ctx, first| {
        match first.evaluated {
            Expr::Bool(x) => Ok(Expr::Bool(!x)),
            actual => Err(EvaluationError::TypeError {
                expected: ExprType::Bool,
                actual: type_of(&actual).unwrap(),
                at_loc: "not".to_string(),
            }),
        }
    });

    let assert = build_invokable!("assert", false, |_ctx, first| {
        match first.evaluated {
            Expr::Bool(x) => {
                assert!(x);
                Ok(Expr::Unit)
            }
            actual => Err(EvaluationError::TypeError {
                expected: ExprType::Bool,
                actual: type_of(&actual).unwrap(),
                at_loc: "assert".to_string(),
            }),
        }
    });

    let add = build_invokable!("+", true, |_ctx, first, second| {
        match (first.evaluated, second.evaluated) {
            (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x + y)),
            (actual, _) if !matches!(actual, Expr::Int(_)) => {
                Err(EvaluationError::TypeError {
                    expected: ExprType::Int,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "+".to_string(),
                })
            }
            (_, actual) => Err(EvaluationError::TypeError {
                expected: ExprType::Int,
                actual: type_of(&actual).unwrap(),
                at_loc: "+".to_string(),
            }),
        }
    });

    let div = build_invokable!("/", true, |_ctx, first, second| {
        match (first.evaluated, second.evaluated) {
            (Expr::Int(x), Expr::Int(y)) => Ok(Expr::Int(x / y)),
            (actual, _) if !matches!(actual, Expr::Int(_)) => {
                Err(EvaluationError::TypeError {
                    expected: ExprType::Int,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "/".to_string(),
                })
            }
            (_, actual) => Err(EvaluationError::TypeError {
                expected: ExprType::Int,
                actual: type_of(&actual).unwrap(),
                at_loc: "/".to_string(),
            }),
        }
    });

    let append = build_invokable!("++", true, |_ctx, first, second| {
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
            }),
        }
    });

    let uppercase = build_invokable!("uppercase", false, |_ctx, first| {
        match first.evaluated {
            Expr::String(x) => Ok(Expr::String(x.to_uppercase())),
            actual => Err(EvaluationError::TypeError {
                expected: ExprType::String,
                actual: type_of(&actual).unwrap(),
                at_loc: "uppercase".to_string(),
            }),
        }
    });

    let type_of_fn = build_invokable!("type_of", false, |_ctx, first| {
        Ok(match type_of(&first.evaluated) {
            Some(x) => Expr::Type(x),
            None => Expr::None,
        })
    });

    let pair_first_fn = build_invokable!("first", false, |_ctx, pair| {
        match pair.evaluated {
            Expr::Pair(x, _) => Ok(x.evaluated),
            actual => Err(EvaluationError::TypeError {
                expected: ExprType::Pair(
                    Box::new(ExprType::Any),
                    Box::new(ExprType::Any),
                ),
                actual: type_of(&actual).unwrap(),
                at_loc: "first".to_string(),
            }),
        }
    });

    let pair_second_fn = build_invokable!("second", false, |_ctx, pair| {
        match pair.evaluated {
            Expr::Pair(_, y) => Ok(y.evaluated),
            actual => Err(EvaluationError::TypeError {
                expected: ExprType::Pair(
                    Box::new(ExprType::Any),
                    Box::new(ExprType::Any),
                ),
                actual: type_of(&actual).unwrap(),
                at_loc: "second".to_string(),
            }),
        }
    });

    let pair_fn = build_invokable!("pair", false, |_ctx, first, second| {
        Ok(Expr::Pair(Box::new(first), Box::new(second)))
    });

    let print_fn = build_invokable!("println", false, |_ctx, first| {
        println!("{}", first);
        Ok(Expr::None)
    });

    let readln_fn = InvokableDefinition {
        name: "readln".to_string(),
        infix: false,
        definition: |_, _: &[TracedExpr]| {
            let stdin = io::stdin();
            let result = stdin.lock().lines().next().unwrap().unwrap();

            Result::Ok(Expr::String(result))
        },
    };

    let update_node_fn =
        build_invokable!("update_node", false, |ctx, first, second| {
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
                }),
            }
        });

    let node_current_value_fn =
        build_invokable!("current_value", false, |ctx, first| {
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
                }),
            }
        });

    let new_node_fn =
        build_invokable!("new_node", false, |ctx, first, second| {
            match first.evaluated {
                Expr::Type(x) => {
                    if type_of(&second.evaluated) == Some(x.clone()) {
                        let update_fn =
                            Expr::BuiltinFunction(UPDATE_NODE_ID).traced();

                        let fresh_id = Node::new(
                            |_| {},
                            Arc::new(AtomicBool::new(false)),
                            &mut ctx.lock().unwrap().graph,
                            x,
                            second,
                        );

                        Ok(Expr::Pair(
                            Box::new(Expr::Node(fresh_id.index()).traced()),
                            Box::new(update_fn),
                        ))
                    } else {
                        Err(EvaluationError::TypeError {
                            expected: x,
                            actual: type_of(&second.evaluated).unwrap(),
                            at_loc: "new_node".to_string(),
                        })
                    }
                }
                actual => Err(EvaluationError::TypeError {
                    expected: ExprType::Type,
                    actual: type_of(&actual).unwrap(),
                    at_loc: "new_node".to_string(),
                }),
            }
        });

    let launch_fn = build_invokable!("launch", false, |ctx, first| {
        let thread_ctx = ctx.clone();

        thread::spawn(move || {
            evaluate_function_application(
                thread_ctx,
                &Expr::Apply(Box::new(first), Box::new([])),
            )
        });
        Ok(Expr::Unit)
    });

    let map_node_fn = build_invokable!("map", false, |ctx, first, second| {
        match first.evaluated {
            Expr::Node(node_id) => {
                let ctx_clone = ctx.clone();
                let second_clone = second.clone();
                let transform =
                    Arc::new(Mutex::new(move |expr: TracedExpr| {
                        evaluate_function_application(
                            ctx_clone.clone(),
                            &Expr::Apply(
                                Box::new(second_clone.clone()),
                                Box::new([expr]),
                            ),
                        )
                        .unwrap()
                    }));

                let fresh_id = map_node(
                    |_| {},
                    Arc::new(AtomicBool::new(false)),
                    ctx.clone(),
                    NodeIndex::new(node_id),
                    ExprType::Any,
                    transform,
                );

                Ok(Expr::Node(fresh_id.index()))
            }
            actual => Err(EvaluationError::TypeError {
                expected: ExprType::Node(Box::new(ExprType::Any)),
                actual: type_of(&actual).unwrap(),
                at_loc: "map_node".to_string(),
            }),
        }
    });

    let filter_node_fn =
        build_invokable!("filter", false, |ctx, first, second| {
            match first.evaluated {
                Expr::Node(node_id) => {
                    let ctx_clone = ctx.clone();
                    let second_clone = second.clone();
                    let transform = move |expr: TracedExpr| {
                        let result = evaluate_function_application(
                            ctx_clone.clone(),
                            &Expr::Apply(
                                Box::new(second_clone.clone()),
                                Box::new([expr]),
                            ),
                        )
                        .unwrap()
                        .evaluated;

                        match result {
                            Expr::Bool(x) => x,
                            _ => todo!(),
                        }
                    };

                    let fresh_id = filter_node(
                        |_| {},
                        Arc::new(AtomicBool::new(false)),
                        &mut ctx.lock().unwrap().graph,
                        NodeIndex::new(node_id),
                        Box::new(transform),
                    );

                    Ok(Expr::Node(fresh_id.index()))
                }
                actual => Err(EvaluationError::TypeError {
                    expected: ExprType::Node(Box::new(ExprType::Any)),
                    actual: type_of(&actual).unwrap(),
                    at_loc: "filter_node".to_string(),
                }),
            }
        });

    let fold_node_fn = build_invokable!("fold", false, |ctx, first, second| {
        match first.evaluated {
            Expr::Node(node_id) => {
                let ctx_clone = ctx.clone();
                let second_clone = second.clone();
                let transform =
                    Box::new(move |expr: TracedExpr, acc: TracedExpr| {
                        evaluate_function_application(
                            ctx_clone.clone(),
                            &Expr::Apply(
                                Box::new(second_clone.clone()),
                                Box::new([expr, acc]),
                            ),
                        )
                        .unwrap()
                    });

                let fresh_id = fold_node(
                    |_| {},
                    Arc::new(AtomicBool::new(false)),
                    &mut ctx.lock().unwrap().graph,
                    NodeIndex::new(node_id),
                    Expr::None.traced(),
                    transform,
                );

                Ok(Expr::Node(fresh_id.index()))
            }
            actual => Err(EvaluationError::TypeError {
                expected: ExprType::Node(Box::new(ExprType::Any)),
                actual: type_of(&actual).unwrap(),
                at_loc: "fold_node".to_string(),
            }),
        }
    });

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
                (PAIR_ID, pair_fn),
                (TYPE_OF_ID, type_of_fn),
                (AND_ID, and),
                (OR_ID, or),
                (NOT_ID, not),
                (ASSERT_ID, assert),
                (MAP_NODE_ID, map_node_fn),
                (FILTER_NODE_ID, filter_node_fn),
                (FOLD_NODE_ID, fold_node_fn),
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
