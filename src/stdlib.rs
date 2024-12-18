use std::{
    collections::HashMap,
    io::{self, BufRead},
    sync::{atomic::AtomicBool, Arc, Mutex},
    thread,
};

use daggy::{Dag, NodeIndex};

use crate::{
    ast::{Expr, ExprTypeable, Statement, TracedExpr},
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
pub const DEBUG_TRACE_FULL_ID: u32 = 23;
pub const LOWERCASE_ID: u32 = 24;
pub const DROP_LIST_ID: u32 = 25;
pub const DROP_LAST_LIST_ID: u32 = 26;
pub const LENGTH_LIST_ID: u32 = 27;
pub const APPEND_LISTS_ID: u32 = 28;
pub const MAP_LIST_ID: u32 = 29;
pub const FILTER_LIST_ID: u32 = 30;
pub const FOLD_LIST_ID: u32 = 31;
pub const FIRST_LIST_ID: u32 = 32;
pub const LAST_LIST_ID: u32 = 33;
pub const LIST_ID: u32 = 34;

macro_rules! build_invokable {
    // Pattern for single argument function with type checking
    ($name:expr, $res_type:expr, |$ctx:ident, $param:ident: $type:ty| $body:expr) => {
        InvokableDefinition {
            argument_types: vec![<$type>::expr_type()],
            result_type: $res_type,
            name: $name.to_string(),
            definition: |$ctx, xs: &[TracedExpr]| {
                let expr = xs.get(0).ok_or(
                    EvaluationError::WrongNumberOfArguments {
                        expected: 1,
                        actual: 0,
                        for_function: $name.to_string(),
                    },
                )?;

                let $param = <$type>::try_from_expr(&expr.evaluated).ok_or(
                    EvaluationError::TypeError {
                        expected: <$type>::expr_type(),
                        actual: type_of(&expr.evaluated)
                            .unwrap_or(ExprType::Any),
                        at_loc: $name.to_string(),
                    },
                )?;

                $body.map(|x| x.to_expr())
            },
        }
    };

    // Pattern for two argument function with type checking
    ($name:expr, $res_type:expr, |$ctx:ident, $param1:ident: $type1:ty, $param2:ident: $type2:ty| $body:expr) => {
        InvokableDefinition {
            name: $name.to_string(),
            argument_types: vec![<$type1>::expr_type(), <$type2>::expr_type()],
            result_type: $res_type,
            definition: |$ctx, xs: &[TracedExpr]| {
                let expr1 = xs.get(0).ok_or(
                    EvaluationError::WrongNumberOfArguments {
                        expected: 2,
                        actual: 0,
                        for_function: $name.to_string(),
                    },
                )?;

                let expr2 = xs.get(1).ok_or(
                    EvaluationError::WrongNumberOfArguments {
                        expected: 2,
                        actual: 1,
                        for_function: $name.to_string(),
                    },
                )?;

                let $param1 = <$type1>::try_from_expr(&expr1.evaluated).ok_or(
                    EvaluationError::TypeError {
                        expected: <$type1>::expr_type(),
                        actual: type_of(&expr1.evaluated)
                            .unwrap_or(ExprType::Any),
                        at_loc: $name.to_string(),
                    },
                )?;

                let $param2 = <$type2>::try_from_expr(&expr2.evaluated).ok_or(
                    EvaluationError::TypeError {
                        expected: <$type2>::expr_type(),
                        actual: type_of(&expr2.evaluated)
                            .unwrap_or(ExprType::Any),
                        at_loc: $name.to_string(),
                    },
                )?;

                $body.map(|x| x.to_expr())
            },
        }
    };

    // Pattern for single argument function without type checking
    ($name:expr, $res_type:expr, $arg_types:expr, |$ctx:ident, $param:ident| $body:expr) => {
        InvokableDefinition {
            name: $name.to_string(),
            argument_types: $arg_types,
            result_type: $res_type,
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

    // Pattern for two argument function without type checking.
    ($name:expr, $res_type:expr, $arg_types:expr, |$ctx:ident, $param1:ident, $param2:ident| $body:expr) => {
        InvokableDefinition {
            name: $name.to_string(),
            argument_types: $arg_types,
            result_type: $res_type,
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

    // Pattern for three argument function without type checking.
    ($name:expr, $res_type:expr, $arg_types:expr, |$ctx:ident, $param1:ident, $param2:ident, $param3:ident| $body:expr) => {
        InvokableDefinition {
            name: $name.to_string(),
            argument_types: $arg_types,
            result_type: $res_type,
            definition: |$ctx, xs: &[TracedExpr]| {
                let $param1 = xs
                    .get(0)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 3,
                        actual: 0,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                let $param2 = xs
                    .get(1)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 3,
                        actual: 1,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                let $param3 = xs
                    .get(2)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 3,
                        actual: 2,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                $body
            },
        }
    };
}

pub fn ef3r_stdlib<'a>() -> Context<'a> {
    let mul = build_invokable!("*", ExprType::Int, |_cx, x: i32, y: i32| {
        Ok(x * y)
    });

    let and =
        build_invokable!("&&", ExprType::Bool, |_cx, x: bool, y: bool| {
            Ok(x && y)
        });

    let or = build_invokable!("||", ExprType::Bool, |_cx, x: bool, y: bool| {
        Ok(x || y)
    });

    let not =
        build_invokable!("not", ExprType::Bool, |_cx, x: bool| { Ok(!x) });

    let assert = build_invokable!("assert", ExprType::Unit, |_cx, x: bool| {
        assert!(x);
        Ok(())
    });

    let add = build_invokable!("+", ExprType::Int, |_cx, x: i32, y: i32| {
        Ok(x + y)
    });

    let div = build_invokable!("/", ExprType::Int, |_cx, x: i32, y: i32| {
        Ok(x / y)
    });

    let append = build_invokable!(
        "++",
        ExprType::String,
        |_cx, x: String, y: String| { Ok(x.to_owned() + y.as_ref()) }
    );

    let uppercase =
        build_invokable!("uppercase", ExprType::String, |_cx, x: String| {
            Ok(x.to_uppercase())
        });

    let lowercase =
        build_invokable!("lowercase", ExprType::String, |_cx, x: String| {
            Ok(x.to_lowercase())
        });

    let drop_list_fn = build_invokable!(
        "drop",
        ExprType::List(Box::new(ExprType::Any)),
        |_cx, list: Vec<TracedExpr>, n: i32| {
            Ok(if n <= 0 {
                list
            } else {
                list.into_iter().skip(n as usize).collect()
            })
        }
    );

    let drop_last_list_fn = build_invokable!(
        "drop_last",
        ExprType::List(Box::new(ExprType::Any)),
        |_cx, list: Vec<TracedExpr>, n: i32| {
            Ok(if n <= 0 {
                list
            } else {
                let length = &list.len();
                list.into_iter()
                    .take(length.saturating_sub(n as usize))
                    .collect()
            })
        }
    );

    let list_fn = InvokableDefinition {
        name: "list".to_string(),
        argument_types: vec![], // Vararg function
        result_type: ExprType::List(Box::new(ExprType::Any)),
        definition: |_, xs: &[TracedExpr]| Ok(Expr::List(xs.to_vec())),
    };

    let length_list_fn = build_invokable!(
        "length",
        ExprType::Int,
        |_cx, list: Vec<TracedExpr>| { Ok(list.len() as i32) }
    );

    let append_lists_fn = build_invokable!(
        "++",
        ExprType::List(Box::new(ExprType::Any)),
        |_cx, list1: Vec<TracedExpr>, list2: Vec<TracedExpr>| {
            let mut result = list1;
            result.extend(list2);
            Ok(result)
        }
    );

    let map_list_fn = build_invokable!(
        "map",
        ExprType::List(Box::new(ExprType::Any)),
        vec![
            ExprType::List(Box::new(ExprType::Any)),
            ExprType::Func(vec![ExprType::Any], Box::new(ExprType::Any))
        ],
        |ctx, list, f| {
            match list.evaluated {
                Expr::List(elements) => {
                    let mapped = elements
                        .into_iter()
                        .map(|e| {
                            evaluate_function_application(
                                ctx.clone(),
                                &Expr::Apply(
                                    Box::new(f.clone()),
                                    Box::new([e]),
                                ),
                            )
                            .unwrap()
                        })
                        .collect();
                    Ok(Expr::List(mapped))
                }
                _ => unreachable!(),
            }
        }
    );

    let filter_list_fn = build_invokable!(
        "filter",
        ExprType::List(Box::new(ExprType::Any)),
        vec![
            ExprType::List(Box::new(ExprType::Any)),
            ExprType::Func(vec![ExprType::Any], Box::new(ExprType::Bool))
        ],
        |ctx, list, pred| {
            match list.evaluated {
                Expr::List(elements) => {
                    let filtered = elements
                        .into_iter()
                        .filter(|e| {
                            match evaluate_function_application(
                                ctx.clone(),
                                &Expr::Apply(
                                    Box::new(pred.clone()),
                                    Box::new([e.clone()]),
                                ),
                            )
                            .unwrap()
                            .evaluated
                            {
                                Expr::Bool(b) => b,
                                _ => unreachable!(),
                            }
                        })
                        .collect();
                    Ok(Expr::List(filtered))
                }
                _ => unreachable!(),
            }
        }
    );

    let fold_list_fn = build_invokable!(
        "fold",
        ExprType::Any,
        vec![
            ExprType::List(Box::new(ExprType::Any)),
            ExprType::Any,
            ExprType::Func(
                vec![ExprType::Any, ExprType::Any],
                Box::new(ExprType::Any)
            )
        ],
        |ctx, list, init, f| {
            match list.evaluated {
                Expr::List(elements) => {
                    let result = elements.into_iter().fold(init, |acc, e| {
                        evaluate_function_application(
                            ctx.clone(),
                            &Expr::Apply(
                                Box::new(f.clone()),
                                Box::new([e, acc]),
                            ),
                        )
                        .unwrap()
                    });
                    Ok(result.evaluated)
                }
                _ => unreachable!(),
            }
        }
    );

    let first_list_fn = build_invokable!(
        "first",
        ExprType::Any,
        |_cx, list: Vec<TracedExpr>| {
            Ok(list
                .first()
                .map(|x| x.evaluated.clone())
                .unwrap_or(Expr::None))
        }
    );

    let last_list_fn = build_invokable!("last", ExprType::Any, |_cx,
                                                                list: Vec<
        TracedExpr,
    >| {
        Ok(list
            .last()
            .map(|x| x.evaluated.clone())
            .unwrap_or(Expr::None))
    });

    let type_of_fn = build_invokable!(
        "type_of",
        ExprType::Type,
        vec![ExprType::Any],
        |_cx, first| {
            Ok(match type_of(&first.evaluated) {
                Some(x) => Expr::Type(x),
                None => Expr::None,
            })
        }
    );

    let pair_first_fn = build_invokable!(
        "first",
        ExprType::Any,
        vec![ExprType::Pair(
            Box::new(ExprType::Any),
            Box::new(ExprType::Any)
        )],
        |_cx, pair| {
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
        }
    );

    let pair_second_fn = build_invokable!(
        "second",
        ExprType::Any,
        vec![ExprType::Pair(
            Box::new(ExprType::Any),
            Box::new(ExprType::Any)
        )],
        |_cx, pair| {
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
        }
    );

    let pair_fn = build_invokable!(
        "pair",
        ExprType::Pair(Box::new(ExprType::Any), Box::new(ExprType::Any)),
        vec![ExprType::Any, ExprType::Any],
        |_cx, first, second| {
            Ok(Expr::Pair(Box::new(first), Box::new(second)))
        }
    );

    let print_fn = build_invokable!(
        "println",
        ExprType::Unit,
        vec![ExprType::Any],
        |_cx, first| {
            println!("{}", first);
            Ok(Expr::None)
        }
    );

    let readln_fn = InvokableDefinition {
        name: "readln".to_string(),
        argument_types: vec![],
        result_type: ExprType::String,
        definition: |_, _: &[TracedExpr]| {
            let stdin = io::stdin();
            let result = stdin.lock().lines().next().unwrap().unwrap();

            Result::Ok(Expr::String(result))
        },
    };

    let update_node_fn = build_invokable!(
        "update_node",
        ExprType::Unit,
        vec![ExprType::Node(Box::new(ExprType::Any)), ExprType::Any],
        |ctx, first, second| {
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
        }
    );

    let node_current_value_fn = build_invokable!(
        "current_value",
        ExprType::Any,
        vec![ExprType::Node(Box::new(ExprType::Any))],
        |ctx, first| {
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
        }
    );

    let new_node_fn = build_invokable!(
        "new_node",
        ExprType::Pair(
            Box::new(ExprType::Node(Box::new(ExprType::Any))),
            Box::new(ExprType::Any)
        ),
        vec![ExprType::Type, ExprType::Any],
        |ctx, first, second| {
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
        }
    );

    let launch_fn = build_invokable!(
        "launch",
        ExprType::Unit,
        vec![ExprType::Func(vec![], Box::new(ExprType::Unit))],
        |ctx, first| {
            let thread_ctx = ctx.clone();

            thread::spawn(move || {
                evaluate_function_application(
                    thread_ctx,
                    &Expr::Apply(Box::new(first), Box::new([])),
                )
            });
            Ok(Expr::Unit)
        }
    );

    let map_node_fn = build_invokable!(
        "map",
        ExprType::Node(Box::new(ExprType::Any)),
        vec![
            ExprType::Node(Box::new(ExprType::Any)),
            ExprType::Func(vec![ExprType::Any], Box::new(ExprType::Any))
        ],
        |ctx, first, second| {
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
        }
    );

    let filter_node_fn = build_invokable!(
        "filter",
        ExprType::Node(Box::new(ExprType::Any)),
        vec![
            ExprType::Node(Box::new(ExprType::Any)),
            ExprType::Func(vec![ExprType::Any], Box::new(ExprType::Bool))
        ],
        |ctx, first, second| {
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
        }
    );

    let fold_node_fn = build_invokable!(
        "fold",
        ExprType::Node(Box::new(ExprType::Any)),
        vec![
            ExprType::Node(Box::new(ExprType::Any)),
            ExprType::Func(
                vec![ExprType::Any, ExprType::Any],
                Box::new(ExprType::Any)
            )
        ],
        |ctx, first, second| {
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
        }
    );

    let dbg_trace_full_fn = build_invokable!(
        "dbg_trace_full",
        ExprType::Unit,
        vec![ExprType::Any],
        |_ctx, first| {
            println!("{}", first.get_trace());
            Ok(Expr::Unit)
        }
    );

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
                (DEBUG_TRACE_FULL_ID, dbg_trace_full_fn),
                (LOWERCASE_ID, lowercase),
                (DROP_LIST_ID, drop_list_fn),
                (DROP_LAST_LIST_ID, drop_last_list_fn),
                (LENGTH_LIST_ID, length_list_fn),
                (APPEND_LISTS_ID, append_lists_fn),
                (MAP_LIST_ID, map_list_fn),
                (FILTER_LIST_ID, filter_list_fn),
                (FOLD_LIST_ID, fold_list_fn),
                // TODO: Currently conflicts with pair's first.
                // Need to implement dynamic dispatch for this to work.
                // (FIRST_LIST_ID, first_list_fn),
                (LAST_LIST_ID, last_list_fn),
                (LIST_ID, list_fn),
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
