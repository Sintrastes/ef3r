use std::{
    collections::{BTreeMap, HashMap},
    io::{self, BufRead},
    sync::{atomic::AtomicBool, Arc, Mutex},
    thread,
};

use bimap::BiMap;
use daggy::{Dag, NodeIndex};

use crate::{
    ast::{
        raw_expr::{RawExpr, RawExprRec},
        traced_expr::{TracedExpr, TracedExprRec},
        Statement,
    },
    debugging::Debugger,
    extern_utils::{build_function, ExprTypeable},
    frp::{filter_node, fold_node, map_node, with_lock, Node},
    interpreter::{
        evaluate_function_application, Context, EvaluationError,
        ExpressionContext, FunctionDefinition, PolymorphicIndex,
    },
    typechecking::type_of,
    types::ExprType,
};

pub fn ef3r_stdlib<'a, T: Debugger + 'static>(
    debugger: T,
    symbol_table: BiMap<usize, String>,
) -> Context<'a, T> {
    let functions = Box::new([
        build_function!(T, "*", ExprType::Int, |_cx, x: i32, y: i32| {
            Ok(x * y)
        }),
        build_function!(T, "+", ExprType::Int, |_cx, x: i32, y: i32| {
            Ok(x + y)
        }),
        FunctionDefinition {
            name: "/".to_string(),
            argument_types: vec![ExprType::Int, ExprType::Int],
            result_type: ExprType::Int,
            definition: |ctx, args: &[TracedExpr<usize>]| {
                let x = match args[0].evaluated {
                    TracedExprRec::Int(i) => i,
                    _ => unreachable!(),
                };
                let y = match args[1].evaluated {
                    TracedExprRec::Int(i) => i,
                    _ => unreachable!(),
                };

                // Check for division by zero
                if y == 0 {
                    // Get full expression trace
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[1].clone())
                        .expression_trace();

                    panic!(
                        "\nError: division by zero. The denominator \
                            was 0 because of the runtime values: \n\n{}\n",
                        expr_trace
                    );
                }

                Ok(TracedExprRec::Int(x / y))
            },
        },
        build_function!(T, "-", ExprType::Int, |_cx, x: i32, y: i32| {
            Ok(x - y)
        }),
        build_function!(T, "*", ExprType::Float, |_cx, x: f32, y: f32| {
            Ok(x * y)
        }),
        build_function!(T, "+", ExprType::Float, |_cx, x: f32, y: f32| {
            Ok(x + y)
        }),
        FunctionDefinition {
            name: "/".to_string(),
            argument_types: vec![ExprType::Float, ExprType::Float],
            result_type: ExprType::Float,
            definition: |ctx, args: &[TracedExpr<usize>]| {
                let x = match args[0].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };
                let y = match args[1].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };

                // Check for division by zero
                if y == 0.0 {
                    // Get full expression trace
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[1].clone())
                        .expression_trace();

                    panic!(
                        "\nError: division by zero. The denominator \
                            was 0 because of the runtime values: \n\n{}\n",
                        expr_trace
                    );
                }

                Ok(TracedExprRec::Float(x / y))
            },
        },
        build_function!(T, "-", ExprType::Float, |_cx, x: f32, y: f32| {
            Ok(x - y)
        }),
        build_function!(
            T,
            "+",
            ExprType::String,
            |_cx, x: String, y: String| { Ok(x.to_owned() + y.as_ref()) }
        ),
        build_function!(T, "uppercase", ExprType::String, |_cx, x: String| {
            Ok(x.to_uppercase())
        }),
        build_function!(
            T,
            "first",
            ExprType::Any,
            vec![ExprType::Pair(
                Box::new(ExprType::Any),
                Box::new(ExprType::Any)
            )],
            |ctx, pair| {
                match pair.evaluated {
                    TracedExprRec::Pair(x, _) => Ok(x.evaluated),
                    actual => Err(EvaluationError::TypeError {
                        expected: ExprType::Pair(
                            Box::new(ExprType::Any),
                            Box::new(ExprType::Any),
                        ),
                        actual: type_of(
                            &ctx.lock().unwrap().expression_context,
                            &actual,
                        )
                        .unwrap(),
                        at_loc: "first".to_string(),
                    }),
                }
            }
        ),
        build_function!(
            T,
            "second",
            ExprType::Any,
            vec![ExprType::Pair(
                Box::new(ExprType::Any),
                Box::new(ExprType::Any)
            )],
            |ctx, pair| {
                match pair.evaluated {
                    TracedExprRec::Pair(_, y) => Ok(y.evaluated),
                    actual => Err(EvaluationError::TypeError {
                        expected: ExprType::Pair(
                            Box::new(ExprType::Any),
                            Box::new(ExprType::Any),
                        ),
                        actual: type_of(
                            &ctx.lock().unwrap().expression_context,
                            &actual,
                        )
                        .unwrap(),
                        at_loc: "second".to_string(),
                    }),
                }
            }
        ),
        build_function!(
            T,
            "print",
            ExprType::Unit,
            vec![ExprType::Any],
            |ctx, first| {
                match first.evaluated {
                    TracedExprRec::String(string) => {
                        println!("{}", string);
                    }
                    _ => {
                        println!(
                            "{}",
                            with_lock(ctx.as_ref(), |ctx| ctx
                                .expression_context
                                .restore_symbols(first.evaluated))
                            .untraced()
                            .as_expr()
                        );
                    }
                }
                Ok(TracedExprRec::None)
            }
        ),
        FunctionDefinition {
            name: "readln".to_string(),
            argument_types: vec![],
            result_type: ExprType::String,
            definition: |_, _: &[TracedExpr<usize>]| {
                let stdin = io::stdin();
                let result = stdin.lock().lines().next().unwrap().unwrap();

                Result::Ok(TracedExprRec::String(result))
            },
        },
        build_function!(
            T,
            "new_node",
            ExprType::Node(Box::new(ExprType::Any)),
            vec![ExprType::Type, ExprType::Any],
            |ctx, first, second| {
                match first.evaluated {
                    TracedExprRec::Type(x) => {
                        if type_of(
                            &ctx.lock().unwrap().expression_context,
                            &second.evaluated,
                        ) == Some(x.clone())
                        {
                            let fresh_id = Node::new(
                                |_| {},
                                Arc::new(AtomicBool::new(false)),
                                &mut ctx.lock().unwrap().graph,
                                x,
                                second,
                            );

                            with_lock(ctx.as_ref(), |lock| {
                                let node =
                                    lock.graph.node_weight(fresh_id).unwrap();

                                lock.debugger
                                    .on_node_added(node, fresh_id.index());
                            });

                            Ok(TracedExprRec::Node(fresh_id.index()))
                        } else {
                            Err(EvaluationError::TypeError {
                                expected: x,
                                actual: type_of(
                                    &ctx.lock().unwrap().expression_context,
                                    &second.evaluated,
                                )
                                .unwrap(),
                                at_loc: "new_node".to_string(),
                            })
                        }
                    }
                    actual => Err(EvaluationError::TypeError {
                        expected: ExprType::Type,
                        actual: type_of(
                            &ctx.lock().unwrap().expression_context,
                            &actual,
                        )
                        .unwrap(),
                        at_loc: "new_node".to_string(),
                    }),
                }
            }
        ),
        build_function!(
            T,
            "update_node",
            ExprType::Unit,
            vec![ExprType::Node(Box::new(ExprType::Any)), ExprType::Any],
            |ctx, first, second| {
                match first.evaluated {
                    TracedExprRec::Node(node_id) => {
                        ctx.lock()
                            .unwrap()
                            .graph
                            .node_weight_mut(NodeIndex::new(node_id))
                            .unwrap()
                            .update(second);

                        Ok(TracedExprRec::Unit)
                    }
                    actual => Err(EvaluationError::TypeError {
                        expected: ExprType::Node(Box::new(ExprType::Any)),
                        actual: type_of(
                            &ctx.lock().unwrap().expression_context,
                            &actual,
                        )
                        .unwrap(),
                        at_loc: "update_node".to_string(),
                    }),
                }
            }
        ),
        build_function!(
            T,
            "current_value",
            ExprType::Any,
            vec![ExprType::Node(Box::new(ExprType::Any))],
            |ctx, first| {
                match first.evaluated {
                    TracedExprRec::Node(node_id) => {
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
                        actual: type_of(
                            &ctx.lock().unwrap().expression_context,
                            &actual,
                        )
                        .unwrap(),
                        at_loc: "current_value".to_string(),
                    }),
                }
            }
        ),
        build_function!(
            T,
            "launch",
            ExprType::Unit,
            vec![ExprType::Func(vec![], Box::new(ExprType::Unit))],
            |ctx, first| {
                let thread_ctx = ctx.clone();

                thread::spawn(move || {
                    evaluate_function_application(
                        thread_ctx,
                        &TracedExprRec::Apply(Box::new(first), Box::new([])),
                    )
                });
                Ok(TracedExprRec::Unit)
            }
        ),
        build_function!(
            T,
            "pair",
            ExprType::Pair(Box::new(ExprType::Any), Box::new(ExprType::Any)),
            vec![ExprType::Any, ExprType::Any],
            |_cx, first, second| {
                Ok(TracedExprRec::Pair(Box::new(first), Box::new(second)))
            }
        ),
        build_function!(
            T,
            "type_of",
            ExprType::Type,
            vec![ExprType::Any],
            |ctx, first| {
                Ok(
                    match type_of(
                        &ctx.lock().unwrap().expression_context,
                        &first.evaluated,
                    ) {
                        Some(x) => TracedExprRec::Type(x),
                        None => TracedExprRec::None,
                    },
                )
            }
        ),
        build_function!(T, "&&", ExprType::Bool, |_cx, x: bool, y: bool| {
            Ok(x && y)
        }),
        build_function!(T, "||", ExprType::Bool, |_cx, x: bool, y: bool| {
            Ok(x || y)
        }),
        build_function!(T, "not", ExprType::Bool, |_cx, x: bool| { Ok(!x) }),
        FunctionDefinition {
            name: "assert".to_string(),
            argument_types: vec![ExprType::Bool],
            result_type: ExprType::Unit,
            definition: |ctx, args: &[TracedExpr<usize>]| {
                let condition = match args[0].evaluated {
                    TracedExprRec::Bool(b) => b,
                    _ => unreachable!(),
                };

                if !condition {
                    // Get full expression trace
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[0].clone())
                        .expression_trace();

                    panic!(
                        "\nAssertion failed. The condition evaluated to false \
                            because of the runtime values: \n\n{}\n",
                        expr_trace
                    );
                }

                Ok(TracedExprRec::Unit)
            },
        },
        build_function!(
            T,
            "map",
            ExprType::Node(Box::new(ExprType::Any)),
            vec![
                ExprType::Node(Box::new(ExprType::Any)),
                // TODO: Typechecking does not currently work in this case, so we
                // can't dispatch on the second argument.
                // ExprType::Func(
                //     vec![ExprType::Any],
                //     Box::new(ExprType::Any)
                // )
            ],
            |ctx, first, second| {
                match first.evaluated {
                    TracedExprRec::Node(node_id) => {
                        let ctx_clone = ctx.clone();
                        let second_clone = second.clone();
                        let transform = Arc::new(Mutex::new(
                            move |expr: TracedExpr<usize>| {
                                evaluate_function_application(
                                    ctx_clone.clone(),
                                    &TracedExprRec::Apply(
                                        Box::new(second_clone.clone()),
                                        Box::new([expr]),
                                    ),
                                )
                                .unwrap()
                            },
                        ));

                        let fresh_id = map_node(
                            |_| {},
                            Arc::new(AtomicBool::new(false)),
                            ctx.clone(),
                            NodeIndex::new(node_id),
                            // TODO: Actually get type of function here.
                            ExprType::Any,
                            transform,
                        );

                        with_lock(ctx.as_ref(), |lock| {
                            let node =
                                lock.graph.node_weight(fresh_id).unwrap();

                            lock.debugger.on_node_added(node, fresh_id.index());
                        });

                        Ok(TracedExprRec::Node(fresh_id.index()))
                    }
                    actual => Err(EvaluationError::TypeError {
                        expected: ExprType::Node(Box::new(ExprType::Any)),
                        actual: type_of(
                            &ctx.lock().unwrap().expression_context,
                            &actual,
                        )
                        .unwrap(),
                        at_loc: "map_node".to_string(),
                    }),
                }
            }
        ),
        build_function!(
            T,
            "filter",
            ExprType::Node(Box::new(ExprType::Any)),
            vec![
                ExprType::Node(Box::new(ExprType::Any)),
                ExprType::Func(vec![ExprType::Any], Box::new(ExprType::Bool))
            ],
            |ctx, first, second| {
                match first.evaluated {
                    TracedExprRec::Node(node_id) => {
                        let ctx_clone = ctx.clone();
                        let second_clone = second.clone();
                        let transform = move |expr: TracedExpr<usize>| {
                            let result = evaluate_function_application(
                                ctx_clone.clone(),
                                &TracedExprRec::Apply(
                                    Box::new(second_clone.clone()),
                                    Box::new([expr]),
                                ),
                            )
                            .unwrap()
                            .evaluated;

                            match result {
                                TracedExprRec::Bool(x) => x,
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

                        with_lock(ctx.as_ref(), |lock| {
                            let node =
                                lock.graph.node_weight(fresh_id).unwrap();

                            lock.debugger.on_node_added(node, fresh_id.index());
                        });

                        Ok(TracedExprRec::Node(fresh_id.index()))
                    }
                    actual => Err(EvaluationError::TypeError {
                        expected: ExprType::Node(Box::new(ExprType::Any)),
                        actual: with_lock(ctx.as_ref(), |lock| {
                            type_of(&lock.expression_context, &actual).unwrap()
                        }),
                        at_loc: "filter_node".to_string(),
                    }),
                }
            }
        ),
        build_function!(
            T,
            "fold",
            ExprType::Node(Box::new(ExprType::Any)),
            vec![
                ExprType::Node(Box::new(ExprType::Any)),
                // ExprType::Func(
                //     vec![ExprType::Any, ExprType::Any],
                //     Box::new(ExprType::Any)
                // )
            ],
            |ctx, first, second| {
                match first.evaluated {
                    TracedExprRec::Node(node_id) => {
                        let ctx_clone = ctx.clone();
                        let second_clone = second.clone();
                        let transform = Box::new(
                                move |expr: TracedExpr<usize>, acc: TracedExpr<usize>| {
                                    evaluate_function_application(
                                        ctx_clone.clone(),
                                        &TracedExprRec::Apply(
                                            Box::new(second_clone.clone()),
                                            Box::new([expr, acc]),
                                        ),
                                    )
                                    .unwrap()
                                },
                            );

                        let fresh_id = fold_node(
                            ctx.clone(),
                            |_| {},
                            Arc::new(AtomicBool::new(false)),
                            NodeIndex::new(node_id),
                            TracedExprRec::None.traced(),
                            transform,
                        );

                        with_lock(ctx.as_ref(), |lock| {
                            let node =
                                lock.graph.node_weight(fresh_id).unwrap();

                            lock.debugger.on_node_added(node, fresh_id.index());
                        });

                        Ok(TracedExprRec::Node(fresh_id.index()))
                    }
                    actual => Err(EvaluationError::TypeError {
                        expected: ExprType::Node(Box::new(ExprType::Any)),
                        actual: with_lock(ctx.as_ref(), |lock| {
                            type_of(&lock.expression_context, &actual).unwrap()
                        }),
                        at_loc: "fold_node".to_string(),
                    }),
                }
            }
        ),
        build_function!(
            T,
            "dbg_trace_full",
            ExprType::Unit,
            vec![ExprType::Any],
            |ctx, first| {
                let resolved = with_lock(ctx.as_ref(), |lock| {
                    lock.expression_context
                        .restore_symbols_traced(first.clone())
                });
                println!("{} = {}", resolved.untraced(), resolved.full_trace());
                Ok(TracedExprRec::Unit)
            }
        ),
        build_function!(T, "lowercase", ExprType::String, |_cx, x: String| {
            Ok(x.to_lowercase())
        }),
        build_function!(
            T,
            "drop",
            ExprType::List(Box::new(ExprType::Any)),
            |_cx, list: Vec<TracedExpr<usize>>, n: i32| {
                Ok(if n <= 0 {
                    list
                } else {
                    list.into_iter().skip(n as usize).collect()
                })
            }
        ),
        build_function!(
            T,
            "drop_last",
            ExprType::List(Box::new(ExprType::Any)),
            |_cx, list: Vec<TracedExpr<usize>>, n: i32| {
                Ok(if n <= 0 {
                    list
                } else {
                    let length = &list.len();
                    list.into_iter()
                        .take(length.saturating_sub(n as usize))
                        .collect()
                })
            }
        ),
        build_function!(
            T,
            "+",
            ExprType::List(Box::new(ExprType::Any)),
            |_cx,
             list1: Vec<TracedExpr<usize>>,
             list2: Vec<TracedExpr<usize>>| {
                let mut result = list1;
                result.extend(list2);
                Ok(result)
            }
        ),
        build_function!(
            T,
            "map",
            ExprType::List(Box::new(ExprType::Any)),
            vec![
                ExprType::List(Box::new(ExprType::Any)),
                // TODO: Typechecking does not currently work in this case, so we
                // can't dispatch on the second argument.
                // ExprType::Func(
                //    vec![ExprType::Any],
                //    Box::new(ExprType::Any)
                //)
            ],
            |ctx, list, f| {
                match list.evaluated {
                    TracedExprRec::List(elements) => {
                        let mapped = elements
                            .into_iter()
                            .map(|e| {
                                evaluate_function_application(
                                    ctx.clone(),
                                    &TracedExprRec::Apply(
                                        Box::new(f.clone()),
                                        Box::new([e]),
                                    ),
                                )
                                .unwrap()
                            })
                            .collect();
                        Ok(TracedExprRec::List(mapped))
                    }
                    _ => unreachable!(),
                }
            }
        ),
        build_function!(
            T,
            "filter",
            ExprType::List(Box::new(ExprType::Any)),
            vec![
                ExprType::List(Box::new(ExprType::Any)),
                ExprType::Func(vec![ExprType::Any], Box::new(ExprType::Bool))
            ],
            |ctx, list, pred| {
                match list.evaluated {
                    TracedExprRec::List(elements) => {
                        let filtered = elements
                            .into_iter()
                            .filter(|e| {
                                match evaluate_function_application(
                                    ctx.clone(),
                                    &TracedExprRec::Apply(
                                        Box::new(pred.clone()),
                                        Box::new([e.clone()]),
                                    ),
                                )
                                .unwrap()
                                .evaluated
                                {
                                    TracedExprRec::Bool(b) => b,
                                    _ => unreachable!(),
                                }
                            })
                            .collect();
                        Ok(TracedExprRec::List(filtered))
                    }
                    _ => unreachable!(),
                }
            }
        ),
        build_function!(
            T,
            "fold",
            ExprType::Any,
            vec![
                ExprType::List(Box::new(ExprType::Any)),
                // ExprType::Any,
                // ExprType::Func(
                //     vec![ExprType::Any, ExprType::Any],
                //     Box::new(ExprType::Any)
                // )
            ],
            |ctx, list, init, f| {
                match list.evaluated {
                    TracedExprRec::List(elements) => {
                        let result =
                            elements.into_iter().fold(init, |acc, e| {
                                evaluate_function_application(
                                    ctx.clone(),
                                    &TracedExprRec::Apply(
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
        ),
        build_function!(T, "first", ExprType::Any, |_cx,
                                                    list: Vec<
            TracedExpr<usize>,
        >| {
            Ok(list
                .first()
                .map(|x| x.evaluated.clone())
                .unwrap_or(TracedExprRec::None))
        }),
        build_function!(T, "last", ExprType::Any, |_cx,
                                                   list: Vec<
            TracedExpr<usize>,
        >| {
            Ok(list
                .last()
                .map(|x| x.evaluated.clone())
                .unwrap_or(TracedExprRec::None))
        }),
        FunctionDefinition {
            name: "list".to_string(),
            argument_types: vec![], // Vararg function
            result_type: ExprType::List(Box::new(ExprType::Any)),
            definition: |_, xs: &[TracedExpr<usize>]| {
                Ok(TracedExprRec::List(xs.to_vec()))
            },
        },
        build_function!(
            T,
            "length",
            ExprType::Int,
            vec![ExprType::List(Box::new(ExprType::Any))],
            |_ctx, first| {
                match first.evaluated {
                    TracedExprRec::List(xs) => {
                        Ok(TracedExprRec::Int(xs.len() as i32))
                    }
                    _ => unreachable!(),
                }
            }
        ),
        build_function!(
            T,
            "length",
            ExprType::Int,
            vec![ExprType::String],
            |_ctx, first| {
                match first.evaluated {
                    TracedExprRec::String(s) => {
                        Ok(TracedExprRec::Int(s.len() as i32))
                    }
                    _ => unreachable!(),
                }
            }
        ),
        build_function!(
            T,
            "split",
            ExprType::List(Box::new(ExprType::String)),
            vec![ExprType::String, ExprType::String],
            |_ctx, first, second| {
                match (first.evaluated, second.evaluated) {
                    (
                        TracedExprRec::String(s),
                        TracedExprRec::String(delim),
                    ) => {
                        let split = s
                            .split(&delim)
                            .map(|s| {
                                TracedExprRec::String(s.to_string()).traced()
                            })
                            .collect();
                        Ok(TracedExprRec::List(split))
                    }
                    _ => unreachable!(),
                }
            }
        ),
        build_function!(
            T,
            "intersperse",
            ExprType::List(Box::new(ExprType::Any)),
            |_cx,
             list: Vec<TracedExpr<usize>>,
             separator: TracedExprRec<usize>| {
                if list.is_empty() {
                    return Ok(TracedExprRec::List(list));
                }

                let mut result = Vec::with_capacity(list.len() * 2 - 1);

                for (i, item) in list.into_iter().enumerate() {
                    if i > 0 {
                        result.push(separator.clone().traced());
                    }
                    result.push(item);
                }

                Ok(TracedExprRec::List(result))
            }
        ),
        build_function!(T, "%", ExprType::Int, |_cx, x: i32, y: i32| {
            Ok(x % y)
        }),
        build_function!(
            T,
            "==",
            ExprType::Bool,
            |_cx, x: TracedExprRec<usize>, y: TracedExprRec<usize>| {
                Ok(x == y)
            }
        ),
        build_function!(
            T,
            "dbg_trace",
            ExprType::Unit,
            vec![ExprType::Any],
            |ctx, first| {
                let resolved = with_lock(ctx.as_ref(), |lock| {
                    lock.expression_context
                        .restore_symbols_traced(first.clone())
                });
                println!(
                    "{} = {}",
                    resolved.untraced(),
                    resolved.get_trace().untraced()
                );
                Ok(TracedExprRec::Unit)
            }
        ),
        build_function!(
            T,
            "log",
            ExprType::Float,
            vec![ExprType::Int, ExprType::Float],
            |ctx, base, number| {
                match (&base.evaluated, &number.evaluated) {
                    (
                        TracedExprRec::Int(base_value),
                        TracedExprRec::Float(num_value),
                    ) => {
                        if *base_value <= 0 {
                            let expr_trace = ctx
                                .lock()
                                .unwrap()
                                .expression_context
                                .restore_symbols_traced(base.clone())
                                .expression_trace();

                            panic!(
                        "\nError: logarithm base must be positive. The base \
                        was {} because of runtime values: \n\n{}\n",
                        base_value,
                        expr_trace
                    );
                        }
                        if *num_value <= 0.0 {
                            let expr_trace = ctx
                                .lock()
                                .unwrap()
                                .expression_context
                                .restore_symbols_traced(number.clone())
                                .expression_trace();

                            panic!(
                        "\nError: logarithm argument must be positive. The number \
                        was {} because of runtime values: \n\n{}\n",
                        num_value,
                        expr_trace
                    );
                        }

                        Ok(TracedExprRec::Float(
                            num_value.ln() / (*base_value as f32).ln(),
                        ))
                    }
                    _ => unreachable!(),
                }
            }
        ),
        FunctionDefinition {
            name: "asin".to_string(),
            argument_types: vec![ExprType::Float],
            result_type: ExprType::Float,
            definition: |ctx, args: &[TracedExpr<usize>]| {
                let x = match args[0].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };

                if x < -1.0 || x > 1.0 {
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[0].clone())
                        .expression_trace();

                    panic!(
            "\nError: arcsin input must be between -1 and 1. The input \
            was {} because of runtime values: \n\n{}\n",
            x,
            expr_trace
        );
                }

                Ok(TracedExprRec::Float(x.asin()))
            },
        },
        FunctionDefinition {
            name: "acos".to_string(),
            argument_types: vec![ExprType::Float],
            result_type: ExprType::Float,
            definition: |ctx, args: &[TracedExpr<usize>]| {
                let x = match args[0].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };

                if x < -1.0 || x > 1.0 {
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[0].clone())
                        .expression_trace();

                    panic!(
                            "\nError: arccos input must be between -1 and 1. The input \
                            was {} because of runtime values: \n\n{}\n",
                            x,
                            expr_trace
                        );
                }

                Ok(TracedExprRec::Float(x.acos()))
            },
        },
        FunctionDefinition {
            name: "tan".to_string(),
            argument_types: vec![ExprType::Float],
            result_type: ExprType::Float,
            definition: |ctx, args: &[TracedExpr<usize>]| {
                let x = match args[0].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };

                let cos = x.cos();
                if cos.abs() < f32::EPSILON {
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[0].clone())
                        .expression_trace();

                    panic!(
                        "\nError: tangent undefined at π/2 + nπ. The input \
                            was {} because of runtime values: \n\n{}\n",
                        x, expr_trace
                    );
                }

                Ok(TracedExprRec::Float(x.tan()))
            },
        },
        FunctionDefinition {
            name: "cot".to_string(),
            argument_types: vec![ExprType::Float],
            result_type: ExprType::Float,
            definition: |ctx, args: &[TracedExpr<usize>]| {
                let x = match args[0].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };

                let sin = x.sin();
                if sin.abs() < f32::EPSILON {
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[0].clone())
                        .expression_trace();

                    panic!(
                        "\nError: cotangent undefined at nπ. The input \
                            was {} because of runtime values: \n\n{}\n",
                        x, expr_trace
                    );
                }

                Ok(TracedExprRec::Float(1.0 / x.tan()))
            },
        },
        FunctionDefinition {
            name: "tanh".to_string(),
            argument_types: vec![ExprType::Float],
            result_type: ExprType::Float,
            definition: |_ctx, args: &[TracedExpr<usize>]| {
                let x = match args[0].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };

                Ok(TracedExprRec::Float(x.tanh()))
            },
        },
        FunctionDefinition {
            name: "cosh".to_string(),
            argument_types: vec![ExprType::Float],
            result_type: ExprType::Float,
            definition: |_ctx, args: &[TracedExpr<usize>]| {
                let x = match args[0].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };

                Ok(TracedExprRec::Float(x.cosh()))
            },
        },
        FunctionDefinition {
            name: "acosh".to_string(),
            argument_types: vec![ExprType::Float],
            result_type: ExprType::Float,
            definition: |ctx, args: &[TracedExpr<usize>]| {
                let x = match args[0].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };

                if x < 1.0 {
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[0].clone())
                        .expression_trace();

                    panic!(
                        "\nError: acosh undefined for values < 1. The input \
                            was {} because of runtime values: \n\n{}\n",
                        x, expr_trace
                    );
                }

                Ok(TracedExprRec::Float(x.acosh()))
            },
        },
        FunctionDefinition {
            name: "atanh".to_string(),
            argument_types: vec![ExprType::Float],
            result_type: ExprType::Float,
            definition: |ctx, args: &[TracedExpr<usize>]| {
                let x = match args[0].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };

                if x <= -1.0 || x >= 1.0 {
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[0].clone())
                        .expression_trace();

                    panic!(
                        "\nError: atanh undefined outside (-1, 1). The input \
                            was {} because of runtime values: \n\n{}\n",
                        x, expr_trace
                    );
                }

                Ok(TracedExprRec::Float(x.atanh()))
            },
        },
        FunctionDefinition {
            name: "pow".to_string(),
            argument_types: vec![ExprType::Float, ExprType::Float],
            result_type: ExprType::Float,
            definition: |ctx, args: &[TracedExpr<usize>]| {
                let base = match args[0].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };
                let exp = match args[1].evaluated {
                    TracedExprRec::Float(i) => i,
                    _ => unreachable!(),
                };

                if base == 0.0 && exp < 0.0 {
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[0].clone())
                        .expression_trace();

                    panic!(
                        "\nError: zero base with negative exponent. The base \
                            was {} because of runtime values: \n\n{}\n",
                        base, expr_trace
                    );
                }

                if base < 0.0 && !exp.fract().is_normal() {
                    let expr_trace = ctx
                        .lock()
                        .unwrap()
                        .expression_context
                        .restore_symbols_traced(args[0].clone())
                        .expression_trace();

                    panic!(
                            "\nError: negative base with non-integer exponent. The base \
                            was {} because of runtime values: \n\n{}\n",
                            base,
                            expr_trace
                        );
                }

                Ok(TracedExprRec::Float(base.powf(exp)))
            },
        },
    ]);

    // Lookup table for the interpreter
    Context {
        debugger,
        expression_context: ExpressionContext {
            symbol_table,
            polymorphic_functions: build_polymorphic_index(functions.as_ref())
                .unwrap(),
            functions,
            variables: HashMap::new(),
        },
        graph: Dag::new(),
    }
}

fn build_polymorphic_index<T: Debugger + 'static>(
    functions: &[FunctionDefinition<T>],
) -> Result<HashMap<PolymorphicIndex, usize>, String> {
    // Needs to be stable, otherwise the IDs will be nondeterministic.
    let mut name_buckets: BTreeMap<
        String,
        Vec<(usize, &FunctionDefinition<T>)>,
    > = BTreeMap::new();

    for id in 0..functions.len() - 1 {
        let func = &functions[id];
        name_buckets
            .entry(func.name.clone())
            .or_default()
            .push((id, func));
    }

    let mut polymorphic_id = 0;
    let mut result = HashMap::new();

    for (name, funcs) in name_buckets.into_iter() {
        // Skip functions with only 1 implementation
        if funcs.len() <= 1 {
            continue;
        }

        for (id, func) in funcs {
            let index = PolymorphicIndex {
                id: polymorphic_id,
                arg_types: func.argument_types.clone(),
            };

            if result.insert(index.clone(), id).is_some() {
                return Err(format!(
                    "Multiple functions named \"{}\" with signature {:?}",
                    name, index.arg_types
                ));
            }
        }

        polymorphic_id += 1;
    }

    Ok(result)
}

pub fn get_stdlib_functions<'a, T: Debugger + 'static>(
    stdlib: &'a Context<T>,
) -> HashMap<usize, usize> {
    stdlib
        .expression_context
        .functions
        .iter()
        .enumerate()
        .flat_map(|(id, invokable)| {
            let symbol_id = stdlib
                .expression_context
                .symbol_table
                .get_by_right(invokable.name.as_str())?;

            Some((*symbol_id, id))
        })
        .collect()
}

pub fn get_stdlib_polymorphic_functions<'a, T: Debugger + 'static>(
    stdlib: &'a Context<T>,
) -> HashMap<usize, usize> {
    stdlib
        .expression_context
        .polymorphic_functions
        .iter()
        .flat_map(|(id, func_id)| {
            let polymorhpic_fn_name =
                stdlib.expression_context.functions[*func_id].name.as_str();

            let symbol_id = stdlib
                .expression_context
                .symbol_table
                .get_by_right(polymorhpic_fn_name)?;

            Some((*symbol_id, id.id))
        })
        .collect()
}

///
/// Utility function to replace raw variable IDs for functions
///  with their corresponding function IDs.
///
pub fn resolve_builtin_functions(
    statements: Vec<Statement<usize>>,
    polymorphic_functions: &HashMap<usize, usize>,
    stdlib_functions: &HashMap<usize, usize>,
) -> Vec<Statement<usize>> {
    statements
        .into_iter()
        .map(|stmt| {
            resolve_functions_in_statement(
                stmt,
                polymorphic_functions,
                stdlib_functions,
            )
        })
        .collect()
}

fn resolve_functions_in_statement(
    stmt: Statement<usize>,
    polymorphic_functions: &HashMap<usize, usize>,
    stdlib_functions: &HashMap<usize, usize>,
) -> Statement<usize> {
    Statement {
        location: stmt.location,
        var: stmt.var,
        expr: resolve_functions_in_expr_raw(
            stmt.expr,
            polymorphic_functions,
            stdlib_functions,
        ),
    }
}

fn resolve_functions_in_expr_raw(
    expr: RawExpr<usize>,
    polymorphic_functions: &HashMap<usize, usize>,
    stdlib_functions: &HashMap<usize, usize>,
) -> RawExpr<usize> {
    RawExpr {
        location: expr.location,
        expr: match expr.expr {
            RawExprRec::Var(var_name) => {
                if let Some(poly_func_id) = polymorphic_functions.get(&var_name)
                {
                    RawExprRec::PolymorphicFunction(*poly_func_id)
                } else if let Some(func_id) = stdlib_functions.get(&var_name) {
                    RawExprRec::BuiltinFunction(*func_id)
                } else {
                    RawExprRec::Var(var_name)
                }
            }
            RawExprRec::Apply(func, args) => RawExprRec::Apply(
                Box::new(resolve_functions_in_expr_raw(
                    *func,
                    polymorphic_functions,
                    stdlib_functions,
                )),
                args.into_vec()
                    .into_iter()
                    .map(|a| {
                        resolve_functions_in_expr_raw(
                            a,
                            polymorphic_functions,
                            stdlib_functions,
                        )
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            RawExprRec::Pair(first, second) => RawExprRec::Pair(
                Box::new(resolve_functions_in_expr_raw(
                    *first,
                    polymorphic_functions,
                    stdlib_functions,
                )),
                Box::new(resolve_functions_in_expr_raw(
                    *second,
                    polymorphic_functions,
                    stdlib_functions,
                )),
            ),
            RawExprRec::Lambda(vars, stmts, body) => RawExprRec::Lambda(
                vars,
                resolve_builtin_functions(
                    stmts,
                    polymorphic_functions,
                    stdlib_functions,
                ),
                Box::new(resolve_functions_in_expr_raw(
                    *body,
                    polymorphic_functions,
                    stdlib_functions,
                )),
            ),
            _ => expr.expr,
        },
    }
}
