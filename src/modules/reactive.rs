use std::sync::{atomic::AtomicBool, Arc, Mutex};

use daggy::NodeIndex;

use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    frp::{filter_node, fold_node, map_node, with_lock, Node},
    interpreter::{
        evaluate_function_application, EvaluationError, FunctionDefinition,
    },
    typechecking::type_of,
    types::ExprType,
};

use super::Module;

pub fn reactive_module<T: Debugger>() -> Module<6, T> {
    Module {
        package: "stdlib".to_string(),
        file_name: "reactive.rs".to_string(),
        definitions: [
            build_function!(
                T,
                "reactive",
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
                                    let node = lock
                                        .graph
                                        .node_weight(fresh_id)
                                        .unwrap();

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
                                    at_loc: "reactive".to_string(),
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

                                lock.debugger
                                    .on_node_added(node, fresh_id.index());
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
                    ExprType::Func(
                        vec![ExprType::Any],
                        Box::new(ExprType::Bool)
                    )
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

                                lock.debugger
                                    .on_node_added(node, fresh_id.index());
                            });

                            Ok(TracedExprRec::Node(fresh_id.index()))
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: with_lock(ctx.as_ref(), |lock| {
                                type_of(&lock.expression_context, &actual)
                                    .unwrap()
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

                                lock.debugger
                                    .on_node_added(node, fresh_id.index());
                            });

                            Ok(TracedExprRec::Node(fresh_id.index()))
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: with_lock(ctx.as_ref(), |lock| {
                                type_of(&lock.expression_context, &actual)
                                    .unwrap()
                            }),
                            at_loc: "fold_node".to_string(),
                        }),
                    }
                }
            ),
        ],
    }
}
