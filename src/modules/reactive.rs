use std::sync::Arc;

use parking_lot::Mutex;

use daggy::NodeIndex;

use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    frp::{filter_node, fold_node, map_node, Node},
    interpreter::{
        evaluate_function_application, Context, EvaluationError,
        FunctionDefinition, VariableId,
    },
    typechecking::type_of,
    types::ExprType,
};

use super::{Module, ModuleName};

pub fn reactive_module<T: Debugger + Send + Sync>() -> Module<6, T> {
    Module {
        package: "stdlib".to_string(),
        name: ModuleName::new("reactive"),
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
                                &ctx.expression_context.read(),
                                &second.evaluated,
                            ) == Some(x.clone())
                            {
                                let fresh_id = Node::new(
                                    Arc::new(|_, _| {}),
                                    &mut ctx.graph.lock(),
                                    x,
                                    second,
                                );

                                let graph = ctx.graph.lock();

                                let node = graph.node_weight(fresh_id).unwrap();

                                ctx.debugger
                                    .lock()
                                    .on_node_added(node, fresh_id.index());

                                Ok(TracedExprRec::Node(fresh_id.index()))
                            } else {
                                Err(EvaluationError::TypeError {
                                    expected: x,
                                    actual: type_of(
                                        &ctx.expression_context.read(),
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
                                &ctx.expression_context.read(),
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
                            Node::update(NodeIndex::new(node_id), ctx, second);

                            Ok(TracedExprRec::Unit)
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: type_of(
                                &ctx.expression_context.read(),
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
                            let graph = ctx.graph.lock();
                            let node: &Node<T> = graph
                                .node_weight(NodeIndex::new(node_id))
                                .unwrap();

                            let value = node.current();

                            Ok(value.evaluated)
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: type_of(
                                &ctx.expression_context.read(),
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
                            let second_clone = second.clone();
                            let transform = Arc::new(Mutex::new(
                                move |ctx: &Context<T>, expr: TracedExpr<VariableId>| {
                                    evaluate_function_application(
                                        ctx,
                                        &TracedExprRec::Apply(
                                            Box::new(second_clone.clone()),
                                            Box::new([expr]),
                                        ),
                                    )
                                    .unwrap()
                                },
                            ));

                            let fresh_id = map_node(
                                Arc::new(|_, _| {}),
                                ctx,
                                NodeIndex::new(node_id),
                                // TODO: Actually get type of function here.
                                ExprType::Any,
                                transform,
                            );

                            let graph = ctx.graph.lock();

                            let node = graph.node_weight(fresh_id).unwrap();

                            ctx.debugger
                                .lock()
                                .on_node_added(node, fresh_id.index());

                            Ok(TracedExprRec::Node(fresh_id.index()))
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: type_of(
                                &ctx.expression_context.read(),
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
                            let second_clone = second.clone();

                            let transform = move |ctx: &Context<T>,
                                                  expr: TracedExpr<
                                VariableId,
                            >| {
                                let result = evaluate_function_application(
                                    ctx,
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

                            let fresh_id = filter_node::<T>(
                                Arc::new(|_, _| {}),
                                &mut ctx.graph.lock(),
                                NodeIndex::new(node_id),
                                Box::new(transform),
                            );

                            let graph = ctx.graph.lock();

                            let node = graph.node_weight(fresh_id).unwrap();

                            ctx.debugger
                                .lock()
                                .on_node_added(node, fresh_id.index());

                            Ok(TracedExprRec::Node(fresh_id.index()))
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: type_of(
                                &ctx.expression_context.read(),
                                &actual,
                            )
                            .unwrap(),
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
                            let second_clone = second.clone();
                            let transform = Box::new(
                                    move |ctx: &Context<T>, expr: TracedExpr<VariableId>, acc: TracedExpr<VariableId>| {
                                        evaluate_function_application(
                                            &ctx.clone(),
                                            &TracedExprRec::Apply(
                                                Box::new(second_clone.clone()),
                                                Box::new([expr, acc]),
                                            ),
                                        )
                                        .unwrap()
                                    },
                                );

                            let fresh_id = fold_node(
                                ctx,
                                Arc::new(|_, _| {}),
                                NodeIndex::new(node_id),
                                TracedExprRec::None.traced(),
                                transform,
                            );

                            let graph = ctx.graph.lock();

                            let node = graph.node_weight(fresh_id).unwrap();

                            ctx.debugger
                                .lock()
                                .on_node_added(node, fresh_id.index());

                            Ok(TracedExprRec::Node(fresh_id.index()))
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: type_of(
                                &ctx.expression_context.read(),
                                &actual,
                            )
                            .unwrap(),
                            at_loc: "fold_node".to_string(),
                        }),
                    }
                }
            ),
        ],
    }
}
