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
        FunctionDefinition,
    },
    typechecking::{type_of, RuntimeLookup},
    types::ExprType,
};

use super::Module;

pub fn reactive_module<T: Debugger + Send + Sync>() -> Module<6, T> {
    Module {
        package: "stdlib".to_string(),
        file_name: "reactive.rs".to_string(),
        definitions: [
            build_function!(
                T,
                "reactive",
                ExprType::Node(Box::new(ExprType::Any)),
                vec![ExprType::Type, ExprType::Any],
                |ctx, _ref, first, second| {
                    match first.evaluated {
                        TracedExprRec::Type(x) => {
                            if type_of::<_, _, RuntimeLookup>(
                                &ctx.expression_context,
                                &second.evaluated,
                            ) == Some(x.clone())
                            {
                                let fresh_id = Node::new(
                                    Arc::new(|_, _| {}),
                                    &mut ctx.graph,
                                    x,
                                    second,
                                );

                                let node =
                                    ctx.graph.node_weight(fresh_id).unwrap();

                                ctx.debugger
                                    .on_node_added(node, fresh_id.index());

                                Ok(TracedExprRec::Node(fresh_id.index()))
                            } else {
                                Err(EvaluationError::TypeError {
                                    expected: x,
                                    actual: type_of::<_, _, RuntimeLookup>(
                                        &ctx.expression_context,
                                        &second.evaluated,
                                    )
                                    .unwrap(),
                                    at_loc: "reactive".to_string(),
                                })
                            }
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Type,
                            actual: type_of::<_, _, RuntimeLookup>(
                                &ctx.expression_context,
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
                |ctx, _ref, first, second| {
                    match first.evaluated {
                        TracedExprRec::Node(node_id) => {
                            Node::update(NodeIndex::new(node_id), ctx, second);

                            Ok(TracedExprRec::Unit)
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: type_of::<_, _, RuntimeLookup>(
                                &ctx.expression_context,
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
                |ctx, _ref, first| {
                    match first.evaluated {
                        TracedExprRec::Node(node_id) => {
                            let node: &Node<T> = ctx
                                .graph
                                .node_weight(NodeIndex::new(node_id))
                                .unwrap();

                            let value = node.current();

                            Ok(value.evaluated)
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: type_of::<_, _, RuntimeLookup>(
                                &ctx.expression_context,
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
                |ctx, _ref, first, second| {
                    match first.evaluated {
                        TracedExprRec::Node(node_id) => {
                            let second_clone = second.clone();
                            let transform = Arc::new(Mutex::new(
                                move |ctx: &mut Context<T>, expr: TracedExpr<usize>| {
                                    evaluate_function_application(
                                        ctx,
                                        _ref.clone(),
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

                            let node = ctx.graph.node_weight(fresh_id).unwrap();

                            ctx.debugger.on_node_added(node, fresh_id.index());

                            Ok(TracedExprRec::Node(fresh_id.index()))
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: type_of::<_, _, RuntimeLookup>(
                                &ctx.expression_context,
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
                |ctx, _ref, first, second| {
                    match first.evaluated {
                        TracedExprRec::Node(node_id) => {
                            let second_clone = second.clone();

                            let transform = move |ctx: &mut Context<T>, expr: TracedExpr<usize>| {
                                let result = evaluate_function_application(
                                    ctx,
                                    _ref.clone(),
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
                                &mut ctx.graph,
                                NodeIndex::new(node_id),
                                Box::new(transform),
                            );

                            let node = ctx.graph.node_weight(fresh_id).unwrap();

                            ctx.debugger.on_node_added(node, fresh_id.index());

                            Ok(TracedExprRec::Node(fresh_id.index()))
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: type_of::<_, _, RuntimeLookup>(
                                &ctx.expression_context,
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
                |ctx, _ref, first, second| {
                    match first.evaluated {
                        TracedExprRec::Node(node_id) => {
                            let second_clone = second.clone();
                            let transform = Box::new(
                                    move |ctx: &mut Context<T>, expr: TracedExpr<usize>, acc: TracedExpr<usize>| {
                                        evaluate_function_application(
                                            ctx,
                                            _ref.clone(),
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

                            let node = ctx.graph.node_weight(fresh_id).unwrap();

                            ctx.debugger.on_node_added(node, fresh_id.index());

                            Ok(TracedExprRec::Node(fresh_id.index()))
                        }
                        actual => Err(EvaluationError::TypeError {
                            expected: ExprType::Node(Box::new(ExprType::Any)),
                            actual: type_of::<_, _, RuntimeLookup>(
                                &ctx.expression_context,
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
