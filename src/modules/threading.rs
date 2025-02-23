use std::{sync::Arc, thread};

use daggy::NodeIndex;

use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    interpreter::{
        evaluate_function_application, Context, EvaluationError,
        FunctionDefinition, VariableId,
    },
    typechecking::{type_of, TypingContext},
    types::ExprType,
};

use super::{Module, ModuleName};

pub fn threading_module<T: Debugger + Sync>() -> Module<4, T> {
    Module {
        package: "stdlib".to_string(),
        name: ModuleName::new("threading"),
        definitions: [
            build_function!(
                T,
                "launch",
                ExprType::Unit,
                vec![ExprType::Func(vec![], Box::new(ExprType::Unit))],
                |ctx, first| {
                    let handle = {
                        let ctx = ctx.clone();
                        thread::spawn(move || {
                            evaluate_function_application(
                                &ctx,
                                &TracedExprRec::Apply(
                                    Box::new(first),
                                    Box::new([]),
                                ),
                            )
                            .map(|_| ())
                        })
                    };

                    ctx.join_handles.lock().push(handle);

                    let new_id = ctx.join_handles.lock().len();

                    Ok(TracedExprRec::JoinHandle(new_id))
                }
            ),
            build_function!(
                T,
                "on_update",
                ExprType::Unit,
                vec![
                    ExprType::Any,
                    ExprType::Func(vec![], Box::new(ExprType::Unit))
                ],
                |ctx, node, on_update_fn| {
                    match &node.evaluated {
                        TracedExprRec::Node(node_id) => {
                            let mut graph = ctx.graph.lock();

                            let node = graph
                                .node_weight_mut(NodeIndex::new(*node_id))
                                .unwrap();

                            println!("Value of node: {:?}", &node.current());

                            let mut on_update = node.on_update.lock();

                            let old_update = on_update.clone();

                            *on_update = Arc::new(
                                move |ctx: &Context<T>, value: TracedExpr<VariableId>| {
                                    old_update(ctx, value.clone());

                                    evaluate_function_application(
                                        ctx,
                                        &TracedExprRec::Apply(
                                            Box::new(on_update_fn.clone()),
                                            Box::new([value]),
                                        ),
                                    )
                                    .unwrap();
                                },
                            );

                            Ok(TracedExprRec::Unit)
                        }
                        _ => Err(EvaluationError::TypeError {
                            expected: ExprType::Any,
                            actual: ExprType::Any,
                            at_loc: "on_update".to_string(),
                        }),
                    }
                }
            ),
            build_function!(
                T,
                "await",
                ExprType::Unit,
                vec![ExprType::Any],
                |ctx, handle| {
                    match handle.evaluated {
                        TracedExprRec::JoinHandle(id) => {
                            let handle = ctx.join_handles.lock().remove(id - 1);
                            match handle.join() {
                                Ok(Ok(())) => Ok(TracedExprRec::Unit),
                                Ok(Err(e)) => Err(e),
                                Err(_) => Err(EvaluationError::ThreadError),
                            }
                        }
                        _ => Err(EvaluationError::TypeError {
                            expected: ExprType::Any,
                            actual: ExprType::Any,
                            at_loc: "await".to_string(),
                        }),
                    }
                }
            ),
            build_function!(
                T,
                "delay",
                ExprType::Unit,
                vec![ExprType::Int],
                |ctx, first| {
                    match first.evaluated {
                        TracedExprRec::Int(ms) => {
                            thread::sleep(std::time::Duration::from_millis(
                                ms as u64,
                            ));
                            Ok(TracedExprRec::Unit)
                        }
                        _ => Err(EvaluationError::TypeError {
                            expected: ExprType::Int,
                            actual: type_of(
                                &ctx.expression_context.read(),
                                &TypingContext::new(),
                                &first.evaluated,
                            )
                            .unwrap(),
                            at_loc: "delay".to_string(),
                        }),
                    }
                }
            ),
        ],
    }
}
