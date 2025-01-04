use crate::typechecking::RuntimeLookup;
use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    frp::with_lock,
    interpreter::{
        evaluate_function_application, EvaluationError, FunctionDefinition,
    },
    typechecking::type_of,
    types::ExprType,
};

use super::Module;

pub fn base_module<T: Debugger>() -> Module<9, T> {
    Module {
        package: "stdlib".to_string(),
        file_name: "base.rs".to_string(),
        definitions: [
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
                            actual: type_of::<_, _, RuntimeLookup>(
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
                            actual: type_of::<_, _, RuntimeLookup>(
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
                "loop",
                ExprType::Unit,
                vec![ExprType::Func(vec![], Box::new(ExprType::Unit))],
                |ctx, first| {
                    loop {
                        evaluate_function_application(
                            ctx.clone(),
                            &TracedExprRec::Apply(
                                Box::new(first.clone()),
                                Box::new([]),
                            ),
                        )
                        .unwrap()
                        .evaluated;
                    }
                }
            ),
            build_function!(
                T,
                "pair",
                ExprType::Pair(
                    Box::new(ExprType::Any),
                    Box::new(ExprType::Any)
                ),
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
                        match type_of::<_, _, RuntimeLookup>(
                            &ctx.lock().unwrap().expression_context,
                            &first.evaluated,
                        ) {
                            Some(x) => TracedExprRec::Type(x),
                            None => TracedExprRec::None,
                        },
                    )
                }
            ),
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
                "dbg_trace_full",
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
                        resolved.full_trace()
                    );
                    Ok(TracedExprRec::Unit)
                }
            ),
            build_function!(T, "%", ExprType::Int, |_cx, x: i32, y: i32| {
                Ok(x % y)
            }),
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
        ],
    }
}
