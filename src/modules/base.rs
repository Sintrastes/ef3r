use crate::ast::raw_expr::{substitute_statement, substitute_traced};
use crate::typechecking::RuntimeLookup;
use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    interpreter::{
        evaluate_function_application, EvaluationError, FunctionDefinition,
    },
    typechecking::type_of,
    types::ExprType,
};

use super::{Module, ModuleName};

pub fn base_module<T: Debugger>() -> Module<10, T> {
    Module {
        package: "stdlib".to_string(),
        name: ModuleName::new("base"),
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
                                &ctx.expression_context.read(),
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
                                &ctx.expression_context.read(),
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
                            ctx,
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
                            &ctx.expression_context.read(),
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
                            .expression_context
                            .read()
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
                    let resolved = ctx
                        .expression_context
                        .read()
                        .restore_symbols_traced(first.clone());
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
            FunctionDefinition {
                name: "with".to_string(),
                argument_types: vec![],
                result_type: ExprType::Any,
                definition: |_, args| partial_application::<T>(args),
            },
            build_function!(
                T,
                "dbg_trace",
                ExprType::Unit,
                vec![ExprType::Any],
                |ctx, first| {
                    let resolved = ctx
                        .expression_context
                        .read()
                        .restore_symbols_traced(first.clone());
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

fn partial_application<T: Debugger>(
    args: &[TracedExpr<usize>],
) -> Result<TracedExprRec<usize>, EvaluationError> {
    let function = ensure_eta_expanded(args[0].clone());

    let args_to_apply = &args[1..];

    match &function.evaluated {
        TracedExprRec::Lambda(lambda_args, stmts, body) => {
            if lambda_args.len() <= args_to_apply.len() {
                panic!("Cannot partially apply. Too many arguments.");
            }

            let subst = lambda_args.iter().zip(args_to_apply.iter());

            let new_args = lambda_args
                .into_iter()
                .skip(args_to_apply.len())
                .map(|x| *x)
                .collect::<Vec<usize>>();

            let new_body = Box::new(
                subst.clone().fold(*body.to_owned(), |expr, (var, with)| {
                    substitute_traced(var, with, expr)
                }),
            );

            let new_stmts = stmts
                .into_iter()
                .map(|stmt| {
                    subst.clone().fold(stmt.clone(), |stmt, (var, with)| {
                        substitute_statement(var, &with.untraced(), stmt)
                    })
                })
                .collect::<Vec<_>>();

            Result::Ok(TracedExprRec::Lambda(new_args, new_stmts, new_body))
        }
        // Impossible because ensure_eta_expanded only outputs lambdas.
        _ => unreachable!(),
    }
}

///
/// Ensures that a function expression is eta-expanded.
///
fn ensure_eta_expanded(expr: TracedExpr<usize>) -> TracedExpr<usize> {
    match &expr.evaluated {
        TracedExprRec::Lambda(_, _, _) => expr,
        TracedExprRec::Apply(_f, _x) => todo!(),
        _ => expr, // Not a function, so leave alone.
    }
}
