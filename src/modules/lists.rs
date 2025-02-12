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

pub fn lists_module<T: Debugger>() -> Module<11, T> {
    Module {
        package: "stdlib".to_string(),
        name: ModuleName::new("list"),
        definitions: [
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
                                        ctx,
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
                    ExprType::Func(
                        vec![ExprType::Any],
                        Box::new(ExprType::Bool)
                    )
                ],
                |ctx, list, pred| {
                    match list.evaluated {
                        TracedExprRec::List(elements) => {
                            let filtered = elements
                                .into_iter()
                                .filter(
                                    |e| match evaluate_function_application(
                                        ctx,
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
                                    },
                                )
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
                                        ctx,
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
                        _ => Err(EvaluationError::TypeError {
                            expected: ExprType::List(Box::new(ExprType::Any)),
                            actual: ExprType::Any,
                            at_loc: "length".to_string(),
                        }),
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
        ],
    }
}
