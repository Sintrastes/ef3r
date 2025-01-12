use crate::typechecking::RuntimeLookup;
use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    frp::with_lock,
    interpreter::{EvaluationError, FunctionDefinition},
    typechecking::type_of,
    types::ExprType,
};

use super::Module;

pub fn math_module<T: Debugger>() -> Module<19, T> {
    Module {
        package: "stdlib".to_string(),
        file_name: "math.rs".to_string(),
        definitions: [
            build_function!(
                T,
                "*",
                ExprType::Int,
                |_cx, _ref, x: i32, y: i32| { Ok(x * y) }
            ),
            build_function!(
                T,
                "+",
                ExprType::Int,
                |_cx, _ref, x: i32, y: i32| { Ok(x + y) }
            ),
            FunctionDefinition {
                name: "/".to_string(),
                argument_types: vec![ExprType::Int, ExprType::Int],
                result_type: ExprType::Int,
                definition: |ctx, _ref, args: &[TracedExpr<usize>]| {
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
            build_function!(
                T,
                "-",
                ExprType::Int,
                |_cx, _ref, x: i32, y: i32| { Ok(x - y) }
            ),
            build_function!(
                T,
                "*",
                ExprType::Float,
                |_cx, _ref, x: f32, y: f32| { Ok(x * y) }
            ),
            build_function!(
                T,
                "+",
                ExprType::Float,
                |_cx, _ref, x: f32, y: f32| { Ok(x + y) }
            ),
            FunctionDefinition {
                name: "/".to_string(),
                argument_types: vec![ExprType::Float, ExprType::Float],
                result_type: ExprType::Float,
                definition: |ctx, _ref, args: &[TracedExpr<usize>]| {
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
            build_function!(
                T,
                "-",
                ExprType::Float,
                |_cx, _ref, x: f32, y: f32| { Ok(x - y) }
            ),
            build_function!(
                T,
                "+",
                ExprType::String,
                |_cx, _ref, x: String, y: String| {
                    Ok(x.to_owned() + y.as_ref())
                }
            ),
            build_function!(
                T,
                "log",
                ExprType::Float,
                vec![ExprType::Int, ExprType::Float],
                |ctx, _ref, base, number| {
                    match (&base.evaluated, &number.evaluated) {
                        (
                            TracedExprRec::Int(base_value),
                            TracedExprRec::Float(num_value),
                        ) => {
                            if *base_value <= 0 {
                                let expr_trace = ctx
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
                definition: |ctx, _ref, args: &[TracedExpr<usize>]| {
                    let x = match args[0].evaluated {
                        TracedExprRec::Float(i) => i,
                        _ => unreachable!(),
                    };

                    if x < -1.0 || x > 1.0 {
                        let expr_trace = ctx
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
                definition: |ctx, _ref, args: &[TracedExpr<usize>]| {
                    let x = match args[0].evaluated {
                        TracedExprRec::Float(i) => i,
                        _ => unreachable!(),
                    };

                    if x < -1.0 || x > 1.0 {
                        let expr_trace = ctx
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
                definition: |ctx, _ref, args: &[TracedExpr<usize>]| {
                    let x = match args[0].evaluated {
                        TracedExprRec::Float(i) => i,
                        _ => unreachable!(),
                    };

                    let cos = x.cos();
                    if cos.abs() < f32::EPSILON {
                        let expr_trace = ctx
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
                definition: |ctx, _ref, args: &[TracedExpr<usize>]| {
                    let x = match args[0].evaluated {
                        TracedExprRec::Float(i) => i,
                        _ => unreachable!(),
                    };

                    let sin = x.sin();
                    if sin.abs() < f32::EPSILON {
                        let expr_trace = ctx
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
                definition: |_ctx, _ref, args: &[TracedExpr<usize>]| {
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
                definition: |_ctx, _ref, args: &[TracedExpr<usize>]| {
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
                definition: |ctx, _ref, args: &[TracedExpr<usize>]| {
                    let x = match args[0].evaluated {
                        TracedExprRec::Float(i) => i,
                        _ => unreachable!(),
                    };

                    if x < 1.0 {
                        let expr_trace = ctx
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
                definition: |ctx, _ref, args: &[TracedExpr<usize>]| {
                    let x = match args[0].evaluated {
                        TracedExprRec::Float(i) => i,
                        _ => unreachable!(),
                    };

                    if x <= -1.0 || x >= 1.0 {
                        let expr_trace = ctx
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
                definition: |ctx, _ref, args: &[TracedExpr<usize>]| {
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
        ],
    }
}
