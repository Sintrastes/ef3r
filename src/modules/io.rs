use std::io::{self, BufRead};

use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    frp::with_lock,
    interpreter::{EvaluationError, FunctionDefinition},
    types::ExprType,
};

use super::Module;

pub fn io_module<T: Debugger>() -> Module<2, T> {
    Module {
        package: "stdlib".to_string(),
        file_name: "io.rs".to_string(),
        definitions: [
            build_function!(
                T,
                "print",
                ExprType::Unit,
                vec![ExprType::Any],
                |ctx, _ref, first| {
                    match first.evaluated {
                        TracedExprRec::String(string) => {
                            println!("{}", string);
                        }
                        _ => {
                            println!(
                                "{}",
                                ctx.expression_context
                                    .restore_symbols(first.evaluated)
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
                definition: |_, _, _: &[TracedExpr<usize>]| {
                    let stdin = io::stdin();
                    let result = stdin.lock().lines().next().unwrap().unwrap();

                    Result::Ok(TracedExprRec::String(result))
                },
            },
        ],
    }
}
