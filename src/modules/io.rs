use std::io::{self, BufRead};

use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    interpreter::{EvaluationError, FunctionDefinition, VariableId},
    typechecking::TypingContext,
    types::ExprType,
};

use super::{Module, ModuleName};

pub fn io_module<T: Debugger>() -> Module<2, T> {
    Module {
        package: "stdlib".to_string(),
        name: ModuleName::new("io"),
        definitions: [
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
                                ctx.expression_context
                                    .read()
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
                definition: |_, _: &[TracedExpr<VariableId>]| {
                    let stdin = io::stdin();
                    let result = stdin.lock().lines().next().unwrap().unwrap();

                    Result::Ok(TracedExprRec::String(result))
                },
            },
        ],
    }
}
