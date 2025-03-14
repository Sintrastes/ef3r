use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    interpreter::{EvaluationError, FunctionDefinition, VariableId},
    typechecking::type_of,
    typechecking::TypingContext,
    types::ExprType,
};

use super::{Module, ModuleName};

pub fn strings_module<T: Debugger>() -> Module<4, T> {
    Module {
        package: "stdlib".to_string(),
        name: ModuleName::new("string"),
        definitions: [
            build_function!(
                T,
                "uppercase",
                ExprType::String,
                |_cx, x: String| { Ok(x.to_uppercase()) }
            ),
            build_function!(
                T,
                "lowercase",
                ExprType::String,
                |_cx, x: String| { Ok(x.to_lowercase()) }
            ),
            build_function!(
                T,
                "length",
                ExprType::Int,
                vec![ExprType::String],
                |_ctx, first| {
                    match first.evaluated {
                        TracedExprRec::String(s) => {
                            Ok(TracedExprRec::Int(s.len() as i32))
                        }
                        _ => unreachable!(),
                    }
                }
            ),
            build_function!(
                T,
                "split",
                ExprType::List(Box::new(ExprType::String)),
                vec![ExprType::String, ExprType::String],
                |_ctx, first, second| {
                    match (first.evaluated, second.evaluated) {
                        (
                            TracedExprRec::String(s),
                            TracedExprRec::String(delim),
                        ) => {
                            let split = s
                                .split(&delim)
                                .map(|s| {
                                    TracedExprRec::String(s.to_string())
                                        .traced()
                                })
                                .collect();
                            Ok(TracedExprRec::List(split))
                        }
                        _ => unreachable!(),
                    }
                }
            ),
        ],
    }
}
