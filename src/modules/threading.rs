use std::thread;

use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    interpreter::{
        evaluate_function_application, EvaluationError, FunctionDefinition,
    },
    types::ExprType,
};

use super::Module;

pub fn threading_module<T: Debugger>() -> Module<2, T> {
    Module {
        package: "stdlib".to_string(),
        file_name: "threading.rs".to_string(),
        definitions: [
            build_function!(
                T,
                "launch",
                ExprType::Unit,
                vec![ExprType::Func(vec![], Box::new(ExprType::Unit))],
                |ctx, first| {
                    let thread_ctx = ctx.clone();

                    thread::spawn(move || {
                        evaluate_function_application(
                            thread_ctx,
                            &TracedExprRec::Apply(
                                Box::new(first),
                                Box::new([]),
                            ),
                        )
                    });
                    Ok(TracedExprRec::Unit)
                }
            ),
            build_function!(
                T,
                "delay",
                ExprType::Unit,
                vec![ExprType::Int],
                |_ctx, first| {
                    match first.evaluated {
                        TracedExprRec::Int(ms) => {
                            thread::sleep(std::time::Duration::from_millis(
                                ms as u64,
                            ));
                            Ok(TracedExprRec::Unit)
                        }
                        _ => unreachable!(),
                    }
                }
            ),
        ],
    }
}
