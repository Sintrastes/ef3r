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

pub fn bool_module<T: Debugger>() -> Module<4, T> {
    Module {
        package: "stdlib".to_string(),
        file_name: "bool.rs".to_string(),
        definitions: [
            build_function!(T, "==", ExprType::Bool, |_cx,
                                                      _ref,
                                                      x: TracedExprRec<
                usize,
            >,
                                                      y: TracedExprRec<
                usize,
            >| {
                Ok(x == y)
            }),
            build_function!(
                T,
                "&&",
                ExprType::Bool,
                |_cx, _ref, x: bool, y: bool| { Ok(x && y) }
            ),
            build_function!(
                T,
                "||",
                ExprType::Bool,
                |_cx, _ref, x: bool, y: bool| { Ok(x || y) }
            ),
            build_function!(T, "not", ExprType::Bool, |_cx, _ref, x: bool| {
                Ok(!x)
            }),
        ],
    }
}
