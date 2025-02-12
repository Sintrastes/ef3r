use crate::typechecking::RuntimeLookup;
use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::Debugger,
    extern_utils::*,
    interpreter::{EvaluationError, FunctionDefinition},
    typechecking::type_of,
    types::ExprType,
};

use super::{Module, ModuleName};

pub fn bool_module<T: Debugger>() -> Module<4, T> {
    Module {
        package: "stdlib".to_string(),
        name: ModuleName::new("bool"),
        definitions: [
            build_function!(T, "==", ExprType::Bool, |_cx,

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
                |_cx, x: bool, y: bool| { Ok(x && y) }
            ),
            build_function!(
                T,
                "||",
                ExprType::Bool,
                |_cx, x: bool, y: bool| { Ok(x || y) }
            ),
            build_function!(T, "not", ExprType::Bool, |_cx, x: bool| {
                Ok(!x)
            }),
        ],
    }
}
