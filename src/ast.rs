pub mod expr;
pub mod raw_expr;
pub mod traced_expr;

use bimap::BiMap;
use quickcheck::{Arbitrary, Gen};
use raw_expr::RawExpr;
use serde::{Deserialize, Serialize};
use traced_expr::{TracedExpr, TracedExprRec};

use crate::{
    debugging::NoOpDebugger, parser::CodeLocation, stdlib::ef3r_stdlib,
};

///
/// An imperative statement to be executed in the ef3r runtime.
///
/// Execute an expression and optionally bind
///  its result to a variable.
///
#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Statement<V> {
    pub location: Option<CodeLocation>,
    pub var: Option<V>,
    pub expr: RawExpr<V>,
}

impl Statement<String> {
    fn arbitrary_with_depth(g: &mut Gen, depth: usize) -> Self {
        let context = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

        Statement {
            location: None,
            var: Option::arbitrary(g),
            expr: TracedExprRec::untraced(
                &TracedExprRec::arbitrary_with_depth(&context, g, depth),
            )
            .as_expr(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use bimap::BiMap;

    use crate::{
        ast::{
            expr::Expr,
            raw_expr::RawExpr,
            traced_expr::{TracedExpr, TracedExprRec},
        },
        debugging::NoOpDebugger,
        interpreter::evaluate_traced,
        stdlib::{ef3r_stdlib, INT_ADD_ID, INT_MUL_ID},
    };

    #[test]
    fn evaluation_keeps_trace() {
        let context = Arc::new(Mutex::new(ef3r_stdlib(
            NoOpDebugger::new(),
            BiMap::new(),
        )));

        // Example expression.
        let expression = TracedExprRec::Apply(
            Box::new(TracedExprRec::BuiltinFunction(INT_MUL_ID).traced()),
            Box::new([
                TracedExprRec::Int(2).traced(),
                TracedExprRec::Apply(
                    Box::new(
                        TracedExprRec::BuiltinFunction(INT_ADD_ID).traced(),
                    ),
                    Box::new([
                        TracedExprRec::Int(1).traced(),
                        TracedExprRec::Int(2).traced(),
                    ]),
                )
                .traced(),
            ]),
        );

        let evaluated =
            evaluate_traced(context, expression.clone().traced()).unwrap();

        assert_eq!(evaluated.evaluated, TracedExprRec::Int(6));

        assert_eq!(evaluated.stored_trace, Some(expression));
    }

    #[test]
    fn evaluating_twice_keeps_entire_trace() {
        let context = Arc::new(Mutex::new(ef3r_stdlib(
            NoOpDebugger::new(),
            BiMap::new(),
        )));

        // Example expression. 2 * (1 + 2)
        let expression = RawExpr::apply(
            RawExpr::builtin_function(INT_MUL_ID),
            [
                RawExpr::int(2),
                RawExpr::apply(
                    RawExpr::builtin_function(INT_ADD_ID),
                    [RawExpr::int(1), RawExpr::int(2)],
                ),
            ],
        );

        let evaluated =
            evaluate_traced(context.clone(), expression.from_raw().clone())
                .unwrap();

        // 2 * (2 * (1 + 2))
        let second_expression = TracedExpr::apply(
            TracedExpr::builtin_function(INT_MUL_ID),
            [TracedExpr::int(2), evaluated],
        );

        let second_evaluated =
            evaluate_traced(context, second_expression.clone()).unwrap();

        let expected = RawExpr::apply(
            RawExpr::builtin_function(INT_MUL_ID),
            [RawExpr::int(2), expression],
        );

        assert_eq!(second_evaluated.full_trace(), expected);
    }
}
