pub mod expr;
pub mod raw_expr;
pub mod traced_expr;

use bimap::BiMap;
use quickcheck::{Arbitrary, Gen};
use raw_expr::RawExpr;
use serde::{Deserialize, Serialize};
use traced_expr::TracedExprRec;

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
            expr: TracedExprRec::to_raw(&TracedExprRec::arbitrary_with_depth(
                &context, g, depth,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use bimap::BiMap;

    use crate::{
        ast::traced_expr::TracedExprRec,
        debugging::NoOpDebugger,
        interpreter::{evaluate_traced, unwind_trace},
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
            evaluate_traced(context.clone(), expression.clone().traced())
                .unwrap();

        let second_expression = TracedExprRec::Apply(
            Box::new(TracedExprRec::BuiltinFunction(INT_MUL_ID).traced()),
            Box::new([TracedExprRec::Int(2).traced(), evaluated]),
        );

        let second_evaluated =
            evaluate_traced(context, second_expression.clone().traced())
                .unwrap();

        let expected = TracedExprRec::Apply(
            Box::new(TracedExprRec::BuiltinFunction(INT_MUL_ID).traced()),
            Box::new([TracedExprRec::Int(2).traced(), expression.traced()]),
        );

        assert_eq!(unwind_trace(second_evaluated).clone().evaluated, expected);
    }
}
