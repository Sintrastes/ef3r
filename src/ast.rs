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
            expr: TracedExprRec::untraced(
                &TracedExprRec::arbitrary_with_depth(&context, g, depth),
            )
            .as_expr(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use bimap::BiMap;
    use parking_lot::RwLock;

    use crate::{
        ast::{
            expr::Expr,
            raw_expr::RawExpr,
            traced_expr::{TracedExpr, TracedExprRec},
        },
        debugging::NoOpDebugger,
        interpreter::evaluate_traced,
        stdlib::ef3r_stdlib,
    };

    #[test]
    fn evaluation_keeps_trace() {
        let context_ref = Arc::new(RwLock::new(ef3r_stdlib(
            NoOpDebugger::new(),
            BiMap::new(),
        )));

        let mut context = context_ref.write();

        // Example expression.
        let expression = TracedExpr::apply(
            TracedExpr::resolve(&context, "*"),
            [
                TracedExpr::int(2),
                TracedExpr::apply(
                    TracedExpr::resolve(&context, "+"),
                    [TracedExpr::int(1), TracedExpr::int(2)],
                ),
            ],
        );

        let evaluated = evaluate_traced(
            &mut context,
            context_ref.clone(),
            expression.clone(),
        )
        .unwrap();

        assert_eq!(evaluated.evaluated, TracedExprRec::Int(6));

        assert_eq!(evaluated.get_trace(), expression);
    }

    #[test]
    fn evaluating_twice_keeps_entire_trace() {
        let context_ref = Arc::new(RwLock::new(ef3r_stdlib(
            NoOpDebugger::new(),
            BiMap::new(),
        )));

        let mut context = context_ref.write();

        // Example expression. 2 * (1 + 2)
        let expression = RawExpr::apply(
            RawExpr::resolve(&context, "*"),
            [
                RawExpr::int(2),
                RawExpr::apply(
                    RawExpr::resolve(&context, "+"),
                    [RawExpr::int(1), RawExpr::int(2)],
                ),
            ],
        );

        let evaluated = evaluate_traced(
            &mut context,
            context_ref.clone(),
            expression.from_raw().clone(),
        )
        .unwrap();

        // 2 * (2 * (1 + 2))
        let second_expression = TracedExpr::apply(
            TracedExpr::resolve(&context, "*"),
            [TracedExpr::int(2), evaluated],
        );

        let expected = RawExpr::apply(
            RawExpr::resolve(&context, "*"),
            [RawExpr::int(2), expression],
        );

        let second_evaluated = evaluate_traced(
            &mut context,
            context_ref.clone(),
            second_expression.clone(),
        )
        .unwrap();

        assert_eq!(second_evaluated.full_trace(), expected);
    }
}
