pub mod expr;
pub mod raw_expr;
pub mod traced_expr;

use bimap::BiMap;
use quickcheck::{Arbitrary, Gen};
use raw_expr::RawExpr;
use serde::{Deserialize, Serialize};
use traced_expr::TracedExprRec;

use crate::{
    debugging::NoOpDebugger,
    modules::{ModuleName, QualifiedName},
    parser::CodeLocation,
    stdlib::ef3r_stdlib,
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

impl<V: Clone> Statement<V> {
    pub fn map<T>(self, f: impl Fn(V) -> T) -> Statement<T> {
        Statement {
            location: self.location,
            var: self.var.map(&f),
            expr: self.expr.map(&f),
        }
    }
}

impl Statement<String> {
    pub fn qualified(&self, module: ModuleName) -> Statement<QualifiedName> {
        Statement {
            location: self.location,
            var: self.var.clone().map(|name| QualifiedName {
                module: module.clone(),
                name: name.to_string(),
            }),
            expr: self.expr.qualified(module),
        }
    }
}

impl Statement<QualifiedName> {
    fn arbitrary_with_depth(g: &mut Gen, depth: usize) -> Self {
        let (_, context) = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

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
        let (_, context) = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

        let context_ref = Arc::new(RwLock::new(context));

        let context = context_ref.write();

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

        let evaluated = evaluate_traced(&context, expression.clone()).unwrap();

        assert_eq!(evaluated.evaluated, TracedExprRec::Int(6));

        assert_eq!(evaluated.get_trace(), expression);
    }

    #[test]
    fn evaluating_twice_keeps_entire_trace() {
        let (_, context) = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

        let context_ref = Arc::new(RwLock::new(context));

        let context = context_ref.write();

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

        let evaluated =
            evaluate_traced(&context, expression.from_raw().clone()).unwrap();

        // 2 * (2 * (1 + 2))
        let second_expression = TracedExpr::apply(
            TracedExpr::resolve(&context, "*"),
            [TracedExpr::int(2), evaluated],
        );

        let expected = RawExpr::apply(
            RawExpr::resolve(&context, "*"),
            [RawExpr::int(2), expression],
        );

        let second_evaluated =
            evaluate_traced(&context, second_expression.clone()).unwrap();

        assert_eq!(second_evaluated.full_trace(), expected);
    }
}
