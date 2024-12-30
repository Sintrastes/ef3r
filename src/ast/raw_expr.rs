use std::fmt::Display;

use bimap::BiMap;
use serde::{Deserialize, Serialize};

use crate::{
    debugging::NoOpDebugger, interpreter::PolymorphicFunctionID,
    stdlib::ef3r_stdlib, types::ExprType,
};

use super::{
    expr::{Expr, FunctionID},
    traced_expr::TracedExprRec,
    Statement,
};

///
/// A more compact representation of an expression that does not include any
///  traces.
///
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RawExpr<V> {
    None,
    Unit,
    Int(i32),
    String(String),
    Float(f32),
    Bool(bool),
    Type(ExprType),
    Pair(Box<RawExpr<V>>, Box<RawExpr<V>>),
    List(Vec<RawExpr<V>>),
    Node(usize),
    BuiltinFunction(FunctionID),
    PolymorphicFunction(PolymorphicFunctionID),
    /// Lambda expression with 0+ parameters: \x y z -> f x
    /// Can either be an "effectful" lambda with statements and a final
    /// return result, or (if there are no statements), a pure function.
    Lambda(Vec<V>, Vec<Statement<V>>, Box<RawExpr<V>>),
    /// Function application: f x
    Apply(Box<RawExpr<V>>, Box<[RawExpr<V>]>),
    /// Locally defined variable.
    Var(V),
}

impl<V: Clone> RawExpr<V> {
    /// Convert a "raw" format expression into one traced with
    ///  expression data.
    pub fn from_raw(&self) -> TracedExprRec<V> {
        match self {
            RawExpr::None => TracedExprRec::None,
            RawExpr::Unit => TracedExprRec::Unit,
            RawExpr::Int(x) => TracedExprRec::Int(*x),
            RawExpr::String(x) => TracedExprRec::String(x.clone()),
            RawExpr::Float(x) => TracedExprRec::Float(*x),
            RawExpr::Bool(x) => TracedExprRec::Bool(*x),
            RawExpr::Type(x) => TracedExprRec::Type(x.clone()),
            RawExpr::Pair(x, y) => TracedExprRec::Pair(
                Box::new(x.from_raw().traced()),
                Box::new(y.from_raw().traced()),
            ),
            RawExpr::List(xs) => TracedExprRec::List(
                xs.iter().map(|x| x.from_raw().traced()).collect(),
            ),
            RawExpr::Node(x) => TracedExprRec::Node(*x),
            RawExpr::BuiltinFunction(x) => TracedExprRec::BuiltinFunction(*x),
            RawExpr::PolymorphicFunction(x) => {
                TracedExprRec::PolymorphicFunction(*x)
            }
            RawExpr::Lambda(vars, stmts, body) => TracedExprRec::Lambda(
                vars.clone(),
                stmts.clone(),
                Box::new(body.from_raw().traced()),
            ),
            RawExpr::Apply(f, args) => TracedExprRec::Apply(
                Box::new(f.from_raw().traced()),
                args.iter()
                    .map(|x| x.from_raw().traced())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            RawExpr::Var(x) => TracedExprRec::Var(x.clone()),
        }
    }
}

impl<V: Display> Display for RawExpr<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let context = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

        match self {
            RawExpr::None => f.write_str("None"),
            RawExpr::Unit => f.write_str("Unit"),
            RawExpr::Int(x) => x.fmt(f),
            RawExpr::Type(x) => x.fmt(f),
            RawExpr::String(x) => {
                f.write_str("\"")?;
                x.chars()
                    .map(|x| {
                        if x == '\n' {
                            "\\n".to_string()
                        } else {
                            x.to_string()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join("")
                    .fmt(f)?;
                f.write_str("\"")
            }
            RawExpr::Float(x) => x.fmt(f),
            RawExpr::BuiltinFunction(x) => {
                let name = context
                    .expression_context
                    .functions
                    .get(x)
                    .unwrap()
                    .name
                    .clone();

                f.write_str(name.as_str())
            }
            RawExpr::PolymorphicFunction(id) => {
                let index = context
                    .expression_context
                    .polymorphic_functions
                    .iter()
                    .find(|(x, _)| x.id == *id)
                    .unwrap()
                    .1;

                let name = context
                    .expression_context
                    .functions
                    .get(&index)
                    .unwrap()
                    .name
                    .clone();

                f.write_str(name.as_str())
            }
            RawExpr::List(elements) => {
                f.write_str("[")?;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    element.fmt(f)?;
                }
                f.write_str("]")
            }
            RawExpr::Lambda(vars, _, body) => {
                f.write_str("\\")?;
                for var in vars {
                    var.fmt(f)?;
                }
                f.write_str(" -> ")?;
                body.as_ref().fmt(f)
            }
            RawExpr::Apply(fun, args) => {
                f.write_str("(")?;
                fun.as_ref().fmt(f)?;
                for arg in args {
                    f.write_str(" ")?;
                    arg.fmt(f)?;
                }
                f.write_str(")")
            }
            RawExpr::Var(x) => x.fmt(f),
            RawExpr::Node(idx) => {
                f.write_str("Node#")?;
                idx.fmt(f)
            }
            RawExpr::Bool(value) => value.fmt(f),
            RawExpr::Pair(traced_expr, traced_expr1) => {
                f.write_str("(")?;
                traced_expr.as_ref().fmt(f)?;
                f.write_str(",")?;
                traced_expr1.as_ref().fmt(f)?;
                f.write_str(")")
            }
        }
    }
}

// Question: How would this work with traced expressions?
pub fn substitute<V: Clone + PartialEq + Eq>(
    variable: &V,
    with: &RawExpr<V>,
    in_expr: RawExpr<V>,
) -> RawExpr<V> {
    match in_expr {
        RawExpr::Pair(x, y) => RawExpr::Pair(
            Box::new(substitute(variable, with, *x)),
            Box::new(substitute(variable, with, *y)),
        ),
        RawExpr::Apply(f, xs) => RawExpr::Apply(
            Box::new(substitute(variable, with, *f)),
            xs.into_vec()
                .into_iter()
                .map(|x| substitute(variable, with, x))
                .collect(),
        ),
        RawExpr::Var(x) => {
            if *variable == x {
                with.clone()
            } else {
                RawExpr::Var(x)
            }
        }
        RawExpr::Lambda(vars, statements, expr) => RawExpr::Lambda(
            vars,
            statements
                .into_iter()
                .map(|statement| {
                    substitute_statement(variable, with, statement)
                })
                .collect(),
            Box::new(substitute(variable, with, *expr)),
        ),
        _ => in_expr,
    }
}

pub fn substitute_statement<V: Clone + PartialEq + Eq>(
    variable: &V,
    with: &RawExpr<V>,
    statement: Statement<V>,
) -> Statement<V> {
    Statement {
        location: statement.location,
        var: statement.var,
        expr: substitute(variable, with, statement.expr),
    }
}

impl Expr for RawExpr<u32> {
    fn evaluated(self) -> RawExpr<u32> {
        self
    }

    fn none() -> Self {
        RawExpr::None
    }

    fn unit() -> Self {
        RawExpr::Unit
    }

    fn int(value: i32) -> Self {
        RawExpr::Int(value)
    }

    fn string(value: i32) -> Self {
        RawExpr::String(value.to_string())
    }

    fn float(value: f32) -> Self {
        RawExpr::Float(value)
    }

    fn bool(value: bool) -> Self {
        RawExpr::Bool(value)
    }

    fn type_(value: ExprType) -> Self {
        RawExpr::Type(value)
    }

    fn pair(lhs: Self, rhs: Self) -> Self {
        RawExpr::Pair(Box::new(lhs), Box::new(rhs))
    }

    fn list(elements: Vec<Self>) -> Self {
        RawExpr::List(elements)
    }

    fn node(value: usize) -> Self {
        RawExpr::Node(value)
    }

    fn builtin_function(value: FunctionID) -> Self {
        RawExpr::BuiltinFunction(value)
    }

    fn polymorphic_function(value: PolymorphicFunctionID) -> Self {
        RawExpr::PolymorphicFunction(value)
    }

    fn lambda(
        vars: Vec<u32>,
        stmts: Vec<Statement<u32>>,
        body: Box<Self>,
    ) -> Self {
        RawExpr::Lambda(vars, stmts, body)
    }

    fn apply(fun: Box<Self>, args: Box<[Self]>) -> Self {
        RawExpr::Apply(fun, args)
    }

    fn var(value: u32) -> Self {
        RawExpr::Var(value)
    }
}
