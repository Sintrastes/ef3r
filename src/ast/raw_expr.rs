use serde::{Deserialize, Serialize};

use crate::{interpreter::PolymorphicFunctionID, types::ExprType};

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
