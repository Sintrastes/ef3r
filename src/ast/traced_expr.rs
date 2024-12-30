use std::fmt::Display;

use bimap::BiMap;
use quickcheck::{Arbitrary, Gen};
use serde::{Deserialize, Serialize};

use crate::{
    debugging::{Debugger, NoOpDebugger},
    interpreter::{Context, PolymorphicFunctionID},
    stdlib::ef3r_stdlib,
    types::ExprType,
};

use super::{expr::FunctionID, raw_expr::RawExpr, Statement};

///
/// An expression together with an optional un-evaluated
///  version of said expression, showing the history of how that expression has
///  been built up.
///
/// If it exists, the trace should evaluate to evaluated.
///
#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct TracedExpr<V> {
    pub evaluated: TracedExprRec<V>,
    pub stored_trace: Option<TracedExprRec<V>>,
}

impl<V: Clone> TracedExpr<V> {
    pub fn build(
        evaluated: TracedExprRec<V>,
        trace: Option<TracedExprRec<V>>,
    ) -> TracedExpr<V> {
        TracedExpr {
            evaluated: evaluated,
            stored_trace: trace,
        }
    }

    pub fn get_trace(&self) -> TracedExprRec<V> {
        match &self.stored_trace {
            Some(expr) => expr.clone(),
            None => self.evaluated.clone(),
        }
    }

    pub fn untraced(&self) -> RawExpr<V> {
        self.evaluated.to_raw()
    }

    pub fn full_trace(&self) -> RawExpr<V> {
        match self {
            TracedExpr {
                evaluated,
                stored_trace,
            } => {
                let actual_trace = stored_trace.as_ref().unwrap_or(evaluated);
                match actual_trace {
                    TracedExprRec::Apply(function, arguments) => {
                        RawExpr::Apply(
                            Box::new(function.full_trace()),
                            arguments
                                .iter()
                                .map(|x| x.to_owned().full_trace())
                                .collect(),
                        )
                    }
                    _ => actual_trace.to_raw(),
                }
            }
        }
    }

    pub fn new(expr: TracedExprRec<V>) -> TracedExpr<V> {
        TracedExpr {
            evaluated: expr,
            stored_trace: None,
        }
    }
}

///
/// Abstract Syntax Tree for the expression language of
///  ef3f.
///
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TracedExprRec<V> {
    /// Null symbol.
    None,
    /// Unit symbol
    Unit,
    /// Integer literal
    Int(i32),
    /// String literal
    String(String),
    /// Float literal
    Float(f32),
    /// Boolean literal,
    Bool(bool),
    /// Type of types
    Type(ExprType),
    /// Pair type,
    Pair(Box<TracedExpr<V>>, Box<TracedExpr<V>>),
    /// Type of lists
    List(Vec<TracedExpr<V>>),
    /// Reference to a node that has been created and stored in the
    ///  interpreter context.
    Node(usize),
    /// Reference to a built-in monomorphic function in the interpreter.
    BuiltinFunction(FunctionID),
    /// Reference to a polymorphic function defined in the interpreter.
    PolymorphicFunction(PolymorphicFunctionID),
    /// Lambda expression with 0+ parameters: \x y z -> f x
    /// Can either be an "effectful" lambda with statements and a final
    /// return result, or (if there are no statements), a pure function.
    Lambda(Vec<V>, Vec<Statement<V>>, Box<TracedExpr<V>>),
    /// Function application: f x
    Apply(Box<TracedExpr<V>>, Box<[TracedExpr<V>]>),
    /// Locally defined variable.
    Var(V),
}

impl<V: Clone> TracedExprRec<V> {
    pub fn traced(self) -> TracedExpr<V> {
        TracedExpr::new(self)
    }

    /// Strip an expression of any "trace" data, converting it into the "raw"
    ///  format.
    pub fn to_raw(&self) -> RawExpr<V> {
        match self {
            TracedExprRec::None => RawExpr::None,
            TracedExprRec::Unit => RawExpr::Unit,
            TracedExprRec::Int(x) => RawExpr::Int(*x),
            TracedExprRec::String(x) => RawExpr::String(x.clone()),
            TracedExprRec::Float(x) => RawExpr::Float(*x),
            TracedExprRec::Bool(x) => RawExpr::Bool(*x),
            TracedExprRec::Type(x) => RawExpr::Type(x.clone()),
            TracedExprRec::Pair(x, y) => RawExpr::Pair(
                Box::new(x.evaluated.to_raw()),
                Box::new(y.evaluated.to_raw()),
            ),
            TracedExprRec::List(xs) => {
                RawExpr::List(xs.iter().map(|x| x.evaluated.to_raw()).collect())
            }
            TracedExprRec::Node(x) => RawExpr::Node(*x),
            TracedExprRec::BuiltinFunction(x) => RawExpr::BuiltinFunction(*x),
            TracedExprRec::PolymorphicFunction(x) => {
                RawExpr::PolymorphicFunction(*x)
            }
            TracedExprRec::Lambda(vars, stmts, body) => RawExpr::Lambda(
                vars.clone(),
                stmts.clone(),
                Box::new(body.evaluated.to_raw()),
            ),
            TracedExprRec::Apply(f, args) => RawExpr::Apply(
                Box::new(f.evaluated.to_raw()),
                args.iter()
                    .map(|x| x.evaluated.to_raw())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            TracedExprRec::Var(x) => RawExpr::Var(x.clone()),
        }
    }
}

impl Arbitrary for TracedExprRec<String> {
    fn arbitrary(g: &mut Gen) -> Self {
        let context = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

        Self::arbitrary_with_depth(&context, g, 0)
    }
}

impl TracedExprRec<String> {
    pub fn arbitrary_with_depth<T: Debugger + 'static>(
        context: &Context<T>,
        g: &mut Gen,
        depth: usize,
    ) -> Self {
        let choices = if depth < 5 {
            vec![
                0, // None
                1, // Int
                2, // String
                3, // Float
                4, // BuiltinFunction
                5, // Lambda
                6, // Apply
                7, // Var
            ]
        } else {
            vec![
                0, // None
                1, // Int
                2, // String
                3, // Float
                4, // BuiltinFunction
                7, // Var
            ]
        };

        match g.choose(&choices).unwrap() {
            0 => TracedExprRec::None,
            1 => TracedExprRec::Int(i32::arbitrary(g)),
            2 => TracedExprRec::String(String::arbitrary(g)),
            3 => loop {
                let value = f32::arbitrary(g);
                if !value.is_nan() {
                    return TracedExprRec::Float(value);
                }
            },
            4 => {
                let keys: Vec<&u32> =
                    context.expression_context.functions.keys().collect();
                let key = **g.choose(&keys).unwrap();
                TracedExprRec::BuiltinFunction(key)
            }
            5 => TracedExprRec::Lambda(
                Vec::arbitrary(g),
                (0..usize::arbitrary(g) % 3)
                    .map(|_| Statement::arbitrary_with_depth(g, depth + 1))
                    .collect(),
                Box::new(
                    TracedExprRec::arbitrary_with_depth(context, g, depth + 1)
                        .traced(),
                ),
            ),
            6 => {
                let num_args = usize::arbitrary(g) % 5; // Limit the number of arguments to a small number
                let args = (0..num_args)
                    .map(|_| {
                        TracedExprRec::arbitrary_with_depth(
                            context,
                            g,
                            depth + 1,
                        )
                        .traced()
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                TracedExprRec::Apply(
                    Box::new(
                        TracedExprRec::arbitrary_with_depth(
                            context,
                            g,
                            depth + 1,
                        )
                        .traced(),
                    ),
                    args,
                )
            }
            7 => TracedExprRec::Var(String::arbitrary(g)),
            _ => unreachable!(),
        }
    }
}
