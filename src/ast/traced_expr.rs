use bimap::BiMap;
use quickcheck::{Arbitrary, Gen};
use serde::{Deserialize, Serialize};

use crate::{
    debugging::{Debugger, NoOpDebugger},
    interpreter::{Context, PolymorphicFunctionID},
    modules::{ModuleName, QualifiedName},
    parser::CodeLocation,
    stdlib::ef3r_stdlib,
    types::ExprType,
};

use super::{
    expr::{Expr, FunctionID},
    raw_expr::{RawExpr, RawExprRec},
    Statement,
};

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
    pub location: Option<CodeLocation>,
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
            location: None,
        }
    }

    pub fn get_trace(&self) -> TracedExpr<V> {
        let trace = match &self.stored_trace {
            Some(expr) => expr.clone(),
            None => self.evaluated.clone(),
        };

        TracedExpr {
            location: self.location,
            evaluated: trace,
            stored_trace: None,
        }
    }

    /// Strip an expression of any "trace" data, converting it into the "raw"
    ///  format.
    pub fn untraced(&self) -> RawExpr<V> {
        RawExpr {
            location: self.location,
            expr: self.evaluated.untraced(),
        }
    }

    pub fn full_trace(&self) -> RawExpr<V> {
        match self {
            TracedExpr {
                evaluated,
                location,
                stored_trace,
            } => {
                let actual_trace = stored_trace.as_ref().unwrap_or(evaluated);
                match actual_trace {
                    TracedExprRec::Apply(function, arguments) => RawExpr {
                        location: *location,
                        expr: RawExprRec::Apply(
                            Box::new(function.full_trace()),
                            arguments
                                .iter()
                                .map(|x| x.to_owned().full_trace())
                                .collect(),
                        ),
                    },
                    _ => TracedExpr {
                        location: *location,
                        evaluated: actual_trace.clone(),
                        stored_trace: None,
                    }
                    .untraced(),
                }
            }
        }
    }

    pub fn new(expr: TracedExprRec<V>) -> TracedExpr<V> {
        TracedExpr {
            evaluated: expr,
            stored_trace: None,
            location: None,
        }
    }
}

impl TracedExpr<usize> {
    ///
    /// Utility to help build an expression for a function
    ///  resolving it by name.
    ///
    pub fn resolve<T: Debugger + 'static>(
        context: &Context<T>,
        name: &str,
    ) -> TracedExpr<usize> {
        let poly_id = &context
            .expression_context
            .read()
            .resolve_polymorphic_function(name);
        if let Some(id) = poly_id {
            return TracedExpr::new(TracedExprRec::PolymorphicFunction(*id));
        }

        let fun_id = context.expression_context.read().resolve_function(name);
        if let Some(id) = fun_id {
            return TracedExpr::new(TracedExprRec::BuiltinFunction(id));
        }

        panic!("Could not resolve function {}", name);
    }
}

impl Expr for TracedExpr<usize> {
    fn evaluated(self) -> RawExpr<usize> {
        self.untraced()
    }

    fn none() -> Self {
        TracedExpr::new(TracedExprRec::None)
    }

    fn unit() -> Self {
        TracedExpr::new(TracedExprRec::Unit)
    }

    fn int(value: i32) -> Self {
        TracedExpr::new(TracedExprRec::Int(value))
    }

    fn string(value: i32) -> Self {
        TracedExpr::new(TracedExprRec::String(value.to_string()))
    }

    fn float(value: f32) -> Self {
        TracedExpr::new(TracedExprRec::Float(value))
    }

    fn bool(value: bool) -> Self {
        TracedExpr::new(TracedExprRec::Bool(value))
    }

    fn type_(value: ExprType) -> Self {
        TracedExpr::new(TracedExprRec::Type(value))
    }

    fn pair(lhs: Self, rhs: Self) -> Self {
        TracedExpr::new(TracedExprRec::Pair(Box::new(lhs), Box::new(rhs)))
    }

    fn list(elements: Vec<Self>) -> Self {
        TracedExpr::new(TracedExprRec::List(elements))
    }

    fn node(value: usize) -> Self {
        TracedExpr::new(TracedExprRec::Node(value))
    }

    fn builtin_function(value: FunctionID) -> Self {
        TracedExpr::new(TracedExprRec::BuiltinFunction(value))
    }

    fn polymorphic_function(value: PolymorphicFunctionID) -> Self {
        TracedExpr::new(TracedExprRec::PolymorphicFunction(value))
    }

    fn lambda(
        vars: Vec<usize>,
        stmts: Vec<Statement<usize>>,
        body: Self,
    ) -> Self {
        TracedExpr::new(TracedExprRec::Lambda(vars, stmts, Box::new(body)))
    }

    fn apply<const N: usize>(fun: Self, args: [Self; N]) -> Self {
        TracedExpr::new(TracedExprRec::Apply(Box::new(fun), Box::new(args)))
    }

    fn var(value: usize) -> Self {
        TracedExpr::new(TracedExprRec::Var(value))
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
    /// Reference to a JoinHandle for a thread.
    JoinHandle(usize),
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

    pub fn untraced(&self) -> RawExprRec<V> {
        match self {
            TracedExprRec::None => RawExprRec::None,
            TracedExprRec::Unit => RawExprRec::Unit,
            TracedExprRec::Int(x) => RawExprRec::Int(*x),
            TracedExprRec::String(x) => RawExprRec::String(x.clone()),
            TracedExprRec::Float(x) => RawExprRec::Float(*x),
            TracedExprRec::Bool(x) => RawExprRec::Bool(*x),
            TracedExprRec::Type(x) => RawExprRec::Type(x.clone()),
            TracedExprRec::Pair(x, y) => {
                RawExprRec::Pair(Box::new(x.untraced()), Box::new(y.untraced()))
            }
            TracedExprRec::List(xs) => {
                RawExprRec::List(xs.iter().map(|x| x.untraced()).collect())
            }
            TracedExprRec::JoinHandle(x) => RawExprRec::JoinHandle(*x),
            TracedExprRec::Node(x) => RawExprRec::Node(*x),
            TracedExprRec::BuiltinFunction(x) => {
                RawExprRec::BuiltinFunction(*x)
            }
            TracedExprRec::PolymorphicFunction(x) => {
                RawExprRec::PolymorphicFunction(*x)
            }
            TracedExprRec::Lambda(vars, stmts, body) => RawExprRec::Lambda(
                vars.clone(),
                stmts.clone(),
                Box::new(body.untraced()),
            ),
            TracedExprRec::Apply(f, args) => RawExprRec::Apply(
                Box::new(f.untraced()),
                args.iter()
                    .map(|x| x.untraced())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            TracedExprRec::Var(x) => RawExprRec::Var(x.clone()),
        }
    }
}

impl Arbitrary for TracedExprRec<QualifiedName> {
    fn arbitrary(g: &mut Gen) -> Self {
        let (_, context) = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

        Self::arbitrary_with_depth(&context, g, 0)
    }
}

impl TracedExprRec<QualifiedName> {
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
                let keys: usize =
                    context.expression_context.read().functions.len();
                let key = if keys == 0 {
                    0
                } else {
                    usize::arbitrary(g) % keys
                };
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
            7 => TracedExprRec::Var(QualifiedName {
                module: ModuleName::new(String::arbitrary(g).as_str()),
                name: String::arbitrary(g),
            }),
            _ => unreachable!(),
        }
    }
}

///
/// Converts a traced expression into reverse polish notation order.
///
pub fn to_rpn<V: Clone>(expr: &RawExpr<V>) -> Vec<RawExpr<V>> {
    match &expr.expr {
        RawExprRec::Apply(function, arguments) => {
            let mut result = vec![];
            for arg in arguments.iter() {
                result.extend(to_rpn(arg));
            }
            result.extend(to_rpn(function));
            //result.push(expr.clone());
            result
        }
        _ => vec![expr.clone()],
    }
}

impl TracedExpr<QualifiedName> {
    pub fn expression_trace(&self) -> String {
        let rpn = to_rpn(&self.full_trace());
        rpn.iter()
            .map(|expr| {
                "    ".to_owned()
                    + match (expr.location, &expr.expr) {
                        (loc, RawExprRec::BuiltinFunction(_)) => {
                            format!(
                                "at line {} -> {} (applied function)",
                                CodeLocation::format(loc),
                                &expr
                            )
                        }
                        (loc, RawExprRec::PolymorphicFunction(_)) => {
                            format!(
                                "at line {} -> {} (applied function)",
                                CodeLocation::format(loc),
                                &expr
                            )
                        }
                        (loc, RawExprRec::Int(n)) => {
                            format!(
                                "at line {} -> {} (constant)",
                                CodeLocation::format(loc),
                                n
                            )
                        }
                        (loc, RawExprRec::Float(n)) => {
                            format!(
                                "at line {} -> {} (constant)",
                                CodeLocation::format(loc),
                                n
                            )
                        }
                        (loc, RawExprRec::String(s)) => {
                            format!(
                                "at line {} -> \"{}\" (constant)",
                                CodeLocation::format(loc),
                                s
                            )
                        }
                        (loc, RawExprRec::Bool(b)) => {
                            format!(
                                "at line {} -> {} (constant)",
                                CodeLocation::format(loc),
                                b
                            )
                        }
                        (loc, x) => {
                            format!(
                                "at line {} -> {} (constant)",
                                CodeLocation::format(loc),
                                x.clone().as_expr()
                            )
                        }
                    }
                    .as_str()
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}
