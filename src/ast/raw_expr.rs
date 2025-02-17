use std::fmt::Display;

use bimap::BiMap;
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
    traced_expr::{TracedExpr, TracedExprRec},
    Statement,
};

///
/// A more compact representation of an expression that does not include any
///  traces.
///
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RawExprRec<V> {
    None,
    Unit,
    Int(i32),
    String(String),
    Float(f32),
    Bool(bool),
    Type(ExprType),
    Pair(Box<RawExpr<V>>, Box<RawExpr<V>>),
    List(Vec<RawExpr<V>>),
    JoinHandle(usize),
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

impl RawExpr<String> {
    pub fn qualified(&self, module: ModuleName) -> RawExpr<QualifiedName> {
        RawExpr {
            expr: self.expr.qualified(module),
            location: self.location,
        }
    }
}

impl RawExprRec<String> {
    pub fn qualified(&self, module: ModuleName) -> RawExprRec<QualifiedName> {
        match self {
            RawExprRec::None => RawExprRec::None,
            RawExprRec::Unit => RawExprRec::Unit,
            RawExprRec::Int(x) => RawExprRec::Int(*x),
            RawExprRec::String(x) => RawExprRec::String(x.clone()),
            RawExprRec::Float(x) => RawExprRec::Float(*x),
            RawExprRec::Bool(x) => RawExprRec::Bool(*x),
            RawExprRec::Type(x) => RawExprRec::Type(x.clone()),
            RawExprRec::Pair(x, y) => RawExprRec::Pair(
                Box::new(x.qualified(module.clone())),
                Box::new(y.qualified(module.clone())),
            ),
            RawExprRec::List(xs) => RawExprRec::List(
                xs.iter()
                    .map(|x| x.expr.qualified(module.clone()).as_expr())
                    .collect(),
            ),
            RawExprRec::JoinHandle(x) => RawExprRec::JoinHandle(*x),
            RawExprRec::Node(x) => RawExprRec::Node(*x),
            RawExprRec::BuiltinFunction(x) => RawExprRec::BuiltinFunction(*x),
            RawExprRec::PolymorphicFunction(x) => {
                RawExprRec::PolymorphicFunction(*x)
            }
            RawExprRec::Lambda(vars, stmts, body) => RawExprRec::Lambda(
                vars.iter()
                    .map(|x| QualifiedName {
                        module: module.clone(),
                        name: x.clone(),
                    })
                    .collect(),
                stmts
                    .iter()
                    .map(|stmt| Statement {
                        location: stmt.location,
                        var: stmt.var.clone().map(|var| QualifiedName {
                            module: module.clone(),
                            name: var,
                        }),
                        expr: RawExpr {
                            expr: stmt.expr.expr.qualified(module.clone()),
                            location: stmt.expr.location,
                        },
                    })
                    .collect(),
                Box::new(body.expr.qualified(module.clone()).as_expr()),
            ),
            RawExprRec::Apply(f, args) => RawExprRec::Apply(
                Box::new(f.qualified(module.clone())),
                args.iter()
                    .map(|x| x.expr.qualified(module.clone()).as_expr())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            RawExprRec::Var(x) => RawExprRec::Var(QualifiedName {
                module,
                name: x.clone(),
            }),
        }
    }
}

impl<V: Clone> RawExprRec<V> {
    pub fn map<U, F>(self, f: F) -> RawExprRec<U>
    where
        F: Fn(V) -> U + Clone,
    {
        match self {
            RawExprRec::None => RawExprRec::None,
            RawExprRec::Unit => RawExprRec::Unit,
            RawExprRec::Int(x) => RawExprRec::Int(x),
            RawExprRec::String(x) => RawExprRec::String(x),
            RawExprRec::Float(x) => RawExprRec::Float(x),
            RawExprRec::Bool(x) => RawExprRec::Bool(x),
            RawExprRec::Type(x) => RawExprRec::Type(x),
            RawExprRec::Pair(x, y) => RawExprRec::Pair(
                Box::new(RawExpr {
                    expr: x.expr.map(f.clone()),
                    location: x.location,
                }),
                Box::new(RawExpr {
                    expr: y.expr.map(f.clone()),
                    location: y.location,
                }),
            ),
            RawExprRec::List(xs) => RawExprRec::List(
                xs.into_iter()
                    .map(|x| RawExpr {
                        expr: x.expr.map(f.clone()),
                        location: x.location,
                    })
                    .collect(),
            ),
            RawExprRec::JoinHandle(x) => RawExprRec::JoinHandle(x),
            RawExprRec::Node(x) => RawExprRec::Node(x),
            RawExprRec::BuiltinFunction(x) => RawExprRec::BuiltinFunction(x),
            RawExprRec::PolymorphicFunction(x) => {
                RawExprRec::PolymorphicFunction(x)
            }
            RawExprRec::Lambda(vars, stmts, body) => RawExprRec::Lambda(
                vars.into_iter().map(f.clone()).collect(),
                stmts
                    .into_iter()
                    .map(|stmt| Statement {
                        location: stmt.location,
                        var: stmt.var.map(&f),
                        expr: RawExpr {
                            expr: stmt.expr.expr.map(f.clone()),
                            location: stmt.expr.location,
                        },
                    })
                    .collect(),
                Box::new(RawExpr {
                    expr: body.expr.map(f.clone()),
                    location: body.location,
                }),
            ),
            RawExprRec::Apply(fun, args) => RawExprRec::Apply(
                Box::new(RawExpr {
                    expr: fun.expr.map(f.clone()),
                    location: fun.location,
                }),
                args.into_vec()
                    .into_iter()
                    .map(|x| RawExpr {
                        expr: x.expr.map(f.clone()),
                        location: x.location,
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            RawExprRec::Var(x) => RawExprRec::Var(f(x)),
        }
    }

    pub fn as_expr(self) -> RawExpr<V> {
        RawExpr {
            expr: self,
            location: None,
        }
    }

    pub fn at_loc(self, location: CodeLocation) -> RawExpr<V> {
        RawExpr {
            expr: self,
            location: Some(location),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct RawExpr<V> {
    pub expr: RawExprRec<V>,
    pub location: Option<CodeLocation>,
}

impl<V: Clone> RawExpr<V> {
    pub fn map<U, F>(self, f: F) -> RawExpr<U>
    where
        F: Fn(V) -> U + Clone,
    {
        RawExpr {
            expr: self.expr.map(f),
            location: self.location,
        }
    }

    /// Convert a "raw" format expression into one traced with
    ///  expression data.
    pub fn from_raw(&self) -> TracedExpr<V> {
        TracedExpr {
            location: self.location,
            stored_trace: None,
            evaluated: match &self.expr {
                RawExprRec::None => TracedExprRec::None,
                RawExprRec::Unit => TracedExprRec::Unit,
                RawExprRec::Int(x) => TracedExprRec::Int(*x),
                RawExprRec::String(x) => TracedExprRec::String(x.clone()),
                RawExprRec::Float(x) => TracedExprRec::Float(*x),
                RawExprRec::Bool(x) => TracedExprRec::Bool(*x),
                RawExprRec::Type(x) => TracedExprRec::Type(x.clone()),
                RawExprRec::Pair(x, y) => TracedExprRec::Pair(
                    Box::new(x.from_raw()),
                    Box::new(y.from_raw()),
                ),
                RawExprRec::List(xs) => TracedExprRec::List(
                    xs.iter().map(|x| x.from_raw()).collect(),
                ),
                RawExprRec::JoinHandle(x) => TracedExprRec::JoinHandle(*x),
                RawExprRec::Node(x) => TracedExprRec::Node(*x),
                RawExprRec::BuiltinFunction(x) => {
                    TracedExprRec::BuiltinFunction(*x)
                }
                RawExprRec::PolymorphicFunction(x) => {
                    TracedExprRec::PolymorphicFunction(*x)
                }
                RawExprRec::Lambda(vars, stmts, body) => TracedExprRec::Lambda(
                    vars.clone(),
                    stmts.clone(),
                    Box::new(body.from_raw()),
                ),
                RawExprRec::Apply(f, args) => TracedExprRec::Apply(
                    Box::new(f.from_raw()),
                    args.iter()
                        .map(|x| x.from_raw())
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                ),
                RawExprRec::Var(x) => TracedExprRec::Var(x.clone()),
            },
        }
    }
}

impl RawExpr<usize> {
    ///
    /// Utility to help build an expression for a function
    ///  resolving it by name.
    ///
    pub fn resolve<T: Debugger + 'static>(
        context: &Context<T>,
        name: &str,
    ) -> RawExpr<usize> {
        let poly_id = context
            .expression_context
            .read()
            .resolve_polymorphic_function(name);
        if let Some(id) = poly_id {
            return RawExpr::polymorphic_function(id);
        }

        let fun_id = context.expression_context.read().resolve_function(name);
        if let Some(id) = fun_id {
            return RawExpr::builtin_function(id);
        }

        panic!("Could not resolve function {}", name);
    }
}

impl<V: Display> Display for RawExpr<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (_, context) = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

        match &self.expr {
            RawExprRec::None => f.write_str("None"),
            RawExprRec::Unit => f.write_str("Unit"),
            RawExprRec::Int(x) => x.fmt(f),
            RawExprRec::Type(x) => x.fmt(f),
            RawExprRec::String(x) => {
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
            RawExprRec::JoinHandle(x) => {
                f.write_str("#joinHandle")?;
                f.write_str(x.to_string().as_str())
            }
            RawExprRec::Float(x) => x.fmt(f),
            RawExprRec::BuiltinFunction(x) => {
                let expression_context = context.expression_context.read();

                let function_definition =
                    expression_context.functions.get(*x).unwrap();

                let module = function_definition.0.clone();

                let name = function_definition.1.name.clone();

                let qualified_name = QualifiedName { module, name };

                f.write_str(qualified_name.to_string().as_str())
            }
            RawExprRec::PolymorphicFunction(id) => {
                let index = *context
                    .expression_context
                    .read()
                    .polymorphic_functions
                    .iter()
                    .find(|(x, _)| x.id == *id)
                    .unwrap()
                    .1;

                let name = context
                    .expression_context
                    .read()
                    .functions
                    .get(index)
                    .unwrap()
                    .1
                    .name
                    .clone();

                f.write_str(name.as_str())
            }
            RawExprRec::List(elements) => {
                f.write_str("[")?;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    element.fmt(f)?;
                }
                f.write_str("]")
            }
            RawExprRec::Lambda(vars, _, body) => {
                f.write_str("\\")?;
                for var in vars {
                    var.fmt(f)?;
                }
                f.write_str(" -> ")?;
                body.as_ref().fmt(f)
            }
            RawExprRec::Apply(fun, args) => {
                // Case split on whether fun is a symbol (for infix notation)
                let fun_str = fun.to_string();
                if fun_str.chars().all(|c| c.is_alphanumeric()) {
                    // Use function application syntax for normal functions
                    fun.as_ref().fmt(f)?;
                    f.write_str("(")?;
                    for arg in args {
                        f.write_str(", ")?;
                        arg.fmt(f)?;
                    }
                    f.write_str(")")
                } else {
                    // Use infix notation for operators
                    f.write_str("(")?;
                    if let Some((first, rest)) = args.split_first() {
                        first.fmt(f)?;
                        for arg in rest {
                            f.write_str(" ")?;
                            fun.fmt(f)?;
                            f.write_str(" ")?;
                            arg.fmt(f)?;
                        }
                    }
                    f.write_str(")")
                }
            }
            RawExprRec::Var(x) => x.fmt(f),
            RawExprRec::Node(idx) => {
                f.write_str("Node#")?;
                idx.fmt(f)
            }
            RawExprRec::Bool(value) => value.fmt(f),
            RawExprRec::Pair(traced_expr, traced_expr1) => {
                f.write_str("(")?;
                traced_expr.as_ref().fmt(f)?;
                f.write_str(",")?;
                traced_expr1.as_ref().fmt(f)?;
                f.write_str(")")
            }
        }
    }
}

pub fn substitute_traced<V: Clone + PartialEq + Eq>(
    variable: &V,
    with: &TracedExpr<V>,
    in_expr: TracedExpr<V>,
) -> TracedExpr<V> {
    TracedExpr {
        location: in_expr.location,
        evaluated: substitute_traced_rec(variable, with, in_expr.evaluated),
        stored_trace: in_expr
            .stored_trace
            .map(|expr| substitute_traced_rec(variable, with, expr)),
    }
}

fn substitute_traced_rec<V: Clone + PartialEq + Eq>(
    variable: &V,
    with: &TracedExpr<V>,
    in_expr: TracedExprRec<V>,
) -> TracedExprRec<V> {
    match in_expr {
        TracedExprRec::Pair(x, y) => TracedExprRec::Pair(
            Box::new(substitute_traced(variable, with, *x)),
            Box::new(substitute_traced(variable, with, *y)),
        ),
        TracedExprRec::Apply(f, xs) => TracedExprRec::Apply(
            Box::new(substitute_traced(variable, with, *f)),
            xs.into_vec()
                .into_iter()
                .map(|x| substitute_traced(variable, with, x))
                .collect(),
        ),
        TracedExprRec::Var(x) => {
            if *variable == x {
                with.clone().evaluated
            } else {
                TracedExprRec::Var(x)
            }
        }
        TracedExprRec::Lambda(vars, statements, expr) => TracedExprRec::Lambda(
            vars,
            statements
                .into_iter()
                .map(|statement| {
                    substitute_statement(variable, &with.untraced(), statement)
                })
                .collect(),
            Box::new(substitute_traced(variable, with, *expr)),
        ),
        _ => in_expr,
    }
}

pub fn substitute<V: Clone + PartialEq + Eq>(
    variable: &V,
    with: &RawExpr<V>,
    in_expr: RawExpr<V>,
) -> RawExpr<V> {
    RawExpr {
        location: in_expr.location,
        expr: match in_expr.expr {
            RawExprRec::Pair(x, y) => RawExprRec::Pair(
                Box::new(substitute(variable, with, *x)),
                Box::new(substitute(variable, with, *y)),
            ),
            RawExprRec::Apply(f, xs) => RawExprRec::Apply(
                Box::new(substitute(variable, with, *f)),
                xs.into_vec()
                    .into_iter()
                    .map(|x| substitute(variable, with, x))
                    .collect(),
            ),
            RawExprRec::Var(x) => {
                if *variable == x {
                    with.clone().expr
                } else {
                    RawExprRec::Var(x)
                }
            }
            RawExprRec::Lambda(vars, statements, expr) => RawExprRec::Lambda(
                vars,
                statements
                    .into_iter()
                    .map(|statement| {
                        substitute_statement(variable, with, statement)
                    })
                    .collect(),
                Box::new(substitute(variable, with, *expr)),
            ),
            _ => in_expr.expr,
        },
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

impl Expr for RawExpr<usize> {
    fn evaluated(self) -> RawExpr<usize> {
        self
    }

    fn none() -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::None,
        }
    }

    fn unit() -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::Unit,
        }
    }

    fn int(value: i32) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::Int(value),
        }
    }

    fn string(value: i32) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::String(value.to_string()),
        }
    }

    fn float(value: f32) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::Float(value),
        }
    }

    fn bool(value: bool) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::Bool(value),
        }
    }

    fn type_(value: ExprType) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::Type(value),
        }
    }

    fn pair(lhs: Self, rhs: Self) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::Pair(Box::new(lhs), Box::new(rhs)),
        }
    }

    fn list(elements: Vec<Self>) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::List(elements),
        }
    }

    fn node(value: usize) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::Node(value),
        }
    }

    fn builtin_function(value: FunctionID) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::BuiltinFunction(value),
        }
    }

    fn polymorphic_function(value: PolymorphicFunctionID) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::PolymorphicFunction(value),
        }
    }

    fn lambda(
        vars: Vec<usize>,
        stmts: Vec<Statement<usize>>,
        body: Self,
    ) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::Lambda(vars, stmts, Box::new(body)),
        }
    }

    fn apply<const N: usize>(fun: Self, args: [Self; N]) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::Apply(Box::new(fun), Box::new(args)),
        }
    }

    fn var(value: usize) -> Self {
        RawExpr {
            location: None,
            expr: RawExprRec::Var(value),
        }
    }
}
