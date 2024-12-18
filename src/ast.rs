use std::{
    fmt::{Display, Pointer},
    thread::JoinHandle,
};

use quickcheck::{Arbitrary, Gen};
use serde::{Deserialize, Serialize};

use crate::{interpreter::Context, stdlib::ef3r_stdlib, types::ExprType};

pub type FunctionID = u32;

pub type VariableID = String;

///
/// Abstract Syntax Tree for the expression language of
///  ef3f.
///
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Expr {
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
    Pair(Box<TracedExpr>, Box<TracedExpr>),
    /// Type of lists
    List(Vec<TracedExpr>),
    /// Reference to a node that has been created and stored in the
    ///  interpreter context.
    Node(usize),
    BuiltinFunction(FunctionID),
    /// Lambda expression with 0+ parameters: \x y z -> f x
    /// Can either be an "effectful" lambda with statements and a final
    /// return result, or (if there are no statements), a pure function.
    Lambda(Vec<VariableID>, Vec<Statement>, Box<TracedExpr>),
    /// Function application: f x
    Apply(Box<TracedExpr>, Box<[TracedExpr]>),
    /// Locally defined variable.
    Var(VariableID),
}

impl Expr {
    pub fn traced(self) -> TracedExpr {
        TracedExpr::new(self)
    }
}

impl Arbitrary for Expr {
    fn arbitrary(g: &mut Gen) -> Self {
        let context = ef3r_stdlib();

        Self::arbitrary_with_depth(&context, g, 0)
    }
}

impl Expr {
    fn arbitrary_with_depth(
        context: &Context,
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
            0 => Expr::None,
            1 => Expr::Int(i32::arbitrary(g)),
            2 => Expr::String(String::arbitrary(g)),
            3 => loop {
                let value = f32::arbitrary(g);
                if !value.is_nan() {
                    return Expr::Float(value);
                }
            },
            4 => {
                let keys: Vec<&u32> =
                    context.expression_context.functions.keys().collect();
                let key = **g.choose(&keys).unwrap();
                Expr::BuiltinFunction(key)
            }
            5 => Expr::Lambda(
                Vec::arbitrary(g),
                vec![],
                Box::new(
                    Expr::arbitrary_with_depth(context, g, depth + 1).traced(),
                ),
            ),
            6 => {
                let num_args = usize::arbitrary(g) % 5; // Limit the number of arguments to a small number
                let args = (0..num_args)
                    .map(|_| {
                        Expr::arbitrary_with_depth(context, g, depth + 1)
                            .traced()
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                Expr::Apply(
                    Box::new(
                        Expr::arbitrary_with_depth(context, g, depth + 1)
                            .traced(),
                    ),
                    args,
                )
            }
            7 => Expr::Var(String::arbitrary(g)),
            _ => unreachable!(),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let context = ef3r_stdlib();

        match self {
            Expr::None => f.write_str("None"),
            Expr::Unit => f.write_str("Unit"),
            Expr::Int(x) => x.fmt(f),
            Expr::Type(x) => x.fmt(f),
            Expr::String(x) => {
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
            Expr::Float(x) => x.fmt(f),
            Expr::BuiltinFunction(x) => {
                let name = context
                    .expression_context
                    .functions
                    .get(x)
                    .unwrap()
                    .name
                    .clone();

                f.write_str(name.as_str())
            }
            Expr::List(elements) => {
                f.write_str("[")?;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    element.fmt(f)?;
                }
                f.write_str("]")
            }
            Expr::Lambda(vars, _, body) => {
                f.write_str("\\")?;
                for var in vars {
                    f.write_str(var)?;
                }
                f.write_str(" -> ")?;
                body.as_ref().fmt(f)
            }
            Expr::Apply(fun, args) => {
                f.write_str("(")?;
                fun.as_ref().fmt(f)?;
                for arg in args {
                    f.write_str(" ")?;
                    arg.fmt(f)?;
                }
                f.write_str(")")
            }
            Expr::Var(x) => x.fmt(f),
            Expr::Node(idx) => {
                f.write_str("Node(")?;
                idx.fmt(f)?;
                f.write_str(")")
            }
            Expr::Bool(value) => value.fmt(f),
            Expr::Pair(traced_expr, traced_expr1) => {
                f.write_str("(")?;
                traced_expr.as_ref().fmt(f)?;
                f.write_str(",")?;
                traced_expr1.as_ref().fmt(f)?;
                f.write_str(")")
            }
        }
    }
}

/// Trait to map Rust types to ExprTypes
pub trait ExprTypeable {
    fn expr_type() -> ExprType;
    fn try_from_expr(expr: &Expr) -> Option<Self>
    where
        Self: Sized;
    fn to_expr(self) -> Expr;
}

// Implementations for basic types
impl ExprTypeable for i32 {
    fn expr_type() -> ExprType {
        ExprType::Int
    }
    fn try_from_expr(expr: &Expr) -> Option<Self> {
        if let Expr::Int(x) = expr {
            Some(*x)
        } else {
            None
        }
    }
    fn to_expr(self) -> Expr {
        Expr::Int(self)
    }
}

impl ExprTypeable for f32 {
    fn expr_type() -> ExprType {
        ExprType::Float
    }
    fn try_from_expr(expr: &Expr) -> Option<Self> {
        if let Expr::Float(x) = expr {
            Some(*x)
        } else {
            None
        }
    }
    fn to_expr(self) -> Expr {
        Expr::Float(self)
    }
}

impl ExprTypeable for String {
    fn expr_type() -> ExprType {
        ExprType::String
    }
    fn try_from_expr(expr: &Expr) -> Option<Self> {
        if let Expr::String(x) = expr {
            Some(x.clone())
        } else {
            None
        }
    }
    fn to_expr(self) -> Expr {
        Expr::String(self)
    }
}

impl ExprTypeable for bool {
    fn expr_type() -> ExprType {
        ExprType::Bool
    }
    fn try_from_expr(expr: &Expr) -> Option<Self> {
        if let Expr::Bool(x) = expr {
            Some(*x)
        } else {
            None
        }
    }
    fn to_expr(self) -> Expr {
        Expr::Bool(self)
    }
}

impl ExprTypeable for () {
    fn expr_type() -> ExprType {
        ExprType::Unit
    }
    fn try_from_expr(expr: &Expr) -> Option<Self> {
        if let Expr::Unit = expr {
            Some(())
        } else {
            None
        }
    }
    fn to_expr(self) -> Expr {
        Expr::Unit
    }
}

impl ExprTypeable for Vec<TracedExpr> {
    fn expr_type() -> ExprType {
        ExprType::List(Box::new(ExprType::Any))
    }
    fn try_from_expr(expr: &Expr) -> Option<Self> {
        if let Expr::List(x) = expr {
            Some(x.clone())
        } else {
            None
        }
    }
    fn to_expr(self) -> Expr {
        Expr::List(self)
    }
}

impl ExprTypeable for Expr {
    fn expr_type() -> ExprType {
        ExprType::Any
    }
    fn try_from_expr(expr: &Expr) -> Option<Self> {
        Some(expr.clone())
    }
    fn to_expr(self) -> Expr {
        self
    }
}

///
/// An expression together with an optional un-evaluated
///  version of said expression, showing the history of how that expression has
///  been built up.
///
/// If it exists, the trace should evaluate to evaluated.
///
#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct TracedExpr {
    pub evaluated: Expr,
    pub stored_trace: Option<Expr>,
}

impl TracedExpr {
    pub fn build(evaluated: Expr, trace: Option<Expr>) -> TracedExpr {
        TracedExpr {
            evaluated: evaluated,
            stored_trace: trace,
        }
    }

    pub fn get_trace(&self) -> Expr {
        match &self.stored_trace {
            Some(expr) => expr.clone(),
            None => self.evaluated.clone(),
        }
    }

    pub fn new(expr: Expr) -> TracedExpr {
        TracedExpr {
            evaluated: expr,
            stored_trace: None,
        }
    }
}

impl Display for TracedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.evaluated.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use crate::{
        ast::Expr,
        interpreter::{evaluate_traced, unwind_trace},
        stdlib::{ef3r_stdlib, ADD_ID, MUL_ID},
    };

    #[test]
    fn evaluation_keeps_trace() {
        let context = Arc::new(Mutex::new(ef3r_stdlib()));

        // Example expression.
        let expression = Expr::Apply(
            Box::new(Expr::BuiltinFunction(MUL_ID).traced()),
            Box::new([
                Expr::Int(2).traced(),
                Expr::Apply(
                    Box::new(Expr::BuiltinFunction(ADD_ID).traced()),
                    Box::new([Expr::Int(1).traced(), Expr::Int(2).traced()]),
                )
                .traced(),
            ]),
        );

        let evaluated =
            evaluate_traced(context, expression.clone().traced()).unwrap();

        println!("Evaluated: {}", evaluated.evaluated);

        assert_eq!(evaluated.evaluated, Expr::Int(6));

        println!("Trace: {}", evaluated.stored_trace.clone().unwrap());

        assert_eq!(evaluated.stored_trace, Some(expression));
    }

    #[test]
    fn evaluating_twice_keeps_entire_trace() {
        let context = Arc::new(Mutex::new(ef3r_stdlib()));

        // Example expression.
        let expression = Expr::Apply(
            Box::new(Expr::BuiltinFunction(MUL_ID).traced()),
            Box::new([
                Expr::Int(2).traced(),
                Expr::Apply(
                    Box::new(Expr::BuiltinFunction(ADD_ID).traced()),
                    Box::new([Expr::Int(1).traced(), Expr::Int(2).traced()]),
                )
                .traced(),
            ]),
        );

        let evaluated =
            evaluate_traced(context.clone(), expression.clone().traced())
                .unwrap();

        let second_expression = Expr::Apply(
            Box::new(Expr::BuiltinFunction(MUL_ID).traced()),
            Box::new([Expr::Int(2).traced(), evaluated]),
        );

        let second_evaluated =
            evaluate_traced(context, second_expression.clone().traced())
                .unwrap();

        let expected = Expr::Apply(
            Box::new(Expr::BuiltinFunction(MUL_ID).traced()),
            Box::new([Expr::Int(2).traced(), expression.traced()]),
        );

        println!(
            "Evaluated: {}",
            unwind_trace(second_evaluated.clone()).clone()
        );
        println!("Expected: {}", expected);

        assert_eq!(unwind_trace(second_evaluated).clone().evaluated, expected);
    }
}

///
/// An imperative statement to be executed in the ef3r runtime.
///
/// Execute an expression and optionally bind
///  its result to a variable.
///
#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Statement {
    pub var: Option<VariableID>,
    pub expr: Expr,
}

///
/// We'll need to keep track to the line, and
/// the token number on each line in order to
/// enable fine-grained debugging based on
/// sub-expressions.
///
#[derive(Clone, Serialize, Deserialize)]
pub struct DebugInfo {
    line_number: i32,
    token_number: i32,
}

// Question: How would this work with traced expressions?
pub fn substitute(variable: String, with: Expr, in_expr: Expr) -> Expr {
    match in_expr {
        Expr::Pair(x, y) => Expr::Pair(
            Box::new(
                substitute(variable.clone(), with.clone(), x.evaluated)
                    .traced(),
            ),
            Box::new(substitute(variable, with, y.evaluated).traced()),
        ),
        Expr::Apply(f, xs) => Expr::Apply(
            Box::new(
                substitute(variable.clone(), with.clone(), f.evaluated)
                    .traced(),
            ),
            xs.into_vec()
                .into_iter()
                .map(|x| {
                    substitute(variable.clone(), with.clone(), x.evaluated)
                        .traced()
                })
                .collect(),
        ),
        Expr::Var(x) => {
            if variable == x {
                with
            } else {
                Expr::Var(x)
            }
        }
        Expr::Lambda(_, _, _) => todo!(),
        _ => in_expr,
    }
}
