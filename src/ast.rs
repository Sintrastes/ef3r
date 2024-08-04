use std::fmt::Display;

use quickcheck::{Arbitrary, Gen};
use serde::{Deserialize, Serialize};

pub type FunctionID = u32;

pub type ActionID = u32;

pub type VariableID = String;

///
/// Abstract Syntax Tree for the expression language of
///  ef3f.
///
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Expr {
    /// Null symbol.
    None,
    /// Integer literal
    Int(i32),
    /// String literal
    String(String),
    /// Float literal
    Float(f32),
    /// Action reference.
    Action(ActionID),
    /// TODO: Not sure what the best approach is to deal with FRP constructs in
    /// the language.
    BuiltinFunction(FunctionID),
    /// Lambda expression: \x -> f x
    Lambda(VariableID, Box<Expr>),
    /// Function application: f x
    Apply(Box<Expr>, Box<[Expr]>),
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
        Self::arbitrary_with_depth(g, 0)
    }
}

impl Expr {
    fn arbitrary_with_depth(g: &mut Gen, depth: usize) -> Self {
        let choices = if depth < 5 {
            vec![
                0, // None
                1, // Int
                2, // String
                3, // Float
                4, // Action
                5, // BuiltinFunction
                6, // Lambda
                7, // Apply
                8, // Var
            ]
        } else {
            vec![
                0, // None
                1, // Int
                2, // String
                3, // Float
                4, // Action
                5, // BuiltinFunction
                8, // Var
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
            4 => Expr::Action(u32::arbitrary(g)),
            5 => Expr::BuiltinFunction(u32::arbitrary(g)),
            6 => Expr::Lambda(
                String::arbitrary(g),
                Box::new(Expr::arbitrary_with_depth(g, depth + 1)),
            ),
            7 => {
                let num_args = usize::arbitrary(g) % 5; // Limit the number of arguments to a small number
                let args = (0..num_args)
                    .map(|_| Expr::arbitrary_with_depth(g, depth + 1))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                Expr::Apply(
                    Box::new(Expr::arbitrary_with_depth(g, depth + 1)),
                    args,
                )
            }
            8 => Expr::Var(String::arbitrary(g)),
            _ => unreachable!(),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::None => f.write_str("None"),
            Expr::Int(x) => x.fmt(f),
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
            Expr::Action(x) => x.fmt(f),
            Expr::BuiltinFunction(x) => x.fmt(f),
            Expr::Lambda(var, body) => {
                f.write_str("\\")?;
                f.write_str(var)?;
                f.write_str(" -> ")?;
                body.fmt(f)
            }
            Expr::Apply(fun, args) => {
                fun.fmt(f)?;
                f.write_str(" ")?;
                for arg in args {
                    arg.fmt(f)?;
                    f.write_str(" ")?;
                }
                Ok(())
            }
            Expr::Var(x) => x.fmt(f),
        }
    }
}

///
/// An expression together with an optional un-evaluated
///  version of said expression, showing the history of how that expression has
///  been built up.
///
/// If it exists, the trace should evaluate to evaluated.
///
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracedExpr {
    pub evaluated: Expr,
    pub trace: Option<Expr>,
}

impl TracedExpr {
    pub fn new(expr: Expr) -> TracedExpr {
        TracedExpr {
            evaluated: expr,
            trace: None,
        }
    }
}

///
/// An imperative statement to be executed in the ef3r runtime.
///
#[derive(Clone, Serialize, Deserialize)]
pub enum Statement {
    /// Initialize the variable with an expression.
    Var(VariableID, Expr),
    /// Execute a side-effecting expression (Action) and optionally bind
    ///  its result to a variable.
    Execute(Option<VariableID>, Expr),
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
