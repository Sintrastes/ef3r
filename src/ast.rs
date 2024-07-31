use std::fmt::Display;

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

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Int(x) => x.fmt(f),
            Expr::String(x) => x.fmt(f),
            Expr::Float(x) => x.fmt(f),
            Expr::Action(_) => todo!(),
            Expr::BuiltinFunction(_) => todo!(),
            Expr::Lambda(var, body) => {
                f.write_str("\\")?;
                f.write_str(var)?;
                f.write_str(" -> ")?;
                body.fmt(f)
            }
            Expr::Apply(f, x) => {
                todo!()
            }
            Expr::Var(_) => todo!(),
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
