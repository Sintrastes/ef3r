use serde::{Deserialize, Serialize};


pub type FunctionID = i32;

pub type ActionID = i32;

pub type VariableID = String;

#[derive(Clone, Serialize, Deserialize)]
pub enum Expr {
    Int(i32),
    String(String),
    Float(f32),
    Action(ActionID),
    // TODO: Not sure what the best approach is to deal with FRP constructs in
    // the language.
    BuiltinFunction(FunctionID),
    Lambda(VariableID, Box<Expr>),
    Apply(Box<Expr>, Box<[Expr]>),
    Var(VariableID)
}

///
/// An expression together with an optional un-evaluated
///  version of said expression, showing the history of how that expression has
///  been built up.
/// 
/// If it exists, the trace should evaluate to evaluated.
/// 
#[derive(Clone, Serialize, Deserialize)]
pub struct TracedExpr {
    pub evaluated: Expr,
    pub trace: Option<Expr>
}

#[derive(Clone, Serialize, Deserialize)]
pub enum Statement {
    /// Initialize the variable with an expression.
    Var(VariableID, Expr),
    /// Execute a side-effecting expression (Action).
    Execute(Expr)
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
    token_number: i32
}