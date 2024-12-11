use serde::{Deserialize, Serialize};

/// The type of types that can be assigned to ef3r terms.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ExprType {
    Any,
    Unit,
    Int,
    Float,
    String,
    Bool,
    Type,
    Node(Box<ExprType>),
    Func(Vec<ExprType>, Box<ExprType>),
    Pair(Box<ExprType>, Box<ExprType>),
}
