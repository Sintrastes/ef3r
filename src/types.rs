use serde::{Deserialize, Serialize};

/// The type of types that can be assigned to ef3r terms.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ExprType {
    Unit,
    Int,
    Float,
    String,
    Bool,
    Type,
    Func(Box<ExprType>, Box<ExprType>),
    Pair(Box<ExprType>, Box<ExprType>),
}
