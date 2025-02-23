use crate::{
    interpreter::{PolymorphicFunctionID, VariableId},
    types::ExprType,
};

use derive_more::{From, Into};
use serde::{Deserialize, Serialize};

use super::{raw_expr::RawExpr, Statement};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, From, Into, Serialize, Deserialize,
)]
pub struct FunctionID(usize);

impl FunctionID {
    pub fn new(raw_id: usize) -> FunctionID {
        FunctionID(raw_id)
    }
}

///
/// A trait defining the contract for implementing a concrete expression
///  type. For instance, RawExpr or TracedExpr.
///
pub trait Expr: Sized {
    fn evaluated(self) -> RawExpr<VariableId>;

    fn none() -> Self;

    fn unit() -> Self;

    fn int(value: i32) -> Self;

    fn string(value: i32) -> Self;

    fn float(value: f32) -> Self;

    fn bool(value: bool) -> Self;

    fn type_(value: ExprType) -> Self;

    fn pair(lhs: Self, rhs: Self) -> Self;

    fn list(elements: Vec<Self>) -> Self;

    fn node(value: usize) -> Self;

    fn builtin_function(value: FunctionID) -> Self;

    fn polymorphic_function(value: PolymorphicFunctionID) -> Self;

    fn lambda(
        vars: Vec<VariableId>,
        stmts: Vec<Statement<VariableId>>,
        body: Self,
    ) -> Self;

    fn apply<const N: usize>(fun: Self, args: [Self; N]) -> Self;

    fn var(value: VariableId) -> Self;
}
