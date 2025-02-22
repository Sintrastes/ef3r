use crate::{
    interpreter::{PolymorphicFunctionID, VariableId},
    types::ExprType,
};

use super::{raw_expr::RawExpr, Statement};

pub type FunctionID = usize;

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
