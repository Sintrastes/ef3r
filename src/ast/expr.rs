use crate::{interpreter::PolymorphicFunctionID, types::ExprType};

use super::{raw_expr::RawExpr, Statement};

pub type FunctionID = u32;

pub type VariableID = String;

///
/// A trait defining the contract for implementing a concrete expression
///  type. For instance, RawExpr or TracedExpr.
///
pub trait Expr: Sized {
    fn evaluated(self) -> RawExpr<u32>;

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
        vars: Vec<u32>,
        stmts: Vec<Statement<u32>>,
        body: Box<Self>,
    ) -> Self;

    fn apply(fun: Box<Self>, args: Box<[Self]>) -> Self;

    fn var(value: u32) -> Self;
}
