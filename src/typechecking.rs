use crate::{ast::Expr, types::ExprType};

/// Attempts to infer the type of expressions.
pub fn type_of(term: &Expr) -> Option<ExprType> {
    match term {
        Expr::None => todo!(),
        Expr::Unit => Some(ExprType::Unit),
        Expr::Int(_) => Some(ExprType::Int),
        Expr::String(_) => Some(ExprType::String),
        Expr::Float(_) => Some(ExprType::Float),
        Expr::Bool(_) => Some(ExprType::Bool),
        Expr::Type(expr_type) => Some(ExprType::Type),
        Expr::Pair(traced_expr, traced_expr1) => Some(ExprType::Pair(
            Box::new(type_of(&traced_expr.evaluated)?),
            Box::new(type_of(&traced_expr1.evaluated)?),
        )),
        Expr::BuiltinFunction(_) => Some(ExprType::Func(
            Box::new(ExprType::Unit),
            Box::new(ExprType::Unit),
        )),
        Expr::Node(_) => todo!(),
        Expr::MapNode(traced_expr, traced_expr1) => todo!(),
        Expr::Lambda(args, statements, traced_expr) => todo!(),
        Expr::Apply(traced_expr, _) => todo!(),
        Expr::Var(_) => None,
    }
}
