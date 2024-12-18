use crate::{ast::Expr, types::ExprType};

/// Attempts to infer the type of expressions.
pub fn type_of(term: &Expr) -> Option<ExprType> {
    match term {
        Expr::None => Some(ExprType::Any),
        Expr::Unit => Some(ExprType::Unit),
        Expr::Int(_) => Some(ExprType::Int),
        Expr::String(_) => Some(ExprType::String),
        Expr::Float(_) => Some(ExprType::Float),
        Expr::Bool(_) => Some(ExprType::Bool),
        Expr::Type(_) => Some(ExprType::Type),
        Expr::List(xs) => {
            let element_types = xs
                .iter()
                .map(|x| type_of(&x.evaluated).unwrap_or(ExprType::Any))
                .collect::<Vec<_>>();
            let unified_type = element_types
                .into_iter()
                .fold(ExprType::Any, |acc, t| union_type(&acc, &t));
            Some(ExprType::List(Box::new(unified_type)))
        }
        Expr::Pair(traced_expr, traced_expr1) => Some(ExprType::Pair(
            Box::new(type_of(&traced_expr.evaluated)?),
            Box::new(type_of(&traced_expr1.evaluated)?),
        )),
        Expr::BuiltinFunction(_) => Some(ExprType::Func(
            vec![ExprType::Unit],
            Box::new(ExprType::Unit),
        )),
        Expr::Node(_) => Some(ExprType::Node(Box::new(ExprType::Any))),
        Expr::Lambda(args, _, traced_expr) => {
            let arg_types: Vec<ExprType> = args
                .iter()
                .map(|_| ExprType::Any) // All args are assumed to be any type
                .collect();
            let return_type = type_of(&traced_expr.evaluated)?;

            Some(ExprType::Func(arg_types, Box::new(return_type)))
        }
        Expr::Apply(_, _) => todo!(),
        Expr::Var(_) => Some(ExprType::Any),
    }
}

fn union_type(t1: &ExprType, t2: &ExprType) -> ExprType {
    if t1 == t2 {
        t1.clone()
    } else {
        ExprType::Any
    }
}
