use crate::{
    ast::traced_expr::TracedExprRec, debugging::Debugger,
    interpreter::ExpressionContext, types::ExprType,
};

/// Attempts to infer the type of expressions.
pub fn type_of<T: Debugger + 'static>(
    ctx: &ExpressionContext<T>,
    term: &TracedExprRec<usize>,
) -> Option<ExprType> {
    match term {
        TracedExprRec::None => Some(ExprType::Any),
        TracedExprRec::Unit => Some(ExprType::Unit),
        TracedExprRec::Int(_) => Some(ExprType::Int),
        TracedExprRec::String(_) => Some(ExprType::String),
        TracedExprRec::Float(_) => Some(ExprType::Float),
        TracedExprRec::Bool(_) => Some(ExprType::Bool),
        TracedExprRec::Type(_) => Some(ExprType::Type),
        TracedExprRec::List(xs) => {
            let element_types = xs
                .iter()
                .map(|x| type_of(ctx, &x.evaluated).unwrap_or(ExprType::Any))
                .collect::<Vec<_>>();
            let unified_type = element_types
                .into_iter()
                .fold(ExprType::Any, |acc, t| union_type(&acc, &t));
            Some(ExprType::List(Box::new(unified_type)))
        }
        TracedExprRec::Pair(traced_expr, traced_expr1) => Some(ExprType::Pair(
            Box::new(type_of(ctx, &traced_expr.evaluated)?),
            Box::new(type_of(ctx, &traced_expr1.evaluated)?),
        )),
        TracedExprRec::BuiltinFunction(fn_id) => Some(ExprType::Func(
            ctx.functions[*fn_id].argument_types.to_vec(),
            Box::new(
                ctx.functions
                    .get(*fn_id)
                    .map(|f| f.result_type.clone())
                    .unwrap_or(ExprType::Any),
            ),
        )),
        TracedExprRec::Node(_) => Some(ExprType::Node(Box::new(ExprType::Any))),
        TracedExprRec::Lambda(args, _, traced_expr) => {
            let arg_types: Vec<ExprType> = args
                .iter()
                .map(|_| ExprType::Any) // All args are assumed to be any type
                .collect();
            let return_type = type_of(ctx, &traced_expr.evaluated)?;

            Some(ExprType::Func(arg_types, Box::new(return_type)))
        }
        // Note: This may need to be refined if we ever add implicit partial application.
        TracedExprRec::Apply(f, _) => {
            type_of(ctx, &f.evaluated).and_then(|f_type| match f_type {
                ExprType::Func(_, return_type) => Some(*return_type),
                _ => None,
            })
        }
        // Note: In the future we might want to memoize results and store them in a
        //  "typing_context" for efficency reasons.
        TracedExprRec::Var(var) => ctx
            .variables
            .get(var)
            .and_then(|expr| type_of(ctx, &expr.evaluated)),
        // We do not currently assign a type to polymorphic functions.
        // But maybe we could assign something like Any -> Any based on
        // the arity of the function?
        TracedExprRec::PolymorphicFunction(_) => None,
    }
}

fn union_type(t1: &ExprType, t2: &ExprType) -> ExprType {
    if t1 == t2 {
        t1.clone()
    } else {
        ExprType::Any
    }
}
