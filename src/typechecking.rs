use crate::{
    ast::{traced_expr::TracedExprRec, Statement},
    debugging::Debugger,
    interpreter::{ExpressionContext, VariableId},
    parser::CodeLocation,
    types::ExprType,
};

/// Attempts to infer the type of expressions.
pub fn type_of<T: Debugger>(
    ctx: &ExpressionContext<T>,
    term: &TracedExprRec<VariableId>,
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
                .map(|x| {
                    type_of::<T>(ctx, &x.evaluated).unwrap_or(ExprType::Any)
                })
                .collect::<Vec<_>>();
            let unified_type = element_types
                .into_iter()
                .fold(ExprType::Any, |acc, t| union_type(&acc, &t));
            Some(ExprType::List(Box::new(unified_type)))
        }
        TracedExprRec::Pair(traced_expr, traced_expr1) => Some(ExprType::Pair(
            Box::new(type_of::<T>(ctx, &traced_expr.evaluated)?),
            Box::new(type_of::<T>(ctx, &traced_expr1.evaluated)?),
        )),
        TracedExprRec::BuiltinFunction(fn_id) => Some(ExprType::Func(
            ctx.functions[*fn_id].1.argument_types.to_vec(),
            Box::new(
                ctx.functions
                    .get(*fn_id)
                    .map(|f| f.1.result_type.clone())
                    .unwrap_or(ExprType::Any),
            ),
        )),
        TracedExprRec::JoinHandle(_) => None,
        TracedExprRec::Node(_) => Some(ExprType::Node(Box::new(ExprType::Any))),
        TracedExprRec::Lambda(args, _, traced_expr) => {
            // Note: In order to properly typecheck this, we need to
            // enter a new typechecking context to be able to account for local
            // variables.
            let arg_types: Vec<ExprType> = args
                .iter()
                .map(|_| ExprType::Any) // All args are assumed to be any type
                .collect();
            let return_type = type_of::<T>(ctx, &traced_expr.evaluated)?;

            Some(ExprType::Func(arg_types, Box::new(return_type)))
        }
        // Note: This may need to be refined if we ever add implicit partial application.
        TracedExprRec::Apply(f, _) => {
            type_of::<T>(ctx, &f.evaluated).and_then(|f_type| match f_type {
                ExprType::Func(_, return_type) => Some(*return_type),
                _ => None,
            })
        }
        TracedExprRec::Var(var) => ctx
            .variables
            .get(&var)
            .and_then(|expr| type_of::<T>(ctx, &expr.evaluated)),
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

#[derive(Debug)]
pub enum TypeError {
    ExpectedButActual {
        expected: ExprType,
        actual: ExprType,
        reason: ExpectationReason,
    },
    // TODO: Probably refine this error in the future
    MalformedExpression {
        loc: Option<CodeLocation>,
    },
}

#[derive(Debug)]
pub enum ExpectationReason {
    BecauseOfTypeAnnotation { loc: Option<CodeLocation> },
}

///
/// Typecheck an entire program, returning any type errors encountered.
///
pub fn typecheck<T: Debugger>(
    ctx: &ExpressionContext<T>,
    program: Vec<Statement<VariableId>>,
) -> Vec<TypeError> {
    let mut errors = vec![];

    for statement in program {
        let statement_type = if let Some(t) =
            type_of(ctx, &statement.expr.from_raw().evaluated)
        {
            t
        } else {
            errors.push(TypeError::MalformedExpression {
                loc: statement.location,
            });
            break;
        };

        if let Some(expected) = statement.type_annotation {
            if statement_type != expected {
                errors.push(TypeError::ExpectedButActual {
                    expected,
                    actual: statement_type,
                    reason: ExpectationReason::BecauseOfTypeAnnotation {
                        loc: None,
                    },
                });
            }
        }

        // Add the type to the typing context.
        // TODO
    }

    errors
}
