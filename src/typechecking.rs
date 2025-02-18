use crate::{
    ast::{traced_expr::TracedExprRec, Statement},
    debugging::Debugger,
    interpreter::ExpressionContext,
    parser::CodeLocation,
    types::ExprType,
};

/// Attempts to infer the type of expressions.
pub fn type_of<T: Debugger + 'static, V, Lookup: TypingLookup<T, V>>(
    ctx: &ExpressionContext<T>,
    term: &TracedExprRec<V>,
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
                    type_of::<T, V, Lookup>(ctx, &x.evaluated)
                        .unwrap_or(ExprType::Any)
                })
                .collect::<Vec<_>>();
            let unified_type = element_types
                .into_iter()
                .fold(ExprType::Any, |acc, t| union_type(&acc, &t));
            Some(ExprType::List(Box::new(unified_type)))
        }
        TracedExprRec::Pair(traced_expr, traced_expr1) => Some(ExprType::Pair(
            Box::new(type_of::<T, V, Lookup>(ctx, &traced_expr.evaluated)?),
            Box::new(type_of::<T, V, Lookup>(ctx, &traced_expr1.evaluated)?),
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
            let arg_types: Vec<ExprType> = args
                .iter()
                .map(|_| ExprType::Any) // All args are assumed to be any type
                .collect();
            let return_type =
                type_of::<T, V, Lookup>(ctx, &traced_expr.evaluated)?;

            Some(ExprType::Func(arg_types, Box::new(return_type)))
        }
        // Note: This may need to be refined if we ever add implicit partial application.
        TracedExprRec::Apply(f, _) => {
            type_of::<T, V, Lookup>(ctx, &f.evaluated).and_then(|f_type| {
                match f_type {
                    ExprType::Func(_, return_type) => Some(*return_type),
                    _ => None,
                }
            })
        }
        // Note: In the future we might want to memoize results and store them in a
        //  "typing_context" for efficency reasons.
        TracedExprRec::Var(var) => Lookup::lookup_var_type(ctx, var),
        // We do not currently assign a type to polymorphic functions.
        // But maybe we could assign something like Any -> Any based on
        // the arity of the function?
        TracedExprRec::PolymorphicFunction(_) => None,
    }
}

///
/// Trait for different techniques for looking up type information.
///
pub trait TypingLookup<T: Debugger + 'static, V> {
    fn lookup_var_type(ctx: &ExpressionContext<T>, var: &V)
        -> Option<ExprType>;
}

pub struct RuntimeLookup;

impl<T: Debugger + 'static> TypingLookup<T, usize> for RuntimeLookup {
    fn lookup_var_type(
        ctx: &ExpressionContext<T>,
        var: &usize,
    ) -> Option<ExprType> {
        ctx.variables
            .get(&var)
            .and_then(|expr| type_of::<T, usize, Self>(ctx, &expr.evaluated))
    }
}

pub struct NoLookup;

impl<T: Debugger + 'static, V> TypingLookup<T, V> for NoLookup {
    fn lookup_var_type(
        _ctx: &ExpressionContext<T>,
        _var: &V,
    ) -> Option<ExprType> {
        None
    }
}

fn union_type(t1: &ExprType, t2: &ExprType) -> ExprType {
    if t1 == t2 {
        t1.clone()
    } else {
        ExprType::Any
    }
}

pub enum TypeError {
    ExpectedButActual {
        expected: ExprType,
        actual: ExprType,
        reason: ExpectationReason,
    },
    // TODO: Probably refine this error in the future
    MalformedExpression,
}

pub enum ExpectationReason {
    BecauseOfTypeAnnotation { loc: Option<CodeLocation> },
}

///
/// Typecheck an entire program, returning any type errors encountered.
///
pub fn typecheck<T: Debugger>(
    ctx: &ExpressionContext<T>,
    program: Vec<Statement<usize>>,
) -> Vec<TypeError> {
    let mut errors = vec![];

    for statement in program {
        let statement_type = if let Some(t) = type_of::<_, _, RuntimeLookup>(
            ctx,
            &statement.expr.from_raw().evaluated,
        ) {
            t
        } else {
            errors.push(TypeError::MalformedExpression);
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
