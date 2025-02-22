use std::collections::HashMap;

use crate::{
    ast::{traced_expr::TracedExprRec, Statement},
    debugging::Debugger,
    interpreter::{ExpressionContext, VariableId},
    parser::CodeLocation,
    types::ExprType,
};

#[derive(Clone)] // Maybe we can do better in the future, but for MVP
                 // we can just clone to create a derived context
pub struct TypingContext {
    pub scope_variables: HashMap<VariableId, ExprType>,
}

impl TypingContext {
    pub fn new() -> TypingContext {
        TypingContext {
            scope_variables: HashMap::new(),
        }
    }
}

/// Attempts to infer the type of expressions.
pub fn type_of<T: Debugger>(
    ctx: &ExpressionContext<T>,
    typing_context: &TypingContext,
    term: &TracedExprRec<VariableId>,
) -> Result<ExprType, Vec<TypeError>> {
    match term {
        TracedExprRec::None => Ok(ExprType::Any),
        TracedExprRec::Unit => Ok(ExprType::Unit),
        TracedExprRec::Int(_) => Ok(ExprType::Int),
        TracedExprRec::String(_) => Ok(ExprType::String),
        TracedExprRec::Float(_) => Ok(ExprType::Float),
        TracedExprRec::Bool(_) => Ok(ExprType::Bool),
        TracedExprRec::Type(_) => Ok(ExprType::Type),
        TracedExprRec::List(xs) => {
            let element_types = xs
                .iter()
                .map(|x| {
                    type_of::<T>(ctx, typing_context, &x.evaluated)
                        .unwrap_or(ExprType::Any)
                })
                .collect::<Vec<_>>();
            let unified_type = element_types
                .into_iter()
                .fold(ExprType::Any, |acc, t| union_type(&acc, &t));
            Ok(ExprType::List(Box::new(unified_type)))
        }
        TracedExprRec::Pair(traced_expr, traced_expr1) => Ok(ExprType::Pair(
            Box::new(type_of::<T>(
                ctx,
                typing_context,
                &traced_expr.evaluated,
            )?),
            Box::new(type_of::<T>(
                ctx,
                typing_context,
                &traced_expr1.evaluated,
            )?),
        )),
        TracedExprRec::BuiltinFunction(fn_id) => Ok(ExprType::Func(
            ctx.functions[*fn_id].1.argument_types.to_vec(),
            Box::new(
                ctx.functions
                    .get(*fn_id)
                    .map(|f| f.1.result_type.clone())
                    .unwrap_or(ExprType::Any),
            ),
        )),
        TracedExprRec::JoinHandle(_) => Err(vec![]),
        TracedExprRec::Node(_) => Ok(ExprType::Node(Box::new(ExprType::Any))),
        TracedExprRec::Lambda(args, stmts, traced_expr) => {
            // Note: In order to properly typecheck this, we need to
            // enter a new typechecking context to be able to account for local
            // variables.
            let arg_types: Vec<ExprType> = args
                .iter()
                .map(|_| ExprType::Any) // All args are assumed to be any type
                // unless annotated otherwise.
                .collect();

            // Create a new typing context for the lambda that we can add
            // local variables to.
            let mut local_context = typing_context.clone();

            let errors = typecheck(ctx, &mut local_context, stmts.to_vec());

            if !errors.is_empty() {
                return Err(errors);
            }

            let return_type =
                type_of::<T>(ctx, typing_context, &traced_expr.evaluated)?;

            Ok(ExprType::Func(arg_types, Box::new(return_type)))
        }
        // Note: This may need to be refined if we ever add implicit partial application.
        TracedExprRec::Apply(f, _) => {
            type_of::<T>(ctx, typing_context, &f.evaluated).and_then(|f_type| {
                match f_type {
                    ExprType::Func(_, return_type) => Ok(*return_type),
                    _ => Err(vec![TypeError::MalformedExpression {
                        loc: f.location,
                    }]),
                }
            })
        }
        TracedExprRec::Var(var) => {
            match typing_context.scope_variables.get(&var) {
                Some(typ) => Ok(typ.clone()),
                None => match ctx.variables.get(&var) {
                    Some(expr) => {
                        type_of::<T>(ctx, typing_context, &expr.evaluated)
                    }
                    None => Err(vec![]),
                },
            }
        }
        // We do not currently assign a type to polymorphic functions.
        // But maybe we could assign something like Any -> Any based on
        // the arity of the function?
        TracedExprRec::PolymorphicFunction(_) => Err(vec![]),
    }
}

fn union_type(t1: &ExprType, t2: &ExprType) -> ExprType {
    if t1 == t2 {
        t1.clone()
    } else {
        ExprType::Any
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExpectationReason {
    BecauseOfTypeAnnotation { loc: Option<CodeLocation> },
}

///
/// Typecheck an entire program, returning any type errors encountered,
///  and adding any typechecked variables into the typing context.
///
pub fn typecheck<T: Debugger>(
    ctx: &ExpressionContext<T>,
    typing_context: &mut TypingContext,
    program: Vec<Statement<VariableId>>,
) -> Vec<TypeError> {
    let mut errors = vec![];

    for statement in program {
        let statement_type = if let Ok(t) =
            type_of(ctx, &typing_context, &statement.expr.from_raw().evaluated)
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
                    actual: statement_type.clone(),
                    reason: ExpectationReason::BecauseOfTypeAnnotation {
                        loc: None,
                    },
                });
            }
        }

        // Add the type to the typing context.
        if let Some(var) = statement.var {
            typing_context.scope_variables.insert(var, statement_type);
        }
    }

    errors
}
