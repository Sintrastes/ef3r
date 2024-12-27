use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use bimap::BiMap;
use daggy::Dag;

use crate::{
    ast::{
        substitute, Expr, FunctionID, RawExpr, Statement, TracedExpr,
        VariableID,
    },
    debugging::Debugger,
    frp::{with_lock, Node},
    typechecking::type_of,
    types::ExprType,
};

///
/// Local state needed to evaluate expressions and
///  update the FRP event loop.
///
#[derive(Clone)]
pub struct Context<'a, T: Debugger + 'static>
where
    'a: 'static,
{
    pub expression_context: ExpressionContext<T>,
    pub graph: Dag<Node<'a, T>, (), u32>,
    pub debugger: T,
}

unsafe impl<'a, T: Debugger> Send for Context<'a, T> {}

#[derive(PartialEq, Debug)]
pub enum EvaluationError {
    TypeError {
        expected: ExprType,
        actual: ExprType,
        at_loc: String,
    },
    NotAFunction(Expr<String>),
    VariableNotFound(String),
    Unimplemented,
    WrongNumberOfArguments {
        expected: usize,
        actual: usize,
        for_function: String,
    },
    CouldNotResolvePolymorphicFunction {
        id: u32,
        arg_types: Vec<ExprType>,
    },
}

#[derive(Clone)]
pub struct FunctionDefinition<T: Debugger + 'static> {
    pub argument_types: Vec<ExprType>,
    pub result_type: ExprType,
    pub definition: fn(
        Arc<Mutex<Context<T>>>,
        &[TracedExpr<u32>],
    ) -> Result<Expr<u32>, EvaluationError>,
    pub name: String,
}

pub type PolymorphicFunctionID = u32;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PolymorphicIndex {
    pub id: PolymorphicFunctionID,
    pub arg_types: Vec<ExprType>,
}

#[derive(Clone)]
pub struct ExpressionContext<T: Debugger + 'static> {
    pub symbol_table: BiMap<u32, String>,
    pub functions: HashMap<FunctionID, FunctionDefinition<T>>,
    pub polymorphic_functions: HashMap<PolymorphicIndex, FunctionID>,
    pub variables: HashMap<u32, TracedExpr<u32>>,
}

impl<T: Debugger + 'static> ExpressionContext<T> {
    /// Strip the symbols from the expression, adding any new symbols to
    /// the symbol table of the expression context.
    pub fn strip_symbols(&mut self, expr: Expr<String>) -> Expr<u32> {
        match expr {
            Expr::None => Expr::None,
            Expr::Unit => Expr::Unit,
            Expr::Int(x) => Expr::Int(x),
            Expr::String(x) => Expr::String(x),
            Expr::Float(x) => Expr::Float(x),
            Expr::Bool(x) => Expr::Bool(x),
            Expr::Type(x) => Expr::Type(x),
            Expr::Pair(x, y) => Expr::Pair(
                Box::new(self.strip_symbols_traced(*x)),
                Box::new(self.strip_symbols_traced(*y)),
            ),
            Expr::List(xs) => Expr::List(
                xs.into_iter()
                    .map(|x| self.strip_symbols_traced(x))
                    .collect(),
            ),
            Expr::Node(x) => Expr::Node(x),
            Expr::BuiltinFunction(x) => Expr::BuiltinFunction(x),
            Expr::PolymorphicFunction(x) => Expr::PolymorphicFunction(x),
            Expr::Lambda(vars, stmts, body) => {
                let stripped_vars: Vec<_> = vars
                    .into_iter()
                    .map(|var| match self.symbol_table.get_by_right(&var) {
                        Some(id) => *id,
                        None => {
                            let id = self.symbol_table.len() as u32;
                            self.symbol_table.insert(id, var);
                            id
                        }
                    })
                    .collect();

                Expr::Lambda(
                    stripped_vars,
                    stmts
                        .into_iter()
                        .map(|stmt| self.strip_symbols_statement(stmt))
                        .collect(),
                    Box::new(self.strip_symbols_traced(*body)),
                )
            }
            Expr::Apply(f, args) => Expr::Apply(
                Box::new(self.strip_symbols_traced(*f)),
                args.into_vec()
                    .into_iter()
                    .map(|a| self.strip_symbols_traced(a))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            Expr::Var(x) => {
                Expr::Var(match self.symbol_table.get_by_right(&x) {
                    Some(id) => *id,
                    None => {
                        let id = self.symbol_table.len() as u32;
                        self.symbol_table.insert(id, x);
                        id
                    }
                })
            }
        }
    }

    /// Strip the symbols from the traced expression, adding any new symbols to
    /// the symbol table of the expression context.
    pub fn strip_symbols_traced(
        &mut self,
        expr: TracedExpr<String>,
    ) -> TracedExpr<u32> {
        TracedExpr {
            evaluated: self.strip_symbols(expr.evaluated),
            stored_trace: expr.stored_trace.map(|t| self.strip_symbols(t)),
        }
    }

    /// Strip the symbols from the statement, adding any new symbols to
    /// the symbol table of the expression context.
    pub fn strip_symbols_statement(
        &mut self,
        statement: Statement<String>,
    ) -> Statement<u32> {
        Statement {
            location: statement.location,
            var: statement.var.map(|var| {
                match self.symbol_table.get_by_right(&var) {
                    Some(id) => *id,
                    None => {
                        let id = self.symbol_table.len() as u32;
                        self.symbol_table.insert(id, var);
                        id
                    }
                }
            }),
            expr: self.strip_symbols_raw(statement.expr),
        }
    }

    pub fn strip_symbols_raw(&mut self, expr: RawExpr<String>) -> RawExpr<u32> {
        match expr {
            RawExpr::None => RawExpr::None,
            RawExpr::Unit => RawExpr::Unit,
            RawExpr::Int(x) => RawExpr::Int(x),
            RawExpr::String(x) => RawExpr::String(x),
            RawExpr::Float(x) => RawExpr::Float(x),
            RawExpr::Bool(x) => RawExpr::Bool(x),
            RawExpr::Type(x) => RawExpr::Type(x),
            RawExpr::Pair(x, y) => RawExpr::Pair(
                Box::new(self.strip_symbols_raw(*x)),
                Box::new(self.strip_symbols_raw(*y)),
            ),
            RawExpr::List(xs) => RawExpr::List(
                xs.into_iter().map(|x| self.strip_symbols_raw(x)).collect(),
            ),
            RawExpr::Node(x) => RawExpr::Node(x),
            RawExpr::BuiltinFunction(x) => RawExpr::BuiltinFunction(x),
            RawExpr::PolymorphicFunction(x) => RawExpr::PolymorphicFunction(x),
            RawExpr::Lambda(vars, stmts, body) => {
                let stripped_vars: Vec<_> = vars
                    .into_iter()
                    .map(|var| match self.symbol_table.get_by_right(&var) {
                        Some(id) => *id,
                        None => {
                            let id = self.symbol_table.len() as u32;
                            self.symbol_table.insert(id, var);
                            id
                        }
                    })
                    .collect();

                RawExpr::Lambda(
                    stripped_vars,
                    stmts
                        .into_iter()
                        .map(|stmt| self.strip_symbols_statement(stmt))
                        .collect(),
                    Box::new(self.strip_symbols_raw(*body)),
                )
            }
            RawExpr::Apply(f, args) => RawExpr::Apply(
                Box::new(self.strip_symbols_raw(*f)),
                args.into_vec()
                    .into_iter()
                    .map(|a| self.strip_symbols_raw(a))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            RawExpr::Var(x) => {
                RawExpr::Var(match self.symbol_table.get_by_right(&x) {
                    Some(id) => *id,
                    None => {
                        let id = self.symbol_table.len() as u32;
                        self.symbol_table.insert(id, x);
                        id
                    }
                })
            }
        }
    }

    /// Restore the symbols to the expression by looking up variables
    /// in the symbol table.
    pub fn restore_symbols(&self, expr: Expr<u32>) -> Expr<String> {
        match expr {
            Expr::None => Expr::None,
            Expr::Unit => Expr::Unit,
            Expr::Int(x) => Expr::Int(x),
            Expr::String(x) => Expr::String(x),
            Expr::Float(x) => Expr::Float(x),
            Expr::Bool(x) => Expr::Bool(x),
            Expr::Type(x) => Expr::Type(x),
            Expr::Pair(x, y) => Expr::Pair(
                Box::new(self.restore_symbols_traced(*x)),
                Box::new(self.restore_symbols_traced(*y)),
            ),
            Expr::List(xs) => Expr::List(
                xs.into_iter()
                    .map(|x| self.restore_symbols_traced(x))
                    .collect(),
            ),
            Expr::Node(x) => Expr::Node(x),
            Expr::BuiltinFunction(x) => Expr::BuiltinFunction(x),
            Expr::PolymorphicFunction(x) => Expr::PolymorphicFunction(x),
            Expr::Lambda(vars, stmts, body) => {
                let restored_vars: Vec<_> = vars
                    .into_iter()
                    .map(|id| {
                        self.symbol_table
                            .get_by_left(&id)
                            .unwrap_or(&format!("var_{}", id))
                            .to_string()
                    })
                    .collect();

                Expr::Lambda(
                    restored_vars,
                    stmts
                        .into_iter()
                        .map(|stmt| self.restore_symbols_statement(stmt))
                        .collect(),
                    Box::new(self.restore_symbols_traced(*body)),
                )
            }
            Expr::Apply(f, args) => Expr::Apply(
                Box::new(self.restore_symbols_traced(*f)),
                args.into_vec()
                    .into_iter()
                    .map(|a| self.restore_symbols_traced(a))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            Expr::Var(id) => Expr::Var(
                self.symbol_table
                    .get_by_left(&id)
                    .unwrap_or(&format!("var_{}", id))
                    .to_string(),
            ),
        }
    }

    /// Restore the symbols to the traced expression by looking up variables
    /// in the symbol table.
    pub fn restore_symbols_traced(
        &self,
        expr: TracedExpr<u32>,
    ) -> TracedExpr<String> {
        TracedExpr {
            evaluated: self.restore_symbols(expr.evaluated),
            stored_trace: expr.stored_trace.map(|t| self.restore_symbols(t)),
        }
    }

    /// Restore the symbols to the statement by looking up variables
    /// in the symbol table.
    pub fn restore_symbols_statement(
        &self,
        statement: Statement<u32>,
    ) -> Statement<String> {
        Statement {
            location: statement.location,
            var: statement.var.map(|id| {
                self.symbol_table
                    .get_by_left(&id)
                    .unwrap_or(&format!("var_{}", id))
                    .to_string()
            }),
            expr: self.restore_symbols_raw(statement.expr),
        }
    }

    /// Restore the symbols to the raw expression by looking up variables
    /// in the symbol table.
    pub fn restore_symbols_raw(&self, expr: RawExpr<u32>) -> RawExpr<String> {
        match expr {
            RawExpr::None => RawExpr::None,
            RawExpr::Unit => RawExpr::Unit,
            RawExpr::Int(x) => RawExpr::Int(x),
            RawExpr::String(x) => RawExpr::String(x),
            RawExpr::Float(x) => RawExpr::Float(x),
            RawExpr::Bool(x) => RawExpr::Bool(x),
            RawExpr::Type(x) => RawExpr::Type(x),
            RawExpr::Pair(x, y) => RawExpr::Pair(
                Box::new(self.restore_symbols_raw(*x)),
                Box::new(self.restore_symbols_raw(*y)),
            ),
            RawExpr::List(xs) => RawExpr::List(
                xs.into_iter()
                    .map(|x| self.restore_symbols_raw(x))
                    .collect(),
            ),
            RawExpr::Node(x) => RawExpr::Node(x),
            RawExpr::BuiltinFunction(x) => RawExpr::BuiltinFunction(x),
            RawExpr::PolymorphicFunction(x) => RawExpr::PolymorphicFunction(x),
            RawExpr::Lambda(vars, stmts, body) => {
                let restored_vars: Vec<_> = vars
                    .into_iter()
                    .map(|id| {
                        self.symbol_table
                            .get_by_left(&id)
                            .unwrap_or(&format!("var_{}", id))
                            .to_string()
                    })
                    .collect();

                RawExpr::Lambda(
                    restored_vars,
                    stmts
                        .into_iter()
                        .map(|stmt| self.restore_symbols_statement(stmt))
                        .collect(),
                    Box::new(self.restore_symbols_raw(*body)),
                )
            }
            RawExpr::Apply(f, args) => RawExpr::Apply(
                Box::new(self.restore_symbols_raw(*f)),
                args.into_vec()
                    .into_iter()
                    .map(|a| self.restore_symbols_raw(a))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            RawExpr::Var(id) => RawExpr::Var(
                self.symbol_table
                    .get_by_left(&id)
                    .unwrap_or(&format!("var_{}", id))
                    .to_string(),
            ),
        }
    }
}

pub fn unwind_trace(expr: TracedExpr<u32>) -> TracedExpr<u32> {
    match expr {
        TracedExpr {
            evaluated,
            stored_trace,
        } => {
            let actual_trace = stored_trace.unwrap_or(evaluated);
            TracedExpr {
                evaluated: match actual_trace {
                    Expr::Apply(function, arguments) => Expr::Apply(
                        Box::new(unwind_trace(*function)),
                        arguments
                            .iter()
                            .map(|x| unwind_trace(x.to_owned()))
                            .collect(),
                    ),
                    _ => actual_trace,
                },
                stored_trace: None,
            }
        }
    }
}

/// Apply an expression to a list of arguments in a traced manner.
pub fn apply_traced<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    expr: TracedExpr<u32>,
    args: &[TracedExpr<u32>],
) -> Result<TracedExpr<u32>, EvaluationError> {
    let evaluated = evaluate::<T>(
        ctx.clone(),
        Expr::Apply(
            Box::new(expr.evaluated.clone().traced()),
            args.iter().map(|x| x.evaluated.clone().traced()).collect(),
        ),
    );

    // Before building the trace, expand variables in args
    let expanded_args: Vec<TracedExpr<u32>> = args
        .iter()
        .map(|arg| match &arg.evaluated {
            Expr::Var(var_name) => {
                if let Some(value) = ctx
                    .lock()
                    .unwrap()
                    .expression_context
                    .variables
                    .get(var_name)
                {
                    value.clone()
                } else {
                    arg.clone()
                }
            }
            _ => arg.clone(),
        })
        .collect();

    let trace = Expr::Apply(
        Box::new(expr.clone()),
        expanded_args
            .iter()
            .map(|x| x.get_trace().traced())
            .collect(),
    );

    Ok(TracedExpr {
        evaluated: evaluated?.evaluated,
        stored_trace: Some(trace),
    })
}

pub fn evaluate_traced<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    expr: TracedExpr<u32>,
) -> Result<TracedExpr<u32>, EvaluationError> {
    evaluate_traced_rec::<T>(ctx, expr.evaluated.clone())
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, Mutex};

    use bimap::BiMap;

    use crate::{
        ast::Expr, debugging::NoOpDebugger, frp::with_lock,
        interpreter::evaluate, stdlib::ef3r_stdlib,
    };

    quickcheck! {
        fn evaluation_is_idempotent(expr: Expr<String>) -> bool {
            let mut context = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

            println!("Evaluating: {}", &expr);

            let expr = context.expression_context.strip_symbols(expr);

            let context = Arc::new(Mutex::new(context));

            let evaluated = evaluate(context.clone(), expr);
            match evaluated {
                Err(_) => true, // Property does not apply if expression is malformed.
                Ok(inner) => Ok(inner.clone()) == evaluate(context, inner.evaluated),
            }
        }
    }
}

pub fn evaluate<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    expr: Expr<u32>,
) -> Result<TracedExpr<u32>, EvaluationError> {
    evaluate_traced_rec::<T>(ctx, expr)
}

fn evaluate_traced_rec<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    expr: Expr<u32>,
) -> Result<TracedExpr<u32>, EvaluationError> {
    match expr {
        // Literals evaluate to themselves.
        Expr::None => Ok(expr.traced()),
        Expr::Unit => Ok(expr.traced()),
        Expr::Int(_) => Ok(expr.traced()),
        Expr::Bool(_) => Ok(expr.traced()),
        Expr::String(_) => Ok(expr.traced()),
        Expr::Float(_) => Ok(expr.traced()),
        Expr::Pair(_, _) => Ok(expr.traced()),
        Expr::Type(_) => Ok(expr.traced()),
        Expr::Lambda(_, _, _) => Ok(expr.traced()),
        Expr::BuiltinFunction(_) => Ok(expr.traced()),
        Expr::PolymorphicFunction(_) => Ok(expr.traced()),
        Expr::Node(_) => Ok(expr.traced()),
        Expr::List(_) => Ok(expr.traced()),
        // Function applications need to be reduced.
        Expr::Apply(_, _) => {
            evaluate_function_application::<T>(ctx, &(expr.clone()))
        }
        // Variables are looked up in the current context.
        Expr::Var(x) => {
            let lock = ctx.lock().unwrap();

            let var_name = lock
                .expression_context
                .symbol_table
                .get_by_left(&x)
                .unwrap_or(&format!("var_{}", x))
                .to_string();

            Ok(lock
                .expression_context
                .variables
                .get(&x)
                .ok_or(EvaluationError::VariableNotFound(var_name))?
                .clone())
        }
    }
}

/// Entrypoint for the ef3r interpreter. Takes a list of a statements
///  and executes them.
pub fn interpret<T>(
    ctx: Arc<Mutex<Context<T>>>,
    statements: &[Statement<u32>],
) -> Result<(), EvaluationError>
where
    T: Debugger + 'static,
{
    for statement in statements {
        with_lock(ctx.as_ref(), |locked| {
            T::suspend(statement.location, locked)
        });

        let evaluated =
            evaluate::<T>(ctx.clone(), statement.expr.clone().from_raw())?;

        if let Some(var) = &statement.var {
            ctx.lock()
                .unwrap()
                .expression_context
                .variables
                .insert(var.clone(), evaluated);
        };
    }

    Ok(())
}

pub fn evaluate_function_application<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    action_expr: &Expr<u32>,
) -> Result<TracedExpr<u32>, EvaluationError> {
    match &action_expr {
        Expr::Apply(action, args) => {
            let evaluated_args: Result<Vec<TracedExpr<u32>>, _> = args
                .into_iter()
                .map(|arg| evaluate_traced::<T>(ctx.clone(), arg.clone()))
                .collect();

            let mut evaluated_args = evaluated_args?;

            let evaluated_fn =
                evaluate::<T>(ctx.clone(), action.clone().evaluated)?
                    .evaluated
                    .clone();

            let expanded_args = args
                .iter()
                .map(|arg| {
                    if let Expr::Var(var_name) = &arg.evaluated {
                        if let Some(value) = ctx
                            .lock()
                            .unwrap()
                            .expression_context
                            .variables
                            .get(var_name)
                        {
                            value.get_trace().traced()
                        } else {
                            arg.clone()
                        }
                    } else {
                        arg.clone()
                    }
                })
                .collect();

            let action_fn = function_from_expression::<T>(
                ctx.clone(),
                &evaluated_args,
                evaluated_fn,
            )?;

            Ok(TracedExpr::build(
                (action_fn)(ctx, &evaluated_args.as_mut_slice())?,
                Some(Expr::Apply(action.clone(), expanded_args)),
            ))
        }
        _ => {
            let reinterpreted = with_lock(ctx.as_ref(), |ctx| {
                ctx.expression_context.restore_symbols(action_expr.clone())
            });

            Err(EvaluationError::NotAFunction(reinterpreted))
        }
    }
}

fn function_from_expression<T: Debugger + 'static>(
    ctx: Arc<Mutex<Context<T>>>,
    evaluated_args: &Vec<TracedExpr<u32>>,
    resolved: Expr<u32>,
) -> Result<
    Box<
        dyn Fn(
            Arc<Mutex<Context<T>>>,
            &[TracedExpr<u32>],
        ) -> Result<Expr<u32>, EvaluationError>,
    >,
    EvaluationError,
> {
    return Ok(match &resolved {
        Expr::BuiltinFunction(action_id) => {
            let reinterpreted = with_lock(ctx.as_ref(), |ctx| {
                ctx.expression_context.restore_symbols(resolved.clone())
            });

            Box::new(
                ctx.lock()
                    .unwrap()
                    .expression_context
                    .functions
                    .get(action_id)
                    .ok_or(EvaluationError::NotAFunction(reinterpreted))?
                    .definition,
            )
        }
        Expr::PolymorphicFunction(polymorphic_id) => {
            let arg_types: Vec<_> = evaluated_args
                .iter()
                .map(|arg| {
                    with_lock(ctx.as_ref(), |lock| {
                        type_of(&lock.expression_context, &arg.evaluated)
                    })
                })
                .collect();

            let polymorphic_index = PolymorphicIndex {
                id: *polymorphic_id,
                arg_types: arg_types.clone().into_iter().flatten().collect(),
            };

            let ctx = ctx.lock().unwrap();

            // First, check to see if there are any polymorphic functions we
            // can resolve via multiple dispatch.
            let mut resolved_function_id = ctx
                .expression_context
                .polymorphic_functions
                .get(&polymorphic_index)
                .ok_or(EvaluationError::CouldNotResolvePolymorphicFunction {
                    id: *polymorphic_id,
                    arg_types: polymorphic_index.arg_types,
                });

            if let Err(_) = resolved_function_id {
                // Otherwise, try to resolve via single dispatch instead
                let first_arg = arg_types
                    .clone()
                    .into_iter()
                    .flatten()
                    .collect::<Vec<ExprType>>()
                    .first()
                    .unwrap()
                    .clone();

                resolved_function_id = ctx
                    .expression_context
                    .polymorphic_functions
                    .get(&PolymorphicIndex {
                        id: *polymorphic_id,
                        arg_types: vec![first_arg.clone()],
                    })
                    .ok_or(
                        EvaluationError::CouldNotResolvePolymorphicFunction {
                            id: *polymorphic_id,
                            arg_types: vec![first_arg],
                        },
                    );
            }

            let reinterpreted =
                ctx.expression_context.restore_symbols(resolved.clone());

            Box::new(
                ctx.expression_context
                    .functions
                    .get(resolved_function_id?)
                    .ok_or(EvaluationError::NotAFunction(reinterpreted))?
                    .definition,
            )
        }
        Expr::Lambda(vars, statements, result) => {
            let vars = vars.clone();
            let statements = statements.clone();
            let result = result.clone();
            Box::new(
                move |ctx: Arc<Mutex<Context<T>>>,
                      var_values: &[TracedExpr<u32>]| {
                    if vars.len() != var_values.len() {
                        return Err(EvaluationError::WrongNumberOfArguments {
                            expected: vars.len(),
                            actual: var_values.len(),
                            for_function: "[lambda]".to_string(),
                        });
                    }

                    // Substitute variables in statements with their corresponding values
                    let substituted_statements: Vec<Statement<u32>> =
                        statements
                            .iter()
                            .map(|statement| {
                                let substituted_expr =
                                    vars.iter().zip(var_values.iter()).fold(
                                        statement.expr.clone(),
                                        |acc, (var, var_value)| {
                                            substitute(
                                                var,
                                                &var_value
                                                    .evaluated
                                                    .clone()
                                                    .to_raw(),
                                                acc,
                                            )
                                        },
                                    );
                                Statement {
                                    location: statement.location,
                                    var: statement.var.clone(),
                                    expr: substituted_expr,
                                }
                            })
                            .collect();

                    // Run the substituted statements
                    interpret::<T>(ctx.clone(), &substituted_statements)?;

                    // Substitute variables in result expression
                    let substituted_result_expr =
                        vars.iter().zip(var_values.iter()).fold(
                            result.evaluated.clone(),
                            |acc, (var, var_value)| {
                                substitute(
                                    var,
                                    &var_value.evaluated.to_raw(),
                                    acc.to_raw(),
                                )
                                .from_raw()
                            },
                        );

                    // Run the result to get the return value
                    let result: Result<Expr<u32>, EvaluationError> =
                        evaluate::<T>(ctx, substituted_result_expr)
                            .map(|x| x.evaluated);

                    result
                },
            )
        }
        _ => {
            let reinterpreted = with_lock(ctx.as_ref(), |ctx| {
                ctx.expression_context.restore_symbols(resolved.clone())
            });
            return Err(EvaluationError::NotAFunction(reinterpreted));
        }
    });
}
