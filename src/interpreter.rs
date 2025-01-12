use std::{collections::HashMap, sync::Arc, thread::JoinHandle};

use bimap::{BiHashMap, BiMap};
use daggy::Dag;
use parking_lot::RwLock;
use thiserror::Error;

use crate::{
    ast::{
        expr::FunctionID,
        raw_expr::{substitute, RawExpr, RawExprRec},
        traced_expr::{TracedExpr, TracedExprRec},
        Statement,
    },
    debugging::Debugger,
    frp::{with_lock, Node},
    modules::Module,
    typechecking::{type_of, RuntimeLookup},
    types::ExprType,
};

///
/// Local state needed to evaluate expressions and
///  update the FRP event loop.
///
pub struct Context<T: Debugger + 'static> {
    pub expression_context: ExpressionContext<T>,
    pub graph: Dag<Node<T>, (), u32>,
    pub join_handles: Vec<JoinHandle<Result<(), EvaluationError>>>,
    pub debugger: T,
}

impl<'a, T: Debugger> Context<T> {
    pub fn load_module<const N: usize>(&mut self, module: Module<N, T>) {
        module.load_into(self);
    }

    ///
    /// Initialize a new context with no modules
    ///  (builtin or otherwise) loaded.
    ///
    pub fn init(debugger: T) -> Context<T> {
        Context {
            debugger,
            join_handles: vec![],
            graph: Dag::new(),
            expression_context: ExpressionContext {
                symbol_table: BiHashMap::new(),
                functions: vec![],
                polymorphic_functions: HashMap::new(),
                variables: HashMap::new(),
            },
        }
    }
}

unsafe impl<T: Debugger> Send for Context<T> {}

#[derive(Error, PartialEq, Debug)]
pub enum EvaluationError {
    #[error("Expected type is {expected}, but should have been {actual} (at {at_loc})")]
    TypeError {
        expected: ExprType,
        actual: ExprType,
        at_loc: String,
    },
    #[error("Attempted to apply {}, but was not a function.", _0.untraced())]
    NotAFunction(TracedExpr<String>),
    #[error("Thread panicked")]
    ThreadError,
    #[error("Could not find variable \"{variable}\" in the current context.")]
    VariableNotFound { variable: String },
    #[error("Function {for_function} takes {expected} arguments, but was passed {actual}.")]
    WrongNumberOfArguments {
        expected: usize,
        actual: usize,
        for_function: String,
    },
    #[error(
        "Error resolving polymorphic function {id} with arguments {:?}",
        arg_types
    )]
    CouldNotResolvePolymorphicFunction { id: usize, arg_types: Vec<ExprType> },
}

#[derive(Clone)]
pub struct FunctionDefinition<T: Debugger + 'static> {
    pub argument_types: Vec<ExprType>,
    pub result_type: ExprType,
    pub definition: fn(
        &mut Context<T>,
        Arc<RwLock<Context<T>>>,
        &[TracedExpr<usize>],
    ) -> Result<TracedExprRec<usize>, EvaluationError>,
    pub name: String,
}

pub type PolymorphicFunctionID = usize;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PolymorphicIndex {
    pub id: PolymorphicFunctionID,
    pub arg_types: Vec<ExprType>,
}

#[derive(Clone)]
pub struct ExpressionContext<T: Debugger + 'static> {
    pub symbol_table: BiMap<usize, String>,
    pub functions: Vec<FunctionDefinition<T>>,
    pub polymorphic_functions: HashMap<PolymorphicIndex, FunctionID>,
    pub variables: HashMap<usize, TracedExpr<usize>>,
}

impl<T: Debugger + 'static> ExpressionContext<T> {
    ///
    /// Resolve the function ID corresponding to the passed name.
    ///
    pub fn resolve_function(&self, name: &str) -> Option<usize> {
        for (index, function) in self.functions.iter().enumerate() {
            if function.name == name {
                return Some(index);
            }
        }

        None
    }

    ///
    /// Resolve the polymorphic function ID corresponding to the passed name.
    ///
    pub fn resolve_polymorphic_function(&self, name: &str) -> Option<usize> {
        let index = self
            .polymorphic_functions
            .iter()
            .find(|(_, func)| {
                let func_name = self.functions[**func].name.clone();
                func_name == name
            })
            .unwrap()
            .0
            .id;

        return Some(index);
    }

    /// Strip the symbols from the expression, adding any new symbols to
    /// the symbol table of the expression context.
    pub fn strip_symbols(
        &mut self,
        expr: TracedExprRec<String>,
    ) -> TracedExprRec<usize> {
        match expr {
            TracedExprRec::None => TracedExprRec::None,
            TracedExprRec::Unit => TracedExprRec::Unit,
            TracedExprRec::Int(x) => TracedExprRec::Int(x),
            TracedExprRec::String(x) => TracedExprRec::String(x),
            TracedExprRec::Float(x) => TracedExprRec::Float(x),
            TracedExprRec::Bool(x) => TracedExprRec::Bool(x),
            TracedExprRec::Type(x) => TracedExprRec::Type(x),
            TracedExprRec::JoinHandle(x) => TracedExprRec::JoinHandle(x),
            TracedExprRec::Pair(x, y) => TracedExprRec::Pair(
                Box::new(self.strip_symbols_traced(*x)),
                Box::new(self.strip_symbols_traced(*y)),
            ),
            TracedExprRec::List(xs) => TracedExprRec::List(
                xs.into_iter()
                    .map(|x| self.strip_symbols_traced(x))
                    .collect(),
            ),
            TracedExprRec::Node(x) => TracedExprRec::Node(x),
            TracedExprRec::BuiltinFunction(x) => {
                TracedExprRec::BuiltinFunction(x)
            }
            TracedExprRec::PolymorphicFunction(x) => {
                TracedExprRec::PolymorphicFunction(x)
            }
            TracedExprRec::Lambda(vars, stmts, body) => {
                let stripped_vars: Vec<_> = vars
                    .into_iter()
                    .map(|var| match self.symbol_table.get_by_right(&var) {
                        Some(id) => *id,
                        None => {
                            let id = self.symbol_table.len();
                            self.symbol_table.insert(id, var);
                            id
                        }
                    })
                    .collect();

                TracedExprRec::Lambda(
                    stripped_vars,
                    stmts
                        .into_iter()
                        .map(|stmt| self.strip_symbols_statement(stmt))
                        .collect(),
                    Box::new(self.strip_symbols_traced(*body)),
                )
            }
            TracedExprRec::Apply(f, args) => TracedExprRec::Apply(
                Box::new(self.strip_symbols_traced(*f)),
                args.into_vec()
                    .into_iter()
                    .map(|a| self.strip_symbols_traced(a))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            TracedExprRec::Var(x) => {
                TracedExprRec::Var(match self.symbol_table.get_by_right(&x) {
                    Some(id) => *id,
                    None => {
                        let id = self.symbol_table.len();
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
    ) -> TracedExpr<usize> {
        TracedExpr {
            location: expr.location,
            evaluated: self.strip_symbols(expr.evaluated),
            stored_trace: expr.stored_trace.map(|t| self.strip_symbols(t)),
        }
    }

    /// Strip the symbols from the statement, adding any new symbols to
    /// the symbol table of the expression context.
    pub fn strip_symbols_statement(
        &mut self,
        statement: Statement<String>,
    ) -> Statement<usize> {
        Statement {
            location: statement.location,
            var: statement.var.map(|var| {
                match self.symbol_table.get_by_right(&var) {
                    Some(id) => *id,
                    None => {
                        let id = self.symbol_table.len();
                        self.symbol_table.insert(id, var);
                        id
                    }
                }
            }),
            expr: self.strip_symbols_raw(statement.expr),
        }
    }

    pub fn strip_symbols_raw(
        &mut self,
        expr: RawExpr<String>,
    ) -> RawExpr<usize> {
        RawExpr {
            location: expr.location,
            expr: match expr.expr {
                RawExprRec::None => RawExprRec::None,
                RawExprRec::Unit => RawExprRec::Unit,
                RawExprRec::Int(x) => RawExprRec::Int(x),
                RawExprRec::String(x) => RawExprRec::String(x),
                RawExprRec::Float(x) => RawExprRec::Float(x),
                RawExprRec::Bool(x) => RawExprRec::Bool(x),
                RawExprRec::Type(x) => RawExprRec::Type(x),
                RawExprRec::Pair(x, y) => RawExprRec::Pair(
                    Box::new(self.strip_symbols_raw(*x)),
                    Box::new(self.strip_symbols_raw(*y)),
                ),
                RawExprRec::List(xs) => RawExprRec::List(
                    xs.into_iter().map(|x| self.strip_symbols_raw(x)).collect(),
                ),
                RawExprRec::JoinHandle(x) => RawExprRec::JoinHandle(x),
                RawExprRec::Node(x) => RawExprRec::Node(x),
                RawExprRec::BuiltinFunction(x) => {
                    RawExprRec::BuiltinFunction(x)
                }
                RawExprRec::PolymorphicFunction(x) => {
                    RawExprRec::PolymorphicFunction(x)
                }
                RawExprRec::Lambda(vars, stmts, body) => {
                    let stripped_vars: Vec<_> = vars
                        .into_iter()
                        .map(|var| match self.symbol_table.get_by_right(&var) {
                            Some(id) => *id,
                            None => {
                                let id = self.symbol_table.len();
                                self.symbol_table.insert(id, var);
                                id
                            }
                        })
                        .collect();

                    RawExprRec::Lambda(
                        stripped_vars,
                        stmts
                            .into_iter()
                            .map(|stmt| self.strip_symbols_statement(stmt))
                            .collect(),
                        Box::new(self.strip_symbols_raw(*body)),
                    )
                }
                RawExprRec::Apply(f, args) => RawExprRec::Apply(
                    Box::new(self.strip_symbols_raw(*f)),
                    args.into_vec()
                        .into_iter()
                        .map(|a| self.strip_symbols_raw(a))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                ),
                RawExprRec::Var(x) => {
                    RawExprRec::Var(match self.symbol_table.get_by_right(&x) {
                        Some(id) => *id,
                        None => {
                            let id = self.symbol_table.len();
                            self.symbol_table.insert(id, x);
                            id
                        }
                    })
                }
            },
        }
    }

    /// Restore the symbols to the expression by looking up variables
    /// in the symbol table.
    pub fn restore_symbols(
        &self,
        expr: TracedExprRec<usize>,
    ) -> TracedExprRec<String> {
        match expr {
            TracedExprRec::None => TracedExprRec::None,
            TracedExprRec::Unit => TracedExprRec::Unit,
            TracedExprRec::Int(x) => TracedExprRec::Int(x),
            TracedExprRec::String(x) => TracedExprRec::String(x),
            TracedExprRec::Float(x) => TracedExprRec::Float(x),
            TracedExprRec::Bool(x) => TracedExprRec::Bool(x),
            TracedExprRec::Type(x) => TracedExprRec::Type(x),
            TracedExprRec::Pair(x, y) => TracedExprRec::Pair(
                Box::new(self.restore_symbols_traced(*x)),
                Box::new(self.restore_symbols_traced(*y)),
            ),
            TracedExprRec::List(xs) => TracedExprRec::List(
                xs.into_iter()
                    .map(|x| self.restore_symbols_traced(x))
                    .collect(),
            ),
            TracedExprRec::JoinHandle(x) => TracedExprRec::JoinHandle(x),
            TracedExprRec::Node(x) => TracedExprRec::Node(x),
            TracedExprRec::BuiltinFunction(x) => {
                TracedExprRec::BuiltinFunction(x)
            }
            TracedExprRec::PolymorphicFunction(x) => {
                TracedExprRec::PolymorphicFunction(x)
            }
            TracedExprRec::Lambda(vars, stmts, body) => {
                let restored_vars: Vec<_> = vars
                    .into_iter()
                    .map(|id| {
                        self.symbol_table
                            .get_by_left(&id)
                            .unwrap_or(&format!("var_{}", id))
                            .to_string()
                    })
                    .collect();

                TracedExprRec::Lambda(
                    restored_vars,
                    stmts
                        .into_iter()
                        .map(|stmt| self.restore_symbols_statement(stmt))
                        .collect(),
                    Box::new(self.restore_symbols_traced(*body)),
                )
            }
            TracedExprRec::Apply(f, args) => TracedExprRec::Apply(
                Box::new(self.restore_symbols_traced(*f)),
                args.into_vec()
                    .into_iter()
                    .map(|a| self.restore_symbols_traced(a))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            TracedExprRec::Var(id) => TracedExprRec::Var(
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
        expr: TracedExpr<usize>,
    ) -> TracedExpr<String> {
        TracedExpr {
            location: expr.location,
            evaluated: self.restore_symbols(expr.evaluated),
            stored_trace: expr.stored_trace.map(|t| self.restore_symbols(t)),
        }
    }

    /// Restore the symbols to the statement by looking up variables
    /// in the symbol table.
    pub fn restore_symbols_statement(
        &self,
        statement: Statement<usize>,
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
    pub fn restore_symbols_raw(&self, expr: RawExpr<usize>) -> RawExpr<String> {
        RawExpr {
            location: expr.location,
            expr: match expr.expr {
                RawExprRec::None => RawExprRec::None,
                RawExprRec::Unit => RawExprRec::Unit,
                RawExprRec::Int(x) => RawExprRec::Int(x),
                RawExprRec::String(x) => RawExprRec::String(x),
                RawExprRec::Float(x) => RawExprRec::Float(x),
                RawExprRec::Bool(x) => RawExprRec::Bool(x),
                RawExprRec::Type(x) => RawExprRec::Type(x),
                RawExprRec::Pair(x, y) => RawExprRec::Pair(
                    Box::new(self.restore_symbols_raw(*x)),
                    Box::new(self.restore_symbols_raw(*y)),
                ),
                RawExprRec::List(xs) => RawExprRec::List(
                    xs.into_iter()
                        .map(|x| self.restore_symbols_raw(x))
                        .collect(),
                ),
                RawExprRec::JoinHandle(x) => RawExprRec::JoinHandle(x),
                RawExprRec::Node(x) => RawExprRec::Node(x),
                RawExprRec::BuiltinFunction(x) => {
                    RawExprRec::BuiltinFunction(x)
                }
                RawExprRec::PolymorphicFunction(x) => {
                    RawExprRec::PolymorphicFunction(x)
                }
                RawExprRec::Lambda(vars, stmts, body) => {
                    let restored_vars: Vec<_> = vars
                        .into_iter()
                        .map(|id| {
                            self.symbol_table
                                .get_by_left(&id)
                                .unwrap_or(&format!("var_{}", id))
                                .to_string()
                        })
                        .collect();

                    RawExprRec::Lambda(
                        restored_vars,
                        stmts
                            .into_iter()
                            .map(|stmt| self.restore_symbols_statement(stmt))
                            .collect(),
                        Box::new(self.restore_symbols_raw(*body)),
                    )
                }
                RawExprRec::Apply(f, args) => RawExprRec::Apply(
                    Box::new(self.restore_symbols_raw(*f)),
                    args.into_vec()
                        .into_iter()
                        .map(|a| self.restore_symbols_raw(a))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                ),
                RawExprRec::Var(id) => RawExprRec::Var(
                    self.symbol_table
                        .get_by_left(&id)
                        .unwrap_or(&format!("var_{}", id))
                        .to_string(),
                ),
            },
        }
    }
}

#[cfg(test)]
mod tests_for_symbols {
    use bimap::BiMap;

    use crate::{
        ast::traced_expr::TracedExprRec, debugging::NoOpDebugger,
        stdlib::ef3r_stdlib,
    };

    quickcheck! {
        fn strip_and_restore_yields_same_expression(expr: TracedExprRec<String>) -> bool {
            let mut context = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

            let stripped = context.expression_context.strip_symbols(expr.clone());
            let restored = context.expression_context.restore_symbols(stripped);

            restored == expr
        }
    }
}

/// Apply an expression to a list of arguments in a traced manner.
pub fn apply_traced<T: Debugger + 'static>(
    ctx: &mut Context<T>,
    ctx_ref: Arc<RwLock<Context<T>>>,
    expr: TracedExpr<usize>,
    args: &[TracedExpr<usize>],
) -> Result<TracedExpr<usize>, EvaluationError> {
    let evaluated = evaluate::<T>(
        ctx,
        ctx_ref.clone(),
        TracedExprRec::Apply(
            Box::new(expr.evaluated.clone().traced()),
            args.iter().map(|x| x.evaluated.clone().traced()).collect(),
        ),
    );

    // Before building the trace, expand variables in args
    let expanded_args: Vec<TracedExpr<usize>> = args
        .iter()
        .map(|arg| match &arg.evaluated {
            TracedExprRec::Var(var_name) => {
                if let Some(value) =
                    ctx.expression_context.variables.get(var_name)
                {
                    value.clone()
                } else {
                    arg.clone()
                }
            }
            _ => arg.clone(),
        })
        .collect();

    let trace = TracedExprRec::Apply(
        Box::new(expr.clone()),
        expanded_args.iter().map(|x| x.get_trace()).collect(),
    );

    Ok(TracedExpr {
        location: expr.location,
        evaluated: evaluated?.evaluated,
        stored_trace: Some(trace),
    })
}

pub fn evaluate_traced<T: Debugger + 'static>(
    ctx: &mut Context<T>,
    ctx_ref: Arc<RwLock<Context<T>>>,
    expr: TracedExpr<usize>,
) -> Result<TracedExpr<usize>, EvaluationError> {
    evaluate_traced_rec::<T>(ctx, ctx_ref, expr.evaluated.clone())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use bimap::BiMap;

    use parking_lot::RwLock;

    use crate::{
        ast::traced_expr::TracedExprRec, debugging::NoOpDebugger,
        interpreter::evaluate, stdlib::ef3r_stdlib,
    };

    quickcheck! {
        fn evaluation_is_idempotent(expr: TracedExprRec<String>) -> bool {
            let mut context = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

            let expr = context.expression_context.strip_symbols(expr);

            let context_ref = Arc::new(RwLock::new(context));

            let context_ref_clone = context_ref.clone();

            let mut context = context_ref_clone.write();

            let evaluated = evaluate(&mut context, context_ref.clone(), expr);
            match evaluated {
                Err(_) => true, // Property does not apply if expression is malformed.
                Ok(inner) => Ok(inner.clone()) == evaluate(&mut context, context_ref, inner.evaluated),
            }
        }
    }
}

pub fn evaluate<T: Debugger + 'static>(
    ctx: &mut Context<T>,
    ctx_ref: Arc<RwLock<Context<T>>>,
    expr: TracedExprRec<usize>,
) -> Result<TracedExpr<usize>, EvaluationError> {
    evaluate_traced_rec::<T>(ctx, ctx_ref, expr)
}

fn evaluate_traced_rec<T: Debugger + 'static>(
    ctx: &mut Context<T>,
    ctx_ref: Arc<RwLock<Context<T>>>,
    expr: TracedExprRec<usize>,
) -> Result<TracedExpr<usize>, EvaluationError> {
    match expr {
        // Literals evaluate to themselves.
        TracedExprRec::None => Ok(expr.traced()),
        TracedExprRec::Unit => Ok(expr.traced()),
        TracedExprRec::Int(_) => Ok(expr.traced()),
        TracedExprRec::Bool(_) => Ok(expr.traced()),
        TracedExprRec::String(_) => Ok(expr.traced()),
        TracedExprRec::Float(_) => Ok(expr.traced()),
        TracedExprRec::Pair(_, _) => Ok(expr.traced()),
        TracedExprRec::Type(_) => Ok(expr.traced()),
        TracedExprRec::Lambda(_, _, _) => Ok(expr.traced()),
        TracedExprRec::BuiltinFunction(_) => Ok(expr.traced()),
        TracedExprRec::PolymorphicFunction(_) => Ok(expr.traced()),
        TracedExprRec::JoinHandle(_) => Ok(expr.traced()),
        TracedExprRec::Node(_) => Ok(expr.traced()),
        TracedExprRec::List(_) => Ok(expr.traced()),
        // Function applications need to be reduced.
        TracedExprRec::Apply(_, _) => {
            evaluate_function_application::<T>(ctx, ctx_ref, &(expr.clone()))
        }
        // Variables are looked up in the current context.
        TracedExprRec::Var(x) => {
            let var_name = ctx
                .expression_context
                .symbol_table
                .get_by_left(&x)
                .unwrap_or(&format!("var_{}", x))
                .to_string();

            Ok(ctx
                .expression_context
                .variables
                .get(&x)
                .ok_or(EvaluationError::VariableNotFound {
                    variable: var_name,
                })?
                .clone())
        }
    }
}

/// Entrypoint for the ef3r interpreter. Takes a list of a statements
///  and executes them.
pub fn interpret<T>(
    ctx: &mut Context<T>,
    ctx_ref: Arc<RwLock<Context<T>>>,
    statements: &[Statement<usize>],
) -> Result<(), EvaluationError>
where
    T: Debugger + 'static,
{
    for statement in statements {
        T::suspend(statement.location, ctx);

        let evaluated = evaluate_traced::<T>(
            ctx,
            ctx_ref.clone(),
            statement.expr.from_raw(),
        )?;

        if let Some(var) = &statement.var {
            ctx.expression_context
                .variables
                .insert(var.clone(), evaluated);
        };
    }

    Ok(())
}

pub fn evaluate_function_application<T: Debugger + 'static>(
    ctx: &mut Context<T>,
    ctx_ref: Arc<RwLock<Context<T>>>,
    action_expr: &TracedExprRec<usize>,
) -> Result<TracedExpr<usize>, EvaluationError> {
    match &action_expr {
        TracedExprRec::Apply(action, args) => {
            let evaluated_args: Result<Vec<TracedExpr<usize>>, _> = args
                .into_iter()
                .map(|arg| {
                    evaluate_traced::<T>(ctx, ctx_ref.clone(), arg.clone())
                })
                .collect();

            let mut evaluated_args = evaluated_args?;

            let evaluated_fn =
                evaluate::<T>(ctx, ctx_ref.clone(), action.clone().evaluated)?
                    .evaluated
                    .clone();

            let expanded_args: Vec<TracedExpr<usize>> = args
                .into_iter()
                .map(|arg| {
                    if let TracedExprRec::Var(var_name) = &arg.evaluated {
                        if let Some(value) =
                            ctx.expression_context.variables.get(var_name)
                        {
                            value.clone()
                        } else {
                            arg.clone()
                        }
                    } else {
                        arg.clone()
                    }
                })
                .collect();

            let action_fn = function_from_expression::<T>(
                ctx,
                ctx_ref.clone(),
                &evaluated_args,
                evaluated_fn,
            )?;

            Ok(TracedExpr::build(
                (action_fn)(
                    ctx,
                    ctx_ref.clone(),
                    &evaluated_args.as_mut_slice(),
                )?,
                Some(TracedExprRec::Apply(
                    action.clone(),
                    expanded_args.into(),
                )),
            ))
        }
        _ => {
            let reinterpreted =
                ctx.expression_context.restore_symbols(action_expr.clone());

            Err(EvaluationError::NotAFunction(reinterpreted.traced()))
        }
    }
}

fn function_from_expression<T: Debugger + 'static>(
    ctx: &mut Context<T>,
    ctx_ref: Arc<RwLock<Context<T>>>,
    evaluated_args: &Vec<TracedExpr<usize>>,
    resolved: TracedExprRec<usize>,
) -> Result<
    Box<
        dyn Fn(
            &mut Context<T>,
            Arc<RwLock<Context<T>>>,
            &[TracedExpr<usize>],
        ) -> Result<TracedExprRec<usize>, EvaluationError>,
    >,
    EvaluationError,
> {
    return Ok(match &resolved {
        TracedExprRec::BuiltinFunction(action_id) => {
            let reinterpreted =
                ctx.expression_context.restore_symbols(resolved.clone());

            Box::new(
                ctx.expression_context
                    .functions
                    .get(*action_id)
                    .ok_or(EvaluationError::NotAFunction(
                        reinterpreted.traced(),
                    ))?
                    .definition,
            )
        }
        TracedExprRec::PolymorphicFunction(polymorphic_id) => {
            let arg_types: Vec<_> = evaluated_args
                .iter()
                .map(|arg| {
                    type_of::<_, _, RuntimeLookup>(
                        &ctx.expression_context,
                        &arg.evaluated,
                    )
                })
                .collect();

            let polymorphic_index = PolymorphicIndex {
                id: *polymorphic_id,
                arg_types: arg_types.clone().into_iter().flatten().collect(),
            };

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
                    .get(*resolved_function_id?)
                    .ok_or(EvaluationError::NotAFunction(
                        reinterpreted.traced(),
                    ))?
                    .definition,
            )
        }
        TracedExprRec::Lambda(vars, statements, result) => {
            let vars = vars.clone();
            let statements = statements.clone();
            let result = result.clone();
            Box::new(
                move |ctx,
                      ctx_ref: Arc<RwLock<Context<T>>>,
                      var_values: &[TracedExpr<usize>]| {
                    if vars.len() != var_values.len() {
                        return Err(EvaluationError::WrongNumberOfArguments {
                            expected: vars.len(),
                            actual: var_values.len(),
                            for_function: "[lambda]".to_string(),
                        });
                    }

                    // Substitute variables in statements with their corresponding values
                    let substituted_statements: Vec<Statement<usize>> =
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
                                                    .untraced()
                                                    .as_expr(),
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
                    interpret::<T>(
                        ctx,
                        ctx_ref.clone(),
                        &substituted_statements,
                    )?;

                    // Substitute variables in result expression
                    let substituted_result_expr = vars
                        .iter()
                        .zip(var_values.iter())
                        .fold(
                            result.untraced().clone(),
                            |acc, (var, var_value)| {
                                substitute(var, &var_value.untraced(), acc)
                            },
                        )
                        .from_raw();

                    // Run the result to get the return value
                    let result: Result<TracedExprRec<usize>, EvaluationError> =
                        evaluate_traced::<T>(
                            ctx,
                            ctx_ref,
                            substituted_result_expr,
                        )
                        .map(|x| x.evaluated);

                    result
                },
            )
        }
        _ => {
            let reinterpreted =
                ctx.expression_context.restore_symbols(resolved.clone());
            return Err(EvaluationError::NotAFunction(reinterpreted.traced()));
        }
    });
}
