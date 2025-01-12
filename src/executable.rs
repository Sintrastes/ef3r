use std::{
    collections::HashMap,
    fs::{read_to_string, File},
};

use bimap::BiMap;
use color_eyre::eyre::Result;
use serde::{Deserialize, Serialize};

use crate::{
    ast::{
        raw_expr::{RawExpr, RawExprRec},
        Statement,
    },
    debugging::Debugger,
    interpreter::Context,
    parser::parse,
    stdlib::ef3r_stdlib,
};

#[derive(Serialize, Deserialize)]
pub struct Executable {
    pub symbol_table: BiMap<usize, String>,
    pub instructions: Vec<Statement<usize>>,
}

///
/// Load a efrs (source) executable file, or a ef3r
///  (binary packed format) executable file.
///
pub fn load_efrs_or_ef3r<'a, T: Debugger + Send + Sync + 'static>(
    debugger: T,
    file_path: String,
) -> Result<(Context<T>, Vec<Statement<usize>>)> {
    if file_path.ends_with(".efrs") {
        load_efrs_file(debugger, file_path.as_str())
    } else {
        let executable: Executable =
            bincode::deserialize_from(File::open(file_path).unwrap()).unwrap();

        Ok((
            ef3r_stdlib(debugger, executable.symbol_table),
            executable.instructions,
        ))
    }
}

///
/// Load an efrs executable file, returning a context
///  with all nescesary modules loaded, together with a
///  list of statements to execute in the executable.
///
pub fn load_efrs_file<T: Debugger + Send + Sync + 'static>(
    debugger: T,
    file_path: &str,
) -> Result<(Context<T>, Vec<Statement<usize>>)> {
    let source = read_to_string(file_path).unwrap();
    load_efrs_source(debugger, source)
}

pub fn load_efrs_source<T: Debugger + Send + Sync + 'static>(
    debugger: T,
    source: String,
) -> Result<(Context<T>, Vec<Statement<usize>>)> {
    let parsed_program = parse(&source)?;

    let mut stdlib = ef3r_stdlib(debugger, BiMap::new());

    let parsed_program = parsed_program
        .into_iter()
        .map(|stmt| stdlib.expression_context.strip_symbols_statement(stmt))
        .collect();

    let stdlib_functions = get_stdlib_functions(&stdlib);
    let polymorphic_functions = get_stdlib_polymorphic_functions(&stdlib);

    let instructions = resolve_builtin_functions(
        parsed_program,
        &polymorphic_functions,
        &stdlib_functions,
    );

    Ok((stdlib, instructions))
}

fn get_stdlib_functions<'a, T: Debugger + 'static>(
    stdlib: &'a Context<T>,
) -> HashMap<usize, usize> {
    stdlib
        .expression_context
        .functions
        .iter()
        .enumerate()
        .flat_map(|(id, invokable)| {
            let symbol_id = stdlib
                .expression_context
                .symbol_table
                .get_by_right(invokable.name.as_str())?;

            Some((*symbol_id, id))
        })
        .collect()
}

fn get_stdlib_polymorphic_functions<'a, T: Debugger + 'static>(
    stdlib: &'a Context<T>,
) -> HashMap<usize, usize> {
    stdlib
        .expression_context
        .polymorphic_functions
        .iter()
        .flat_map(|(id, func_id)| {
            let polymorhpic_fn_name =
                stdlib.expression_context.functions[*func_id].name.as_str();

            let symbol_id = stdlib
                .expression_context
                .symbol_table
                .get_by_right(polymorhpic_fn_name)?;

            Some((*symbol_id, id.id))
        })
        .collect()
}

///
/// Utility function to replace raw variable IDs for functions
///  with their corresponding function IDs.
///
fn resolve_builtin_functions(
    statements: Vec<Statement<usize>>,
    polymorphic_functions: &HashMap<usize, usize>,
    stdlib_functions: &HashMap<usize, usize>,
) -> Vec<Statement<usize>> {
    statements
        .into_iter()
        .map(|stmt| {
            resolve_functions_in_statement(
                stmt,
                polymorphic_functions,
                stdlib_functions,
            )
        })
        .collect()
}

fn resolve_functions_in_statement(
    stmt: Statement<usize>,
    polymorphic_functions: &HashMap<usize, usize>,
    stdlib_functions: &HashMap<usize, usize>,
) -> Statement<usize> {
    Statement {
        location: stmt.location,
        var: stmt.var,
        expr: resolve_functions_in_expr_raw(
            stmt.expr,
            polymorphic_functions,
            stdlib_functions,
        ),
    }
}

fn resolve_functions_in_expr_raw(
    expr: RawExpr<usize>,
    polymorphic_functions: &HashMap<usize, usize>,
    stdlib_functions: &HashMap<usize, usize>,
) -> RawExpr<usize> {
    RawExpr {
        location: expr.location,
        expr: match expr.expr {
            RawExprRec::Var(var_name) => {
                if let Some(poly_func_id) = polymorphic_functions.get(&var_name)
                {
                    RawExprRec::PolymorphicFunction(*poly_func_id)
                } else if let Some(func_id) = stdlib_functions.get(&var_name) {
                    RawExprRec::BuiltinFunction(*func_id)
                } else {
                    RawExprRec::Var(var_name)
                }
            }
            RawExprRec::Apply(func, args) => RawExprRec::Apply(
                Box::new(resolve_functions_in_expr_raw(
                    *func,
                    polymorphic_functions,
                    stdlib_functions,
                )),
                args.into_vec()
                    .into_iter()
                    .map(|a| {
                        resolve_functions_in_expr_raw(
                            a,
                            polymorphic_functions,
                            stdlib_functions,
                        )
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            RawExprRec::Pair(first, second) => RawExprRec::Pair(
                Box::new(resolve_functions_in_expr_raw(
                    *first,
                    polymorphic_functions,
                    stdlib_functions,
                )),
                Box::new(resolve_functions_in_expr_raw(
                    *second,
                    polymorphic_functions,
                    stdlib_functions,
                )),
            ),
            RawExprRec::Lambda(vars, stmts, body) => RawExprRec::Lambda(
                vars,
                resolve_builtin_functions(
                    stmts,
                    polymorphic_functions,
                    stdlib_functions,
                ),
                Box::new(resolve_functions_in_expr_raw(
                    *body,
                    polymorphic_functions,
                    stdlib_functions,
                )),
            ),
            _ => expr.expr,
        },
    }
}
