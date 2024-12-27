use std::fs::{read_to_string, File};

use bimap::BiMap;
use serde::{Deserialize, Serialize};

use crate::{
    ast::Statement,
    debugging::Debugger,
    interpreter::Context,
    parser::parse,
    stdlib::{
        ef3r_stdlib, get_stdlib_functions, get_stdlib_polymorphic_functions,
        resolve_builtin_functions,
    },
};

#[derive(Serialize, Deserialize)]
pub struct Executable {
    pub symbol_table: BiMap<u32, String>,
    pub instructions: Vec<Statement<u32>>,
}

pub fn load_efrs_or_ef3r<'a, T: Debugger + 'static>(
    debugger: T,
    file_path: String,
) -> Result<(Context<'a, T>, Vec<Statement<u32>>), String> {
    if file_path.ends_with(".efrs") {
        load_efrs_file(debugger, file_path)
    } else {
        let executable: Executable =
            bincode::deserialize_from(File::open(file_path).unwrap()).unwrap();

        Ok((
            ef3r_stdlib(debugger, executable.symbol_table),
            executable.instructions,
        ))
    }
}

pub fn load_efrs_file<'a, T: Debugger + 'static>(
    debugger: T,
    file_path: String,
) -> Result<(Context<'a, T>, Vec<Statement<u32>>), String> {
    let source = read_to_string(file_path).unwrap();
    load_efrs_source(debugger, source)
}

pub fn load_efrs_source<'a, T: Debugger + 'static>(
    debugger: T,
    source: String,
) -> Result<(Context<'a, T>, Vec<Statement<u32>>), String> {
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
