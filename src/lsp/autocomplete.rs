use rayon::prelude::*;
use tower_lsp::lsp_types::Position;

use crate::{
    ast::{raw_expr::RawExpr, Statement},
    debugging::Debugger,
    interpreter::{Context, VariableId},
    parser::{parse, CodeLocation},
    typechecking::{type_of, TypingContext},
};

///
/// Attempts to get a list of autocompletions in the current source file,
///  given a parricular cursor location.
///
pub fn autocomplete_at<'a, T: Debugger + 'static>(
    context: &Context<T>,
    statements: &Vec<Statement<VariableId>>,
    cursor: Position,
) -> Vec<String> {
    // TODO: Traverse through the statements until arriving at the correct line,
    // then traverse through the statement to try to find the correct column.
    // In the future we may want to build an index to speed this process up.
    todo!()
}

///
/// Attempts to autocomplete on an expression (if it is valid and
/// typechecks), returns a list of potential string completions
/// based on the types of the functions.
///
pub fn autocomplete<'a, T: Debugger + 'static>(
    context: &Context<T>,
    expr: &RawExpr<VariableId>,
) -> Vec<String> {
    println!("Attempting to typecheck {:?}", &expr.from_raw().evaluated);

    let expr_type = type_of(
        &context.expression_context.read(),
        &TypingContext::new(),
        &expr.from_raw().evaluated,
    );

    if expr_type.is_err() {
        println!("Could not typecheck expression");
        return vec![];
    }

    let expr_type = expr_type.unwrap();

    println!("Got expr type: {}", &expr_type);

    context
        .expression_context
        .read()
        .functions
        // In most applications there will be a large amount of functions
        //  to sift through, so parallelize.
        .raw
        .par_iter()
        .filter(|f| {
            if f.1.argument_types.is_empty() {
                return false;
            }

            println!("Checking argument types: {}", &f.1.argument_types[0]);

            f.1.argument_types[0] == expr_type
        })
        .map(|f| f.1.name.clone())
        .collect()
}
