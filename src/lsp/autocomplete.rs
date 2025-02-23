use rayon::prelude::*;
use tower_lsp::lsp_types::Position;

use crate::{
    ast::{
        raw_expr::{RawExpr, RawExprRec},
        Statement,
    },
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
    // Note: In the future we may want to build an index to speed this process up.

    // Do a binary search to try to find a matching line.
    let search_result = statements
        .binary_search_by(|x| x.location.unwrap().line.cmp(&cursor.line));

    match search_result {
        Ok(i) => {
            // If we found a matching line, we can just search the matching
            // statement.
            let found_line = &statements[i];

            let found_expr = find_expr_before(cursor, &found_line.expr);

            autocomplete(context, &found_expr)
        }
        Err(i) => {
            // Otherwise, we need to look at the line before where the
            //  match would have been inserted.

            let search_line = &statements[i - 1];

            // This should be a lambda that we can look into to find the actual line.
            match &search_line.expr.expr {
                RawExprRec::Lambda(_, lambda_statements, _) => {
                    autocomplete_at(context, &lambda_statements, cursor)
                }
                _ => panic!("Internal error: Autocompletion expected a lambda"),
            }
        }
    }
}

///
/// Traverses through an expression to find the expression immediately before the
///  cursor's location on the current line.
///
/// For example, if there was an expression at (2:31), and another at (2:33)
///  and the cursor was at (2:32), we'd return the expression at (2:31).
///
/// Note: Currently our line / column numbering data does not include the entire
///  "length" of the tokens, which makes it difficult to tell if the cursor
///  is actually "after" an expression. This will have to be fixed somehow
///  in the future.
///
fn find_expr_before(
    cursor: Position,
    expr: &RawExpr<VariableId>,
) -> RawExpr<VariableId> {
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
