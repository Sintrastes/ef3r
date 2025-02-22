use rayon::prelude::*;

use crate::{
    debugging::Debugger,
    interpreter::Context,
    parser::parse,
    typechecking::{type_of, TypingContext},
};

///
/// Attempts to parse the expression and (if it is valid and
/// typechecks), returns a list of potential string completions
/// based on the types of the functions.
///
pub fn autocomplete<'a, T: Debugger + 'static>(
    context: &Context<T>,
    expression: &str,
) -> Vec<String> {
    let parsed = parse(expression);

    if parsed.is_err() {
        return vec![];
    }

    let (imports, statements) = parsed.unwrap();
    if statements.len() != 1 {
        return vec![];
    }

    let statement = statements[0].clone();

    let expr = context
        .expression_context
        .write()
        .strip_symbols_statement(statement)
        .expr;

    let expr_type = type_of(
        &context.expression_context.read(),
        &TypingContext::new(),
        &expr.from_raw().evaluated,
    );

    if expr_type.is_err() {
        return vec![];
    }

    let expr_type = expr_type.unwrap();

    context
        .expression_context
        .read()
        .functions
        // In most applications there will be a large amount of functions
        //  to sift through, so parallelize.
        .par_iter()
        .filter(|f| {
            if f.1.argument_types.is_empty() {
                return false;
            }

            f.1.argument_types[0] == expr_type
        })
        .map(|f| f.1.name.clone())
        .collect()
}
