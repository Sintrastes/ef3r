use rayon::prelude::*;

use crate::{
    debugging::Debugger,
    interpreter::Context,
    parser::parse,
    typechecking::{type_of, NoLookup},
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

    let statements = parsed.unwrap();
    if statements.len() != 1 {
        return vec![];
    }

    let expr = statements[0].expr.clone();
    let expr_type = type_of::<_, _, NoLookup>(
        &context.expression_context.read(),
        &expr.from_raw().evaluated,
    );

    if expr_type.is_none() {
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
            if f.argument_types.is_empty() {
                return false;
            }

            f.argument_types[0] == expr_type
        })
        .map(|f| f.name.clone())
        .collect()
}
