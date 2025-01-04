use bimap::BiMap;

use crate::{
    debugging::Debugger,
    interpreter::Context,
    modules::{
        base::base_module, bool::bool_module, io::io_module,
        lists::lists_module, math::math_module, reactive::reactive_module,
        strings::strings_module, threading::threading_module,
    },
};

///
/// Load a context with all of the stdlib modules initialized,
///  with (optionally) a symbol table loaded to be used for
///  debugging purposes.
///
pub fn ef3r_stdlib<'a, T: Debugger + 'static>(
    debugger: T,
    symbol_table: BiMap<usize, String>,
) -> Context<'a, T> {
    // Lookup table for the interpreter
    let mut context = Context::init(debugger);

    context.load_module(base_module());
    context.load_module(bool_module());
    context.load_module(io_module());
    context.load_module(lists_module());
    context.load_module(math_module());
    context.load_module(reactive_module());
    context.load_module(strings_module());
    context.load_module(threading_module());

    context.expression_context.symbol_table = symbol_table;

    context
}
