// Builtin modules.

pub mod base;
pub mod bool;
pub mod io;
pub mod lists;
pub mod math;
pub mod reactive;
pub mod strings;
pub mod threading;

use std::collections::{BTreeMap, HashMap};

use crate::{
    debugging::Debugger,
    interpreter::{Context, FunctionDefinition, PolymorphicIndex},
};

pub struct Module<const N: usize, T: Debugger + 'static> {
    pub package: String,
    pub file_name: String,
    pub definitions: [FunctionDefinition<T>; N],
}

impl<'a, const N: usize, T: Debugger> Module<N, T> {
    pub fn load_into(self, context: &mut Context<'a, T>) {
        // Add the polymorphic index and function definitions from the module
        // to the context.
        context
            .expression_context
            .functions
            .extend(self.definitions);

        // Update the polymorphic functions map
        let new_index =
            build_polymorphic_index(&context.expression_context.functions)
                .unwrap();

        // TODO: Should figure out an incremental way of doing this.
        context.expression_context.polymorphic_functions = new_index
    }
}

fn build_polymorphic_index<T: Debugger + 'static>(
    functions: &[FunctionDefinition<T>],
) -> Result<HashMap<PolymorphicIndex, usize>, String> {
    // Needs to be stable, otherwise the IDs will be nondeterministic.
    let mut name_buckets: BTreeMap<
        String,
        Vec<(usize, &FunctionDefinition<T>)>,
    > = BTreeMap::new();

    for id in 0..functions.len() - 1 {
        let func = &functions[id];
        name_buckets
            .entry(func.name.clone())
            .or_default()
            .push((id, func));
    }

    let mut polymorphic_id = 0;
    let mut result = HashMap::new();

    for (name, funcs) in name_buckets.into_iter() {
        // Skip functions with only 1 implementation
        if funcs.len() <= 1 {
            continue;
        }

        for (id, func) in funcs {
            let index = PolymorphicIndex {
                id: polymorphic_id,
                arg_types: func.argument_types.clone(),
            };

            if result.insert(index.clone(), id).is_some() {
                return Err(format!(
                    "Multiple functions named \"{}\" with signature {:?}",
                    name, index.arg_types
                ));
            }
        }

        polymorphic_id += 1;
    }

    Ok(result)
}
