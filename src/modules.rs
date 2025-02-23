// Builtin modules.

pub mod base;
pub mod bool;
pub mod io;
pub mod lists;
pub mod math;
pub mod reactive;
pub mod strings;
pub mod threading;

use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
};

use quickcheck::Arbitrary;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use typed_index_collections::TiVec;

use crate::{
    ast::expr::FunctionID,
    debugging::Debugger,
    interpreter::{Context, FunctionDefinition, PolymorphicIndex},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct QualifiedName {
    pub module: ModuleName,
    pub name: String,
}

impl QualifiedName {
    pub fn unqualified(name: String) -> QualifiedName {
        QualifiedName {
            module: ModuleName::new(""),
            name,
        }
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.module.0.is_empty() {
            f.write_str(&self.module.0)?;
            f.write_str("::")?;
        }
        f.write_str(&self.name)
    }
}

impl Arbitrary for QualifiedName {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        Self {
            module: ModuleName(String::arbitrary(g)),
            name: String::arbitrary(g),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ModuleName(String);

impl ModuleName {
    pub fn new(name: &str) -> ModuleName {
        ModuleName(name.to_string())
    }
}

pub struct Module<const N: usize, T: Debugger + 'static> {
    pub package: String,
    pub name: ModuleName,
    pub definitions: [FunctionDefinition<T>; N],
}

impl<const N: usize, T: Debugger> Module<N, T> {
    pub fn load_into(self, context: &Context<T>) {
        // Add the polymorphic index and function definitions from the module
        // to the context.
        context.expression_context.write().functions.extend(
            self.definitions.map(|definition| {
                (ModuleName::new(self.name.clone().0.as_str()), definition)
            }),
        );

        // Update the polymorphic functions map
        let new_index = build_polymorphic_index(
            &context.expression_context.read().functions,
        )
        .unwrap();

        // TODO: Should figure out an incremental way of doing this.
        context.expression_context.write().polymorphic_functions = new_index
    }
}

fn build_polymorphic_index<T: Debugger + 'static>(
    functions: &TiVec<FunctionID, (ModuleName, FunctionDefinition<T>)>,
) -> Result<HashMap<PolymorphicIndex, FunctionID>, String> {
    // Needs to be stable, otherwise the IDs will be nondeterministic.
    let mut name_buckets: BTreeMap<
        String,
        Vec<(FunctionID, &FunctionDefinition<T>)>,
    > = BTreeMap::new();

    for id in functions.keys() {
        let func = &functions[id];
        name_buckets
            .entry(func.1.name.clone())
            .or_default()
            .push((id, &func.1));
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

impl<const N: usize, T: Debugger> Module<N, T> {
    ///
    /// Get the names of all functions associated with a module.
    ///
    pub fn function_names(&self) -> HashSet<String> {
        self.definitions
            .iter()
            .map(|def| def.name.clone())
            .collect()
    }
}

#[derive(Clone, PartialEq)]
pub struct ModuleData {
    pub name: ModuleName,
    pub symbols: HashSet<String>,
}

///
/// Try find the first module which includes the given name, and
///  use that to return a qualified name with the appropriate
///  module resolved.
///
pub fn resolve_imports(
    modules: &Vec<ModuleData>,
    name: QualifiedName,
) -> QualifiedName {
    for module in modules {
        let found = module.symbols.contains(&name.name);

        if found {
            return QualifiedName {
                module: module.name.clone(),
                name: name.name.clone(),
            };
        }
    }

    // If no match found, return original name
    name
}
