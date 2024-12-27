#[cfg(test)]
#[macro_use]
extern crate quickcheck;

pub mod ast;
pub mod debugging;
pub mod executable;
pub mod extern_utils;
pub mod frp;
pub mod interpreter;
pub mod node_visualization;
pub mod parser;
pub mod stdlib;
pub mod typechecking;
pub mod types;
