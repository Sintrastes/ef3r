#[cfg(test)]
#[macro_use]
extern crate quickcheck;

pub mod ast;
pub mod debugging;
pub mod frp;
pub mod interpreter;
pub mod parser;
pub mod stdlib;
pub mod typechecking;
pub mod types;
