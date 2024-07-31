use daggy::Dag;
use ef3r::ast::{Expr, Statement};
use ef3r::interpreter::{evaluate, Context};
use ef3r::stdlib::{ef3r_stdlib, MUL_ID, PRINT_ID};
use std::{collections::HashMap, env, fs::File, io::Write};

const UNKNOWN_COMMAND: &str = "Unknown sub-command";

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();

    let sub_command = args.get(1).ok_or(UNKNOWN_COMMAND)?.as_str();

    match sub_command {
        "execute" => {
            // Executes an ef3r bytecode file.
        }
        "pack" => {
            // Parses ef3r source code and converts it into a ef3r bytecode file.
        }
        "debug" => {
            // Debug a running ef3r process.
        }
        "example" => {
            // Runs a built-in example.
            let context = ef3r_stdlib();

            // Example expression.
            let expression = Expr::Apply(
                Box::new(Expr::BuiltinFunction(MUL_ID)),
                Box::new([Expr::Int(2), Expr::Int(3)]),
            );

            // Example program
            let program = [
                Statement::Var("x".to_string(), Expr::Int(42)),
                Statement::Execute(
                    None,
                    Expr::Apply(
                        Box::new(Expr::Action(PRINT_ID)),
                        Box::new([Expr::Var("x".to_string())]),
                    ),
                ),
            ];

            println!("Before evaluate: {:?}", expression);

            println!(
                "After evaluate: {:?}",
                evaluate(&context.expressionContext, expression)
            );

            // Write the bytecode of the example to a file.

            let mut out_file = File::create("target/out.ef3r").unwrap();

            let encoded: Vec<u8> = bincode::serialize(&program).unwrap();

            out_file.write(&encoded).unwrap();
        }
        _ => Err(UNKNOWN_COMMAND)?,
    }

    Ok(())
}
