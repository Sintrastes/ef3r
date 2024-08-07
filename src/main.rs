use ef3r::ast::{Expr, Statement};
use ef3r::interpreter::{self};
use ef3r::stdlib::{ef3r_stdlib, PRINT_ID};
use std::{env, fs::File, io::Write};

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
            let mut context = ef3r_stdlib();

            // Example program
            let program = [
                Statement::Var("x".to_string(), Expr::Int(42).traced()),
                Statement::Var(
                    "y".to_string(),
                    Expr::String("Hello, world!".to_string()).traced(),
                ),
                Statement::Execute(
                    None,
                    Expr::Apply(
                        Box::new(Expr::Action(PRINT_ID).traced()),
                        Box::new([Expr::Var("y".to_string()).traced()]),
                    )
                    .traced(),
                ),
            ];

            interpreter::interpret(&mut context, &program);

            // Write the bytecode of the example to a file.

            let mut out_file = File::create("target/out.ef3r").unwrap();

            let encoded: Vec<u8> = bincode::serialize(&program).unwrap();

            out_file.write(&encoded).unwrap();
        }
        _ => Err(UNKNOWN_COMMAND)?,
    }

    Ok(())
}
