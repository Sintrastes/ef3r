use ef3r::ast::{Expr, Statement};
use ef3r::interpreter::{self};
use ef3r::stdlib::{
    ef3r_stdlib, LAUNCH, NEW_NODE_ID, NODE_CURRENT_VALUE, PAIR_FIRST_ID,
    PAIR_SECOND_ID, PRINT_ID, READLN_ID, UPPERCASE_ID,
};
use ef3r::types::ExprType;
use std::sync::{Arc, Mutex};
use std::{env, fs::File, io::Write};

const UNKNOWN_COMMAND: &str = "Unknown sub-command";

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();

    let sub_command = args.get(1).ok_or(UNKNOWN_COMMAND)?.as_str();

    match sub_command {
        "execute" => {
            // Executes an ef3r bytecode file.
            let file_path = args.get(2).ok_or("File not specified")?.as_str();

            let program: Vec<Statement> =
                bincode::deserialize_from(File::open(file_path).unwrap())
                    .unwrap();

            let context = Arc::new(Mutex::new(ef3r_stdlib()));

            interpreter::interpret(context, &program);
        }
        "pack" => {
            // Parses ef3r source code and converts it into a ef3r bytecode file.
        }
        "debug" => {
            // Debug a running ef3r process.
        }
        "example" => {
            // Example program
            let program = vec![
                Statement::Var("x".to_string(), Expr::Int(42).traced()),
                Statement::Var(
                    "y".to_string(),
                    Expr::String("Hello, world!".to_string()).traced(),
                ),
                Statement::Execute(
                    None,
                    Expr::Apply(
                        Box::new(Expr::BuiltinFunction(PRINT_ID).traced()),
                        Box::new([Expr::Var("y".to_string()).traced()]),
                    )
                    .traced(),
                ),
                Statement::Execute(
                    Some("nodeAndSetNode".to_string()),
                    Expr::Apply(
                        Box::new(Expr::BuiltinFunction(NEW_NODE_ID).traced()),
                        Box::new([
                            Expr::Type(ExprType::Int).traced(),
                            Expr::Int(0).traced(),
                        ]),
                    )
                    .traced(),
                ),
                Statement::Execute(
                    None,
                    Expr::Apply(
                        Box::new(Expr::BuiltinFunction(PRINT_ID).traced()),
                        Box::new([
                            Expr::String("got node".to_string()).traced()
                        ]),
                    )
                    .traced(),
                ),
                Statement::Execute(
                    Some("node".to_string()),
                    Expr::Apply(
                        Box::new(Expr::BuiltinFunction(PAIR_FIRST_ID).traced()),
                        Box::new([
                            Expr::Var("nodeAndSetNode".to_string()).traced()
                        ]),
                    )
                    .traced(),
                ),
                Statement::Execute(
                    Some("set_node".to_string()),
                    Expr::Apply(
                        Box::new(
                            Expr::BuiltinFunction(PAIR_SECOND_ID).traced(),
                        ),
                        Box::new([
                            Expr::Var("nodeAndSetNode".to_string()).traced()
                        ]),
                    )
                    .traced(),
                ),
                Statement::Execute(
                    None,
                    Expr::Apply(
                        Box::new(Expr::BuiltinFunction(LAUNCH).traced()),
                        Box::new([Expr::Lambda(
                            vec![],
                            vec![],
                            Box::new(
                                Expr::Apply(
                                    Box::new(
                                        Expr::Var("set_node".to_string())
                                            .traced(),
                                    ),
                                    Box::new([
                                        Expr::Var("node".to_string()).traced(),
                                        Expr::Int(1).traced(),
                                    ]),
                                )
                                .traced(),
                            ),
                        )
                        .traced()]),
                    )
                    .traced(),
                ),
                Statement::Execute(
                    None,
                    Expr::Apply(
                        Box::new(Expr::BuiltinFunction(PRINT_ID).traced()),
                        Box::new([Expr::Apply(
                            Box::new(
                                Expr::BuiltinFunction(NODE_CURRENT_VALUE)
                                    .traced(),
                            ),
                            Box::new([Expr::Var("node".to_string()).traced()]),
                        )
                        .traced()]),
                    )
                    .traced(),
                ),
                Statement::Execute(
                    Some("z".to_string()),
                    Expr::Apply(
                        Box::new(Expr::BuiltinFunction(READLN_ID).traced()),
                        Box::new([]),
                    )
                    .traced(),
                ),
                Statement::Execute(
                    None,
                    Expr::Apply(
                        Box::new(Expr::BuiltinFunction(PRINT_ID).traced()),
                        Box::new([Expr::Apply(
                            Box::new(
                                Expr::BuiltinFunction(UPPERCASE_ID).traced(),
                            ),
                            Box::new([Expr::Var("z".to_string()).traced()]),
                        )
                        .traced()]),
                    )
                    .traced(),
                ),
            ];

            // Write the bytecode of the example to a file.

            let mut out_file = File::create("target/out.ef3r").unwrap();

            let encoded: Vec<u8> = bincode::serialize(&program).unwrap();

            out_file.write(&encoded).unwrap();
        }
        _ => Err(UNKNOWN_COMMAND)?,
    }

    Ok(())
}
