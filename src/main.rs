use ef3r::ast::Statement;
use ef3r::debugging::{NoOpDebugger, StepDebugger};
use ef3r::interpreter::{self};
use ef3r::node_visualization::node_visualizer_server::NodeVisualizerServer;
use ef3r::node_visualization::{node_visualization, NodeVisualizerState};
use ef3r::stdlib::{ef3r_stdlib, get_stdlib_functions};
use std::sync::{Arc, Mutex, RwLock};
use std::{env, fs::File, io::Write};
use tonic::transport::Server;

const UNKNOWN_COMMAND: &str = "Unknown sub-command";

#[tokio::main]
async fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();

    let sub_command = args.get(1).ok_or(UNKNOWN_COMMAND)?.as_str();

    match sub_command {
        "execute" => {
            // Executes an ef3r bytecode file.
            let file_path = args.get(2).ok_or("File not specified")?.as_str();

            let program: Vec<Statement> =
                bincode::deserialize_from(File::open(file_path).unwrap())
                    .unwrap();

            let context =
                Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new())));

            interpreter::interpret(context, &program).unwrap();
        }
        "viz" => {
            let state = NodeVisualizerState {
                vertices: Arc::new(RwLock::new(vec![])),
            };

            let state_clone = state.clone();
            tokio::spawn(async {
                let addr = "[::1]:50051".parse().unwrap();

                Server::builder()
                    .add_service(NodeVisualizerServer::new(state_clone))
                    .serve(addr)
                    .await
            });

            macroquad::Window::new("ef3r", node_visualization(state));
        }
        "debug" => {
            // Executes an ef3r bytecode file.
            let file_path = args.get(2).ok_or("File not specified")?.as_str();

            let program: Vec<Statement> =
                bincode::deserialize_from(File::open(file_path).unwrap())
                    .unwrap();

            let context =
                Arc::new(Mutex::new(ef3r_stdlib(StepDebugger::new())));

            interpreter::interpret(context, &program).unwrap();
        }
        "pack" => {
            // Parses ef3r source code and converts it into a ef3r bytecode file.
            let file_path =
                args.get(2).ok_or("Source file not specified")?.as_str();
            let out_path =
                args.get(3).ok_or("Output file not specified")?.as_str();

            let source = std::fs::read_to_string(file_path).unwrap();

            let mut parsed_program = ef3r::parser::parse(&source)?;

            let stdlib = ef3r_stdlib(NoOpDebugger::new());
            let stdlib_functions = get_stdlib_functions(&stdlib);

            parsed_program = ef3r::stdlib::resolve_builtin_functions(
                parsed_program,
                &stdlib_functions,
            );

            let mut out_file = File::create(out_path).unwrap();
            let encoded: Vec<u8> = bincode::serialize(&parsed_program).unwrap();
            out_file.write(&encoded).unwrap();
        }
        _ => Err(UNKNOWN_COMMAND)?,
    }

    Ok(())
}
