use ef3r::ast::Statement;
use ef3r::debugging::{GrpcDebugger, NoOpDebugger, StepDebugger};
use ef3r::interpreter::{self};
use ef3r::node_visualization::node_visualizer_server::NodeVisualizerServer;
use ef3r::node_visualization::{node_visualization, NodeVisualizerState};
use ef3r::stdlib::{
    ef3r_stdlib, get_stdlib_functions, get_stdlib_polymorphic_functions,
};
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

            let program: Vec<Statement> = load_efrs_or_ef3r(file_path)?;

            let context =
                Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new())));

            interpreter::interpret(context, &program).unwrap();
        }
        // Launch the node visualizer that can be connected to a debugging
        // interpreter via gRPC.
        "standalone-viz" => {
            let state = start_visualizer_server().await;
            start_visualizer(state);
        }
        // Run a simple command-line debugger.
        "debug" => {
            // Executes an ef3r bytecode file.
            let file_path = args.get(2).ok_or("File not specified")?.as_str();

            let program: Vec<Statement> = load_efrs_or_ef3r(file_path)?;

            let context =
                Arc::new(Mutex::new(ef3r_stdlib(StepDebugger::new())));

            interpreter::interpret(context, &program).unwrap();
        }
        // Debug an ef3r program with a visual debugger.
        "debug-viz" => {
            let state = start_visualizer_server().await;

            let file_path = args.get(2).ok_or("File not specified")?.as_str();

            let program: Vec<Statement> = load_efrs_or_ef3r(file_path)?;

            let context =
                Arc::new(Mutex::new(ef3r_stdlib(GrpcDebugger::new().await)));

            tokio::spawn(async move {
                interpreter::interpret(context, &program).unwrap();
            });

            start_visualizer(state);
        }
        "pack" => {
            // Parses ef3r source code and converts it into a ef3r bytecode file.
            let file_path =
                args.get(2).ok_or("Source file not specified")?.as_str();

            let default_out_path = &file_path.replace(".efrs", ".ef3r");

            let out_path = args.get(3).unwrap_or(default_out_path).as_str();

            let parsed_program = load_efrs(file_path)?;

            let mut out_file = File::create(out_path).unwrap();
            let encoded: Vec<u8> = bincode::serialize(&parsed_program).unwrap();
            out_file.write(&encoded).unwrap();
        }
        _ => Err(UNKNOWN_COMMAND)?,
    }

    Ok(())
}

fn load_efrs_or_ef3r(file_path: &str) -> Result<Vec<Statement>, String> {
    if file_path.ends_with(".efrs") {
        load_efrs(file_path)
    } else {
        Ok(bincode::deserialize_from(File::open(file_path).unwrap()).unwrap())
    }
}

fn load_efrs(file_path: &str) -> Result<Vec<Statement>, String> {
    let source = std::fs::read_to_string(file_path).unwrap();
    let parsed_program = ef3r::parser::parse(&source)?;

    let stdlib = ef3r_stdlib(NoOpDebugger::new());
    let stdlib_functions = get_stdlib_functions(&stdlib);
    let polymorphic_functions = get_stdlib_polymorphic_functions(&stdlib);

    Ok(ef3r::stdlib::resolve_builtin_functions(
        parsed_program,
        &polymorphic_functions,
        &stdlib_functions,
    ))
}

async fn start_visualizer_server() -> NodeVisualizerState {
    let state = NodeVisualizerState {
        vertices: Arc::new(RwLock::new(vec![])),
        nodes_added: Arc::new(RwLock::new(0)),
    };

    let state_clone = state.clone();

    tokio::spawn(async {
        let addr = "[::1]:50051".parse().unwrap();

        Server::builder()
            .add_service(NodeVisualizerServer::new(state_clone))
            .serve(addr)
            .await
    });

    state
}

fn start_visualizer(state: NodeVisualizerState) {
    macroquad::Window::new("ef3r", node_visualization(state));
}
