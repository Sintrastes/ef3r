use bimap::BiMap;
use ef3r::debugging::{GrpcDebugger, NoOpDebugger, StepDebugger};
use ef3r::executable::{load_efrs_file, load_efrs_or_ef3r, Executable};
use ef3r::interpreter::{self};
use ef3r::node_visualization::node_visualizer_server::NodeVisualizerServer;
use ef3r::node_visualization::{node_visualization, NodeVisualizerState};
use std::sync::{Arc, Mutex, RwLock};
use std::{env, fs::File, io::Write};
use tonic::transport::Server;

const UNKNOWN_COMMAND: &str = "Unknown sub-command";

#[tokio::main]
async fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let args = Arc::new(args);

    let sub_command = args.get(1).ok_or(UNKNOWN_COMMAND)?.as_str();

    match sub_command {
        "execute" => {
            // Executes an ef3r bytecode file.
            let file_path =
                args.get(2).ok_or("File not specified")?.to_string();

            let (context, program) =
                load_efrs_or_ef3r(NoOpDebugger::new(), file_path)?;

            interpreter::interpret(Arc::new(Mutex::new(context)), &program)
                .unwrap();
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
            let file_path =
                args.get(2).ok_or("File not specified")?.to_string();

            let (context, program) =
                load_efrs_or_ef3r(StepDebugger::new(), file_path)?;

            interpreter::interpret(Arc::new(Mutex::new(context)), &program)
                .unwrap();
        }
        // Debug an ef3r program with a visual debugger.
        "debug-viz" => {
            let state = start_visualizer_server().await;

            let debugger = GrpcDebugger::new().await;

            let file_path =
                args.get(2).ok_or("File not specified")?.to_string();

            let (context, program) = load_efrs_or_ef3r(debugger, file_path)?;

            tokio::spawn(async move {
                interpreter::interpret(Arc::new(Mutex::new(context)), &program)
                    .unwrap();
            });

            start_visualizer(state);
        }
        "pack" => {
            // Parses ef3r source code and converts it into a ef3r bytecode file.

            let mode =
                args.get(2).unwrap_or(&"--debug".to_string()).to_string();

            let file_path =
                args.get(3).ok_or("Source file not specified")?.to_string();

            let default_out_path = &file_path.replace(".efrs", ".ef3r");

            let out_path = args.get(4).unwrap_or(default_out_path).as_str();

            let (context, parsed_program) =
                load_efrs_file(NoOpDebugger::new(), file_path)?;

            let executable = Executable {
                symbol_table: if mode == "--debug" {
                    context.expression_context.symbol_table
                } else {
                    BiMap::new()
                },
                instructions: if mode == "--debug" {
                    parsed_program
                } else {
                    parsed_program
                        .into_iter()
                        .map(|stmt| strip_line_numbers(stmt))
                        .collect()
                },
            };

            let mut out_file = File::create(out_path).unwrap();
            let encoded: Vec<u8> = bincode::serialize(&executable).unwrap();
            out_file.write(&encoded).unwrap();
        }
        _ => Err(UNKNOWN_COMMAND)?,
    }

    Ok(())
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

fn strip_line_numbers(
    statement: ef3r::ast::Statement<u32>,
) -> ef3r::ast::Statement<u32> {
    ef3r::ast::Statement {
        location: None,
        var: statement.var,
        expr: strip_line_numbers_raw_expr(statement.expr),
    }
}

fn strip_line_numbers_raw_expr(
    expr: ef3r::ast::RawExpr<u32>,
) -> ef3r::ast::RawExpr<u32> {
    match expr {
        ef3r::ast::RawExpr::Lambda(vars, statements, body) => {
            ef3r::ast::RawExpr::Lambda(
                vars,
                statements.into_iter().map(strip_line_numbers).collect(),
                Box::new(strip_line_numbers_raw_expr(*body)),
            )
        }
        ef3r::ast::RawExpr::Apply(func, args) => ef3r::ast::RawExpr::Apply(
            Box::new(strip_line_numbers_raw_expr(*func)),
            args.into_vec()
                .into_iter()
                .map(strip_line_numbers_raw_expr)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        ),
        ef3r::ast::RawExpr::Pair(first, second) => ef3r::ast::RawExpr::Pair(
            Box::new(strip_line_numbers_raw_expr(*first)),
            Box::new(strip_line_numbers_raw_expr(*second)),
        ),
        ef3r::ast::RawExpr::List(elements) => ef3r::ast::RawExpr::List(
            elements
                .into_iter()
                .map(strip_line_numbers_raw_expr)
                .collect(),
        ),
        _ => expr,
    }
}
