use bimap::BiMap;
use clap::{command, Parser, Subcommand};
use color_eyre::eyre::eyre;
use ef3r::ast::raw_expr::{RawExpr, RawExprRec};
use ef3r::debugging::{GrpcDebugger, NoOpDebugger, StepDebugger};
use ef3r::executable::{load_efrs_file, load_efrs_or_ef3r, Executable};
use ef3r::interpreter::{self};
use ef3r::node_visualization::node_visualizer_server::NodeVisualizerServer;
use ef3r::node_visualization::{node_visualization, NodeVisualizerState};
use parking_lot::RwLock;
use std::sync::Arc;
use std::{fs::File, io::Write};
use tonic::transport::Server;

#[derive(Parser)]
#[command(name = "ef3r")]
#[command(about = "ef3r interpreter and tools")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Execute an ef3r bytecode or source file")]
    Execute {
        #[arg(help = "Path to the ef3r bytecode or source file")]
        file: String,
    },
    #[command(about = "Debug an ef3r program")]
    Debug {
        #[arg(
            long,
            help = "Run the visual debugger alongside the running program"
        )]
        visual: bool,
        #[arg(
            long,
            default_value = Some("http://localhost:50051"),
            help = "Run the debugger in \"remote\" mode, to be attached to a running \n\
            ef3r program on another process (or potentially another machine)"
        )]
        remote: Option<String>,
        #[arg(
            help = "Path to the ef3r source or bytecode file. Not required for remote debugging."
        )]
        file: Option<String>,
    },
    #[command(about = "Compile ef3r source into bytecode")]
    Pack {
        #[arg(default_value = "--debug", help = "Optimizaton mode.")]
        mode: String,
        #[arg(help = "Path to the ef3r source file")]
        file: String,
        #[arg(help = "Output path")]
        out_path: Option<String>,
    },
    #[command(about = "Launch the language server")]
    LSP {},
}

#[tokio::main]
async fn main() -> color_eyre::eyre::Result<()> {
    color_eyre::install()?;

    use parking_lot::deadlock;
    use std::thread;
    use std::time::Duration;

    // Create a background thread which checks for deadlocks every 10s
    thread::spawn(move || loop {
        thread::sleep(Duration::from_secs(10));
        let deadlocks = deadlock::check_deadlock();
        if deadlocks.is_empty() {
            continue;
        }

        println!("{} deadlocks detected", deadlocks.len());
        for (i, threads) in deadlocks.iter().enumerate() {
            println!("Deadlock #{}", i);
            for t in threads {
                println!("Thread Id {:#?}", t.thread_id());
                println!("{:#?}", t.backtrace());
            }
        }
    });

    let cli = Cli::parse();

    match cli.command {
        Commands::Execute { file } => {
            let (context, program) =
                load_efrs_or_ef3r(NoOpDebugger::new(), file)?;

            interpreter::interpret(&context, &program)?;
        }
        Commands::Debug {
            visual,
            remote,
            file,
        } => {
            if visual && remote.is_some() {
                let debug_address = remote.unwrap();
                let state = start_visualizer_server(debug_address).await;
                start_visualizer(state);
            } else if visual {
                let state = start_visualizer_server(
                    "http://localhost:50051".to_string(),
                )
                .await;

                let debugger = GrpcDebugger::new().await;

                let (context, program) = load_efrs_or_ef3r(
                    debugger,
                    file.unwrap_or(Err(eyre!("File must be specified for a non-remote debugging session"))?)
                )?;

                tokio::spawn(async move {
                    interpreter::interpret(&context, &program).unwrap();
                });

                start_visualizer(state);
            } else {
                let (context, program) =
                    load_efrs_or_ef3r(
                        StepDebugger::new(),
                        file.unwrap_or(Err(eyre!("File must be specified for a non-remote debugging session"))?)
                    )?;

                let context_ref = Arc::new(RwLock::new(context));

                let context = context_ref.write();

                interpreter::interpret(&context, &program).unwrap();
            }
        }
        Commands::Pack {
            mode,
            file,
            out_path,
        } => {
            let (context, parsed_program) =
                load_efrs_file(NoOpDebugger::new(), &file)?;

            let default_out_path = file.replace(".efrs", ".ef3r");
            let out_path = out_path.unwrap_or(default_out_path);

            let executable = Executable {
                symbol_table: if mode == "--debug" {
                    context.expression_context.read().symbol_table.clone()
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
        Commands::LSP {} => {
            ef3r::lsp::start_lsp().await;
        }
    }

    Ok(())
}

async fn start_visualizer_server(
    debug_process_addr: String,
) -> NodeVisualizerState {
    let state = NodeVisualizerState {
        vertices: Arc::new(RwLock::new(vec![])),
        nodes_added: Arc::new(RwLock::new(0)),
    };

    let state_clone = state.clone();

    let debug_process_addr = Arc::new(debug_process_addr);

    tokio::spawn(async move {
        let addr = debug_process_addr.parse().unwrap();

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
    statement: ef3r::ast::Statement<usize>,
) -> ef3r::ast::Statement<usize> {
    ef3r::ast::Statement {
        location: None,
        var: statement.var,
        type_annotation: None,
        expr: strip_line_numbers_raw_expr(statement.expr),
    }
}

fn strip_line_numbers_raw_expr(expr: RawExpr<usize>) -> RawExpr<usize> {
    RawExpr {
        location: expr.location,
        expr: match expr.expr {
            RawExprRec::Lambda(vars, statements, body) => RawExprRec::Lambda(
                vars,
                statements.into_iter().map(strip_line_numbers).collect(),
                Box::new(strip_line_numbers_raw_expr(*body)),
            ),
            RawExprRec::Apply(func, args) => RawExprRec::Apply(
                Box::new(strip_line_numbers_raw_expr(*func)),
                args.into_vec()
                    .into_iter()
                    .map(strip_line_numbers_raw_expr)
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            ),
            RawExprRec::Pair(first, second) => RawExprRec::Pair(
                Box::new(strip_line_numbers_raw_expr(*first)),
                Box::new(strip_line_numbers_raw_expr(*second)),
            ),
            RawExprRec::List(elements) => RawExprRec::List(
                elements
                    .into_iter()
                    .map(strip_line_numbers_raw_expr)
                    .collect(),
            ),
            _ => expr.expr,
        },
    }
}
