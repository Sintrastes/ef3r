use std::collections::HashSet;

use tonic::transport::Channel;

use crate::{
    frp::Node,
    interpreter::Context,
    node_visualization::{
        node_visualizer_client::NodeVisualizerClient, AddNodesRequest, NodeData,
    },
    parser::CodeLocation,
};

///
/// Interface for an ef3r debugger.
///
pub trait Debugger: Sized {
    /// Suspend execution of the interpreter and
    ///  inject the debugging environment.
    fn suspend(location: CodeLocation, ctx: &mut Context<Self>);

    /// Handle for the debugger to perform an action when
    /// a node has been added.
    fn on_node_added(&self, node: &Node<Self>, node_id: usize);

    /// Handle for the debugger to perform an action when
    /// a node has been removed.
    fn on_node_removed(&self, node_id: usize);
}

/// A debugger that does nothing, allowing
///  the interpreter to execute as normal.
pub struct NoOpDebugger {}

impl NoOpDebugger {
    pub fn new() -> Self {
        NoOpDebugger {}
    }
}

impl Debugger for NoOpDebugger {
    fn suspend(_location: CodeLocation, _ctx: &mut Context<Self>) {}

    fn on_node_added(&self, _node: &Node<Self>, _node_id: usize) {}

    fn on_node_removed(&self, _node_id: usize) {}
}

/// A simple "stepping" debugger that allows execution
///  to occur one step at a time and allows the user to
///  enter simple commands manipulating the environment.
pub struct StepDebugger {
    breakpoints: HashSet<(usize, u32)>,
    initial_suspend: bool,
}

impl StepDebugger {
    pub fn new() -> StepDebugger {
        StepDebugger {
            breakpoints: HashSet::new(),
            // Suspend initially to give the user a chance to set breakpoints.
            initial_suspend: true,
        }
    }
}

impl Debugger for StepDebugger {
    fn on_node_added(&self, _node: &Node<Self>, _node_id: usize) {}

    fn on_node_removed(&self, _node_id: usize) {}

    fn suspend(location: CodeLocation, ctx: &mut Context<Self>) {
        if ctx.debugger.initial_suspend
            || ctx
                .debugger
                .breakpoints
                .contains(&(location.column, location.line))
        {
            ctx.debugger.initial_suspend = false;

            println!("PAUSED AT (line {}, column {}): Type enter to continue, :break [line] [col] to set breakpoints, and :show [var] to examine the value of variables.", location.line, location.column);
            loop {
                let mut input = String::new();
                std::io::stdin().read_line(&mut input).unwrap();
                let input = input.trim();

                if input == "" {
                    break;
                } else if input.starts_with(":show ") {
                    let var_name = input.trim_start_matches(":show ").trim();
                    match ctx.expression_context.variables.get(var_name) {
                        Some(val) => println!("{} = {:?}", var_name, val),
                        None => {
                            println!(
                                "DEBUGGER: Variable '{}' not found",
                                var_name
                            )
                        }
                    }
                } else if input.starts_with(":break ") {
                    let mut parts =
                        input.trim_start_matches(":break ").split_whitespace();
                    if let (Some(line), Some(col)) =
                        (parts.next(), parts.next())
                    {
                        if let (Ok(line), Ok(col)) =
                            (line.parse::<u32>(), col.parse::<usize>())
                        {
                            ctx.debugger.breakpoints.insert((col, line));
                            println!(
                                "Added breakpoint at line {}, column {}",
                                line, col
                            );
                        } else {
                            println!("DEBUGGER: Invalid line/column numbers");
                        }
                    } else {
                        println!("DEBUGGER: Expected ':break [line] [col]'");
                    }
                } else {
                    println!("DEBUGGER: Unknown command");
                }
            }
        }
    }
}

pub struct GrpcDebugger {
    client: NodeVisualizerClient<Channel>,
}

impl GrpcDebugger {
    pub async fn new() -> Self {
        let client = NodeVisualizerClient::connect("http://[::1]:50051")
            .await
            .unwrap();
        GrpcDebugger { client }
    }
}

impl Debugger for GrpcDebugger {
    fn on_node_added(&self, node: &Node<Self>, node_id: usize) {
        let mut client = self.client.clone();

        let node_type = node.expr_type.to_string();
        let label = node.value.read().unwrap().to_string();
        let id: u64 = node_id.try_into().unwrap();

        std::thread::spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();

            rt.block_on(async {
                if let Err(e) = client
                    .add_nodes(AddNodesRequest {
                        nodes: vec![NodeData {
                            label,
                            node_type,
                            id,
                        }],
                    })
                    .await
                {
                    eprintln!("Failed to notify debugger: {}", e);
                }
            });
        })
        .join()
        .unwrap();
    }

    fn on_node_removed(&self, _node_id: usize) {}

    fn suspend(_location: CodeLocation, _ctx: &mut Context<Self>) {}
}
