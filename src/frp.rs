use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};

use parking_lot::{lock_api::MutexGuard, Mutex, RawMutex, RwLock};

use daggy::{
    petgraph::{
        adj::Neighbors,
        algo::toposort,
        visit::{GraphBase, GraphRef, IntoNeighbors, IntoNeighborsDirected},
        Direction,
    },
    Dag, NodeIndex,
};

use crate::{
    ast::traced_expr::TracedExpr,
    debugging::Debugger,
    interpreter::Context,
    typechecking::{type_of, RuntimeLookup},
    types::ExprType,
};

///
/// A node in the graph of functional reactive values.
///
/// This is a directed acyclic graph used to determine
///  how updates are propogated throughout the network.
///
/// In classical Functional Reactive Programming, there are
///  two types of values used for reactivity:
///
///   * Events: Something that happens at discrete points in time.
///   * Behaviors: Something that varies (continiously) over time.
///
/// EF3R tweaks these concepts slightly, using the unifying idea of
///  _reactive values_, AKA _signal_s. These are values that vary
///  discretely over time, and like events, can signal updates to
///  values reactively.
///
/// In this view, there are still two seperate notions corresponding
///  to events and behaviors, but they are both modeled with the same
///  construct. "Behaviors" in this system are just plain reactive values,
///  and "Events" in this system are viewed as behaviors without an
///  "initial state".
///
/// In other words, `Event<T>` can be thought of as just a `Behavior<Option<T>>`.
///
#[derive(Clone)]
pub struct Node<T: Debugger + Send + Sync + 'static> {
    /// The type of expressions used in the node.
    pub expr_type: ExprType,
    /// The underlying value held by this node.
    pub value: Arc<RwLock<TracedExpr<usize>>>,
    /// Flag to check if the value has been changed since the last event loop.
    dirty: Arc<AtomicBool>,
    ///
    /// Function to call when the node's value has been updated to a new value.
    ///
    /// Should only be used as a means for observability to communicate with an outside
    ///  system. In other words, this should not be used to update the values of
    ///  other nodes in the node graph.
    ///
    pub on_update:
        Arc<Mutex<Arc<dyn Fn(&Context<T>, TracedExpr<usize>) + Send + Sync>>>,
    /// Action to perform to update state of this node when one of its dependencies
    /// has updated one of its values.
    on_dependency_update:
        Arc<dyn Fn(&Context<T>, NodeIndex, TracedExpr<usize>) + Send + Sync>,
}

impl<T: Debugger + Send + Sync + 'static> Node<T> {
    ///
    /// Get the current value of the node.
    ///
    pub fn current(&self) -> TracedExpr<usize> {
        self.value.read().clone()
    }

    ///
    /// Builds a new mutable node whose value can manually be updated externally.
    ///
    pub fn new(
        on_update: Arc<dyn Fn(&Context<T>, TracedExpr<usize>) + Send + Sync>,
        graph: &mut Dag<Node<T>, (), u32>,
        expr_type: ExprType,
        initial: TracedExpr<usize>,
    ) -> NodeIndex {
        let value = Arc::new(RwLock::new(initial));

        let dirty = Arc::new(AtomicBool::new(false));

        let node = Node {
            expr_type,
            value,
            dirty,
            on_update: Arc::new(Mutex::new(on_update)),
            on_dependency_update: Arc::new(|_, _, _| {
                // A new node does not depend on any other nodes,
                // so this should never be called.
            }),
        };

        graph.add_node(node)
    }

    ///
    /// Utility to update a node, while keeping all invariants
    ///  (i.e. node needs to be marked dirty on updates).
    ///
    pub fn update(
        index: NodeIndex,
        ctx: &Context<T>,
        new_value: TracedExpr<usize>,
    ) {
        let mut graph = ctx.graph.lock();
        let node = graph.node_weight_mut(index).unwrap();

        let on_update_clone = node.on_update.clone();
        let on_update = on_update_clone.lock();

        let mut value = node.value.write();

        *value = new_value.clone();
        node.dirty.store(true, Ordering::SeqCst);

        drop(value);
        drop(graph);

        (*on_update)(ctx, new_value);
    }
}

///
/// Build a variant of a node whose values are mapped.
///
pub fn map_node<T: Debugger + Send + Sync + 'static>(
    on_update: Arc<dyn Fn(&Context<T>, TracedExpr<usize>) + Send + Sync>,
    ctx: &Context<T>,
    parent_index: NodeIndex,
    result_type: ExprType,
    transform: Arc<
        Mutex<
            dyn Fn(&Context<T>, TracedExpr<usize>) -> TracedExpr<usize>
                + Send
                + Sync,
        >,
    >,
) -> NodeIndex {
    let parent_value = {
        let value = ctx
            .graph
            .lock()
            .node_weight(parent_index)
            .unwrap()
            .value
            .read()
            .clone();
        value
    };

    let dirty = Arc::new(AtomicBool::new(false));

    let transform_lock = transform.lock();

    let initial = (transform_lock)(ctx, parent_value);

    let value = Arc::new(RwLock::new(initial));

    drop(transform_lock);

    let cloned = value.clone();

    let on_dependency_update =
        Arc::new(move |context: &Context<T>, id, new_value| {
            if id == parent_index {
                // Update the value to transform of new_value.
                *cloned.write() = (transform.lock())(context, new_value);
            }
        });

    let new_node = Node {
        expr_type: result_type,
        value,
        dirty,
        on_update: Arc::new(Mutex::new(on_update)),
        on_dependency_update,
    };

    let graph = &mut ctx.graph.lock();

    let new_node_index = graph.add_node(new_node);

    graph.add_edge(parent_index, new_node_index, ()).unwrap();

    new_node_index
}

///
/// Build a variant of a node whose values are filtered by the given predicate.
///
pub fn filter_node<T: Debugger + Send + Sync>(
    on_update: Arc<dyn Fn(&Context<T>, TracedExpr<usize>) + Send + Sync>,
    graph: &mut Dag<Node<T>, (), u32>,
    parent_index: NodeIndex,
    predicate: Box<
        dyn Fn(&Context<T>, TracedExpr<usize>) -> bool + Send + Sync,
    >,
) -> NodeIndex {
    let parent = graph.node_weight(parent_index).unwrap();

    let initial = parent.value.read().clone();

    let value = Arc::new(RwLock::new(initial));

    let dirty = Arc::new(AtomicBool::new(false));

    let cloned = value.clone();

    let on_dependency_update =
        Arc::new(move |ctx: &Context<T>, id, new_value: TracedExpr<usize>| {
            if id == parent_index && predicate(ctx, new_value.clone()) {
                *cloned.write() = new_value;
            }
        });

    let new_node = Node {
        expr_type: parent.expr_type.clone(),
        value: value.clone(),
        dirty,
        on_update: Arc::new(Mutex::new(on_update)),
        on_dependency_update,
    };

    let new_node_index = graph.add_node(new_node);

    graph.add_edge(parent_index, new_node_index, ()).unwrap();

    new_node_index
}

///
/// Build a node combining the value of two dependent nodes.
///
/// The resultant node will update whenever either of the input nodes updates.
///
pub fn combined_node<T: Debugger + Send + Sync + 'static>(
    on_update: Arc<dyn Fn(&Context<T>, TracedExpr<usize>) + Send + Sync>,
    context: &Context<T>,
    first_node_index: NodeIndex,
    second_node_index: NodeIndex,
    result_type: ExprType,
    transform: Box<
        dyn Fn(
                &Context<T>,
                TracedExpr<usize>,
                TracedExpr<usize>,
            ) -> TracedExpr<usize>
            + Send
            + Sync,
    >,
) -> NodeIndex {
    let graph = context.graph.lock();

    let first_node = graph.node_weight(first_node_index).unwrap();

    let second_node = graph.node_weight(second_node_index).unwrap();

    let (first_value, second_value) = (
        first_node.value.read().clone(),
        second_node.value.read().clone(),
    );

    drop(graph);

    let initial = transform(context, first_value, second_value);

    let value = Arc::new(RwLock::new(initial));

    let dirty = Arc::new(AtomicBool::new(false));

    let cloned = value.clone();

    let on_dependency_update = Arc::new(
        move |context: &Context<T>, id, new_value: TracedExpr<usize>| {
            println!("Updating node {:?}", id);

            let graph = context.graph.lock();

            let first_value_ref =
                graph.node_weight(first_node_index).unwrap().value.clone();

            let second_value_ref =
                graph.node_weight(second_node_index).unwrap().value.clone();

            drop(graph);

            match id {
                x if x == first_node_index => {
                    println!("Updating first node");
                    let second_value = second_value_ref.read().clone();
                    *cloned.write() =
                        transform(context, new_value, second_value);
                }
                x if x == second_node_index => {
                    println!("Updating second node");
                    let first_value = first_value_ref.read().clone();
                    *cloned.write() =
                        transform(context, first_value, new_value);
                }
                _ => (),
            }
        },
    );

    let new_node = Node {
        expr_type: result_type,
        value: value.clone(),
        dirty,
        on_update: Arc::new(Mutex::new(on_update)),
        on_dependency_update,
    };

    let graph = &mut context.graph.lock();

    let new_node_index = graph.add_node(new_node);

    graph
        .add_edge(first_node_index, new_node_index, ())
        .unwrap();

    graph
        .add_edge(second_node_index, new_node_index, ())
        .unwrap();

    new_node_index
}

pub fn fold_node<'a, T: Debugger + Send + Sync + 'static>(
    ctx: &Context<T>,
    on_update: Arc<dyn Fn(&Context<T>, TracedExpr<usize>) + Send + Sync>,
    event_index: NodeIndex,
    initial: TracedExpr<usize>,
    fold: Box<
        dyn Fn(
                &Context<T>,
                TracedExpr<usize>,
                TracedExpr<usize>,
            ) -> TracedExpr<usize>
            + Send
            + Sync,
    >,
) -> NodeIndex {
    let initial_clone = initial.clone();
    let value = Arc::new(RwLock::new(initial.clone()));

    let dirty = Arc::new(AtomicBool::new(false));

    let cloned = value.clone();
    let fold = Arc::new(fold);
    let fold2 = fold.clone();

    let on_dependency_update = Arc::new(move |ctx: &Context<T>, id, event| {
        if id == event_index {
            let current_value = {
                let lock = cloned.read();
                lock.clone()
            };

            // Update the value to transform of new_value.
            *cloned.write() = fold2(ctx, event, current_value);
        }
    });

    let new_node = Node {
        expr_type: type_of::<_, _, RuntimeLookup>(
            &ctx.expression_context.read(),
            &initial_clone.evaluated,
        )
        .unwrap(),
        value: value.clone(),
        dirty,
        on_update: Arc::new(Mutex::new(on_update)),
        on_dependency_update,
    };

    let graph = &mut ctx.graph.lock();

    let new_node_index = graph.add_node(new_node);

    graph.add_edge(event_index, new_node_index, ()).unwrap();

    new_node_index
}

///
/// Main event loop for the FRP runtime.
///
/// Continually checks nodes for changed values, and propogates those
///  values to dependent nodes as-needed in topologically-sorted order.
///
/// As opposed to other popular modern "reactive" frameworks such as
///  kotlinx.flow or anything in the reactiveX family, rather than
///  allowing reactive values to update asynchronously, we use an
///  event loop to synchronize to a linear "timeline".
///
/// External events can still hook into the system asynchronously, but
///  from the perspective of `Event`s and `Behavior`s inside the system,
///  everything occurs "at once" within a single "frame" (i.e. a single
///  execution of the loop).
///
/// This has the advantage of banishing any kind of concurrency concerns
///  to the "edge" of the FRP runtime. Given the same starting conditions,
///  and the same odering of external events, the resulting state of the entire
///  FRP network is gaurnteed to be the same.
///
pub fn event_loop<T: Debugger + 'static>(ctx: &mut Context<T>) {
    loop {
        process_event_frame(ctx);
    }
}

///
/// Runs a single iteration of the [event_loop], for testing purposes, or for integrating
/// into a custom workflow.
///
pub fn process_event_frame<T: Debugger + 'static>(ctx: &Context<T>) {
    let nodes = toposort(&*ctx.graph.lock(), None).unwrap();

    for node_id in nodes {
        let derived_nodes: Vec<_>;
        let dirty;
        let value;

        {
            let graph = &ctx.graph.lock();

            println!(
                "Updating values for node {:?} with value {:?}",
                node_id,
                graph.node_weight(node_id).unwrap().value.read()
            );

            // Check if any of the values have been changed.
            let node = graph.node_weight(node_id).unwrap();

            derived_nodes = graph.neighbors(node_id).collect();

            dirty = node.dirty.load(Ordering::SeqCst);
            value = node.value.read().clone();

            println!("Node was dirty: {}", dirty);
        }

        if dirty {
            // If so, notify dependent nodes.
            for dependent_id in derived_nodes {
                println!("Checking dependent node: {:?}", dependent_id);
                let dependent_node_update = {
                    let graph = &ctx.graph.lock();
                    graph
                        .node_weight(dependent_id)
                        .unwrap()
                        .on_dependency_update
                        .clone()
                };

                println!("Got dependent node");

                dependent_node_update(ctx, node_id, value.clone());
            }

            let graph = &ctx.graph.lock();
            let node = graph.node_weight(node_id).unwrap();
            node.dirty.store(false, Ordering::SeqCst);
        }
    }
}

/// Utility function to execute a lock within a function block,
/// returning the result from using said lock.
pub fn with_lock<T, F, Ref, Res>(mutex: Ref, f: F) -> Res
where
    F: FnOnce(&mut T) -> Res,
    Ref: AsRef<RwLock<T>>,
{
    let mut guard = mutex.as_ref().write();
    f(&mut *guard)
}
