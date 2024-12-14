use std::{
    borrow::BorrowMut,
    ops::DerefMut,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex, RwLock,
    },
};

use daggy::{
    petgraph::{algo::toposort, visit::IntoNeighbors},
    Dag, NodeIndex,
};

use crate::{ast::TracedExpr, interpreter::Context};

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
pub struct Node<'a: 'static> {
    /// The underlying value held by this node.
    value: Arc<RwLock<TracedExpr>>,
    /// Flag to check if the value has been changed since the last event loop.
    dirty: Arc<AtomicBool>,
    /// Flag to check if this node is currently being traced or not.
    traced: Arc<AtomicBool>,
    ///
    /// Function to call when the node's value has been updated to a new value.
    ///
    /// Should only be used as a means for observability to communicate with an outside
    ///  system. In other words, this should not be used to update the values of
    ///  other nodes in the node graph.
    ///
    on_update: fn(TracedExpr),
    /// Action to perform to update state of this node when one of its dependencies
    /// has updated one of its values.
    on_dependency_update:
        Arc<dyn Fn(Arc<Mutex<Context<'a>>>, NodeIndex, TracedExpr)>,
}

impl<'a> Node<'a> {
    ///
    /// Get the current value of the node.
    ///
    pub fn current(&self) -> TracedExpr {
        self.value.read().unwrap().clone()
    }

    ///
    /// Builds a new mutable node whose value can manually be updated externally.
    ///
    pub fn new(
        on_update: fn(TracedExpr),
        traced: Arc<AtomicBool>,
        graph: &mut Dag<Node<'a>, (), u32>,
        initial: TracedExpr,
    ) -> NodeIndex {
        let value = Arc::new(RwLock::new(initial));

        let dirty = Arc::new(AtomicBool::new(false));

        let node = Node {
            value,
            dirty,
            traced,
            on_update,
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
    pub fn update(&self, new_value: TracedExpr) {
        let mut value = self.value.write().unwrap();

        *value = new_value.clone();
        self.dirty.store(true, Ordering::SeqCst);
        if self.traced.load(Ordering::SeqCst) {
            (self.on_update)(new_value)
        };
    }
}

///
/// Build a variant of a node whose values are mapped.
///
pub fn map_node(
    on_update: fn(TracedExpr),
    traced: Arc<AtomicBool>,
    context: Arc<Mutex<Context>>,
    parent_index: NodeIndex,
    transform: Arc<Mutex<dyn Fn(TracedExpr) -> TracedExpr>>,
) -> NodeIndex {
    let parent_value = {
        let mut context_lock = context.lock().unwrap();
        let graph = &mut context_lock.graph;
        println!("GOT MAP NODE LOCK");
        let value = graph
            .node_weight(parent_index)
            .unwrap()
            .value
            .read()
            .unwrap()
            .clone();
        value
    };

    let dirty = Arc::new(AtomicBool::new(false));

    let transform_lock = transform.lock().unwrap();

    let initial = (transform_lock)(parent_value);

    let value = Arc::new(RwLock::new(initial));

    drop(transform_lock);

    let cloned = value.clone();

    let on_dependency_update =
        Arc::new(move |_ctx: Arc<Mutex<Context>>, id, new_value| {
            if id == parent_index {
                // Update the value to transform of new_value.
                *cloned.write().unwrap() =
                    (transform.lock().unwrap())(new_value);
            }
        });

    let new_node = Node {
        value,
        dirty,
        traced,
        on_update,
        on_dependency_update,
    };

    let mut context_lock = context.lock().unwrap();
    let graph = &mut context_lock.graph;

    let new_node_index = graph.add_node(new_node);

    graph.add_edge(parent_index, new_node_index, ()).unwrap();

    new_node_index
}

///
/// Build a variant of a node whose values are filtered by the given predicate.
///
pub fn filter_node(
    on_update: fn(TracedExpr),
    traced: Arc<AtomicBool>,
    graph: &mut Dag<Node, (), u32>,
    parent_index: NodeIndex,
    predicate: Box<dyn Fn(TracedExpr) -> bool>,
) -> NodeIndex {
    let parent = graph.node_weight(parent_index).unwrap();

    let initial = parent.value.read().unwrap().clone();

    let value = Arc::new(RwLock::new(initial));

    let dirty = Arc::new(AtomicBool::new(false));

    let cloned = value.clone();

    let on_dependency_update =
        Arc::new(move |_: Arc<Mutex<Context>>, id, new_value: TracedExpr| {
            if id == parent_index && predicate(new_value.clone()) {
                *cloned.write().unwrap() = new_value;
            }
        });

    let new_node = Node {
        value: value.clone(),
        dirty,
        traced,
        on_update,
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
pub fn combined_node(
    on_update: fn(TracedExpr),
    traced: Arc<AtomicBool>,
    context: Arc<Mutex<Context>>,
    first_node_index: NodeIndex,
    second_node_index: NodeIndex,
    transform: Box<dyn Fn(TracedExpr, TracedExpr) -> TracedExpr>,
) -> NodeIndex {
    let (first_value, second_value) = with_lock(context.as_ref(), |lock| {
        let graph = &mut lock.graph;

        let first_node = graph.node_weight(first_node_index).unwrap();

        let second_node = graph.node_weight(second_node_index).unwrap();

        (
            first_node.value.read().unwrap().clone(),
            second_node.value.read().unwrap().clone(),
        )
    });

    let initial = transform(first_value, second_value);

    let value = Arc::new(RwLock::new(initial));

    let dirty = Arc::new(AtomicBool::new(false));

    let cloned = value.clone();

    let on_dependency_update =
        Arc::new(move |ctx: Arc<Mutex<Context>>, id, new_value: TracedExpr| {
            let ctx = ctx.lock().unwrap();

            println!("Updating node {:?}", id);

            let first_value_ref = ctx
                .graph
                .node_weight(first_node_index)
                .unwrap()
                .value
                .clone();

            let second_value_ref = ctx
                .graph
                .node_weight(second_node_index)
                .unwrap()
                .value
                .clone();

            drop(ctx);

            match id {
                x if x == first_node_index => {
                    println!("Updating first node");
                    let second_value = second_value_ref.read().unwrap().clone();
                    *cloned.write().unwrap() =
                        transform(new_value, second_value);
                }
                x if x == second_node_index => {
                    println!("Updating second node");
                    let first_value = first_value_ref.read().unwrap().clone();
                    *cloned.write().unwrap() =
                        transform(first_value, new_value);
                }
                _ => (),
            }
        });

    let new_node = Node {
        value: value.clone(),
        dirty,
        traced,
        on_update,
        on_dependency_update,
    };

    with_lock(context.as_ref(), |lock| {
        let mut graph = &mut lock.graph;

        let new_node_index = graph.add_node(new_node);

        graph
            .add_edge(first_node_index, new_node_index, ())
            .unwrap();

        graph
            .add_edge(second_node_index, new_node_index, ())
            .unwrap();

        new_node_index
    })
}

pub fn fold_node<'a>(
    on_update: fn(TracedExpr),
    traced: Arc<AtomicBool>,
    graph: &mut Dag<Node, (), u32>,
    event_index: NodeIndex,
    initial: TracedExpr,
    fold: fn(TracedExpr, TracedExpr) -> TracedExpr,
) -> NodeIndex {
    let value = Arc::new(RwLock::new(initial));

    let dirty = Arc::new(AtomicBool::new(false));

    let cloned = value.clone();

    let on_dependency_update = Arc::new(move |_, id, event| {
        if id == event_index {
            let current_value = {
                let lock = cloned.read().unwrap();
                lock.clone()
            };

            // Update the value to transform of new_value.
            *cloned.write().unwrap() = fold(event, current_value);
        }
    });

    let new_node = Node {
        value: value.clone(),
        dirty,
        traced,
        on_update,
        on_dependency_update,
    };

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
pub fn event_loop(ctx: Arc<Mutex<Context>>) {
    loop {
        process_event_frame(ctx.clone());
    }
}

///
/// Runs a single iteration of the [event_loop], for testing purposes, or for integrating
/// into a custom workflow.
///
pub fn process_event_frame(ctx: Arc<Mutex<Context>>) {
    let ctx_copy = ctx.clone();

    let ctx_lock = ctx.lock().unwrap();
    let nodes = toposort(&ctx_lock.graph, None).unwrap();
    drop(ctx_lock);

    for node_id in nodes {
        let mut derived_nodes: Vec<_>;
        let dirty;
        let value;

        {
            let ctx_lock = ctx.lock().unwrap();
            println!(
                "Updating values for node {:?} with value {:?}",
                node_id,
                (&ctx_lock.graph)
                    .node_weight(node_id)
                    .unwrap()
                    .value
                    .read()
                    .unwrap()
            );

            // Check if any of the values have been changed.
            let node = ctx_lock.graph.node_weight(node_id).unwrap();

            derived_nodes = ctx_lock.graph.neighbors(node_id).collect();

            dirty = node.dirty.load(Ordering::SeqCst);
            value = node.value.read().unwrap().clone();

            println!("Node was dirty: {}", dirty);
        }

        if dirty {
            // If so, notify dependent nodes.
            for dependent_id in derived_nodes {
                println!("Checking dependent node: {:?}", dependent_id);
                let ctx_lock = ctx.lock().unwrap();
                let dependent_node_update = ctx_lock
                    .graph
                    .node_weight(dependent_id)
                    .unwrap()
                    .on_dependency_update
                    .clone();

                println!("Got dependent node");

                drop(ctx_lock);

                (dependent_node_update)(
                    ctx_copy.clone(),
                    node_id,
                    value.clone(),
                );
            }

            let ctx_lock = ctx.lock().unwrap();
            let node = ctx_lock.graph.node_weight(node_id).unwrap();
            node.dirty.store(false, Ordering::SeqCst);
        }
    }
}

/// Utility function to execute a lock within a function block,
/// returning the result from using said lock.
pub fn with_lock<T, F, R>(mutex: &Mutex<T>, f: F) -> R
where
    F: FnOnce(&mut T) -> R,
{
    let mut guard = mutex.lock().unwrap();
    f(&mut *guard)
}
