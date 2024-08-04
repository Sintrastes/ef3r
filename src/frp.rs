use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc, RwLock,
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
pub struct Node<T> {
    /// The underlying value held by this node.
    value: Arc<RwLock<T>>,
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
    on_update: fn(T),
    /// Action to perform to update state of this node when one of its dependencies
    /// has updated one of its values.
    on_dependency_update: Box<dyn Fn(NodeIndex, T)>,
}

impl<T: Clone> Node<T> {
    ///
    /// Get the current value of the node.
    ///
    pub fn current(&self) -> T {
        self.value.read().unwrap().clone()
    }

    ///
    /// Builds a new mutable node whose value can manually be updated externally.
    ///
    pub fn new(
        on_update: fn(T),
        traced: Arc<AtomicBool>,
        graph: &mut Dag<Node<T>, (), u32>,
        initial: T,
    ) -> NodeIndex {
        let value = Arc::new(RwLock::new(initial));

        let dirty = Arc::new(AtomicBool::new(false));

        let node = Node {
            value,
            dirty,
            traced,
            on_update,
            on_dependency_update: Box::new(|_, _| {
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
    pub fn update(&self, new_value: T) {
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
pub fn map_node<T: 'static + Clone>(
    on_update: fn(T),
    traced: Arc<AtomicBool>,
    graph: &mut Dag<Node<T>, (), u32>,
    parent_index: NodeIndex,
    transform: Box<dyn Fn(T) -> T>,
) -> NodeIndex {
    let parent = graph.node_weight(parent_index).unwrap();

    let initial = transform(parent.value.read().unwrap().clone());

    let value = Arc::new(RwLock::new(initial));

    let dirty = Arc::new(AtomicBool::new(false));

    let cloned = value.clone();

    let on_dependency_update = Box::new(move |id, new_value| {
        if id == parent_index {
            // Update the value to transform of new_value.
            *cloned.write().unwrap() = transform(new_value);
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
/// Build a variant of a node whose values are filtered by the given predicate.
///
pub fn filter_node<T: 'static + Clone>(
    on_update: fn(T),
    traced: Arc<AtomicBool>,
    graph: &mut Dag<Node<T>, (), u32>,
    parent_index: NodeIndex,
    predicate: Box<dyn Fn(T) -> bool>,
) -> NodeIndex {
    let parent = graph.node_weight(parent_index).unwrap();

    let initial = parent.value.read().unwrap().clone();

    let value = Arc::new(RwLock::new(initial));

    let dirty = Arc::new(AtomicBool::new(false));

    let cloned = value.clone();

    let on_dependency_update = Box::new(move |id, new_value: T| {
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
pub fn combined_node<T: 'static + Clone>(
    on_update: fn(T),
    traced: Arc<AtomicBool>,
    graph: &mut Dag<Node<T>, (), u32>,
    first_node_index: NodeIndex,
    second_node_index: NodeIndex,
    transform: Box<dyn Fn(T, T) -> T>,
) -> NodeIndex {
    let first_node = graph.node_weight(first_node_index).unwrap();

    let second_node = graph.node_weight(second_node_index).unwrap();

    let initial = transform(
        first_node.value.read().unwrap().clone(),
        second_node.value.read().unwrap().clone(),
    );

    let value = Arc::new(RwLock::new(initial));

    let dirty = Arc::new(AtomicBool::new(false));

    let cloned = value.clone();

    let first_value_ref =
        graph.node_weight(first_node_index).unwrap().value.clone();

    let second_value_ref =
        graph.node_weight(second_node_index).unwrap().value.clone();

    let on_dependency_update = Box::new(move |id, new_value: T| {
        println!("Updating node {:?}", id);
        match id {
            x if x == first_node_index => {
                println!("Updating first node");
                let second_value = second_value_ref.read().unwrap().clone();
                *cloned.write().unwrap() = transform(new_value, second_value);
            }
            x if x == second_node_index => {
                println!("Updating second node");
                let first_value = first_value_ref.read().unwrap().clone();
                *cloned.write().unwrap() = transform(first_value, new_value);
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

    let new_node_index = graph.add_node(new_node);

    graph
        .add_edge(first_node_index, new_node_index, ())
        .unwrap();

    graph
        .add_edge(second_node_index, new_node_index, ())
        .unwrap();

    new_node_index
}

pub fn fold_node(
    on_update: fn(TracedExpr),
    traced: Arc<AtomicBool>,
    graph: &mut Dag<Node<TracedExpr>, (), u32>,
    event_index: NodeIndex,
    initial: TracedExpr,
    fold: fn(TracedExpr, TracedExpr) -> TracedExpr,
) -> Node<TracedExpr> {
    let value = Arc::new(RwLock::new(initial));

    let dirty = Arc::new(AtomicBool::new(false));

    let cloned = value.clone();

    let on_dependency_update = Box::new(move |id, event| {
        if id == event_index {
            // Update the value to transform of new_value.
            *cloned.write().unwrap() =
                fold(event, cloned.read().unwrap().clone());
        }
    });

    let new_node = Node {
        value: value.clone(),
        dirty,
        traced,
        on_update,
        on_dependency_update,
    };

    let _new_node_index = graph.add_node(new_node);

    todo!()
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
pub fn event_loop(ctx: &Context) {
    loop {
        process_event_frame(ctx);
    }
}

///
/// Runs a single iteration of the [event_loop], for testing purposes, or for integrating
/// into a custom workflow.
///
pub fn process_event_frame(ctx: &Context) {
    let nodes = toposort(&ctx.graph, None).unwrap();

    for node_id in nodes {
        println!(
            "Updating values for node {:?} with value {:?}",
            node_id,
            &ctx.graph
                .node_weight(node_id)
                .unwrap()
                .value
                .read()
                .unwrap()
        );

        // Check if any of the values have been changed.
        let node = ctx.graph.node_weight(node_id).unwrap();

        let derived_nodes = ctx.graph.neighbors(node_id);

        let dirty = node.dirty.load(Ordering::SeqCst);

        println!("Node was dirty: {}", dirty);

        if dirty {
            // If so, notify dependent nodes.
            for dependent_id in derived_nodes {
                println!("Checking dependent node: {:?}", dependent_id);
                let dependent_node =
                    &ctx.graph.node_weight(dependent_id).unwrap();

                (dependent_node.on_dependency_update)(
                    node_id,
                    node.value.read().unwrap().clone(),
                )
            }

            node.dirty.store(false, Ordering::SeqCst);
        }
    }
}
