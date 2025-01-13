use std::sync::{atomic::AtomicBool, Arc};

use bimap::BiMap;
use ef3r::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::NoOpDebugger,
    frp::{
        combined_node, filter_node, fold_node, map_node, process_event_frame,
        with_lock, Node,
    },
    interpreter::{apply_traced, Context},
    stdlib::ef3r_stdlib,
    types::ExprType,
};
use parking_lot::{Mutex, RwLock};

#[test]
fn test_map_node() {
    let on_update = |_: &Context<NoOpDebugger>, new_value| {
        println!("New value is: {:?}", new_value)
    };

    // Setup our FRP graph with two nodes: An input node and a mapped node.

    let context =
        Arc::new(RwLock::new(ef3r_stdlib(NoOpDebugger::new(), BiMap::new())));

    let mut context_lock = context.write();

    let node_index = Node::new(
        Arc::new(on_update),
        &mut context_lock.graph.lock(),
        ExprType::Int,
        TracedExprRec::Int(20).traced(),
    );

    println!("Building mapped node");

    let mapped_node_index = map_node(
        Arc::new(on_update),
        &mut context_lock,
        node_index,
        ExprType::Int,
        Arc::new(Mutex::new(move |ctx: &Context<NoOpDebugger>, x| {
            apply_traced(
                ctx,
                TracedExpr::resolve(&ctx, "*"),
                &[TracedExprRec::Int(2).traced(), x],
            )
            .unwrap()
        })),
    );

    println!("After mapped node");

    Node::update(
        node_index,
        &mut context_lock,
        TracedExprRec::Int(21).traced(),
    );

    // Check the initial state of the mapped node is what we expect

    let graph = context_lock.graph.lock();

    let mapped_node = graph.node_weight(mapped_node_index);

    assert_eq!(
        TracedExprRec::Int(40),
        mapped_node.unwrap().current().evaluated.clone()
    );

    drop(graph);

    // Update the input value and step through a single frame of the event loop.

    process_event_frame(&mut context_lock);

    // Check that the mapped node has updated.

    let graph = &mut context_lock.graph.lock();
    let mapped_node = graph.node_weight(mapped_node_index);

    assert_eq!(
        TracedExprRec::Int(42),
        mapped_node.unwrap().current().evaluated.clone()
    );
}

#[test]
fn test_filter_node() {
    let on_update = |_: &Context<NoOpDebugger>, new_value| {
        println!("New value is: {:?}", new_value)
    };

    // Setup our FRP graph with two nodes: An input node and a mapped node.

    let context = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

    let node_index = Node::new(
        Arc::new(on_update),
        &mut context.graph.lock(),
        ExprType::Int,
        TracedExprRec::Int(1).traced(),
    );

    let filtered_node_index = filter_node::<NoOpDebugger>(
        Arc::new(on_update),
        &mut context.graph.lock(),
        node_index,
        Box::new(move |_, x| match x.evaluated {
            TracedExprRec::Int(i) => i % 2 != 0,
            _ => true,
        }),
    );

    Node::update(node_index, &context, TracedExprRec::Int(2).traced());

    let graph = context.graph.lock();

    let filtered_node = graph.node_weight(filtered_node_index).unwrap();

    assert_eq!(
        TracedExprRec::Int(1),
        filtered_node.current().evaluated.clone()
    );

    drop(graph);

    // Update the input value and step through a single frame of the event loop.
    process_event_frame(&context);
    // Check that the mapped node has not updated, since we are filtering out even values..

    let graph = context.graph.lock();

    let filtered_node = graph.node_weight(filtered_node_index).unwrap();

    assert_eq!(
        TracedExprRec::Int(1),
        filtered_node.current().evaluated.clone()
    );

    drop(graph);

    Node::update(node_index, &context, TracedExprRec::Int(3).traced());

    process_event_frame(&context);

    // Check that the mapped node has updated, since now we have updated to an odd value.

    let graph = context.graph.lock();

    let filtered_node = graph.node_weight(filtered_node_index).unwrap();

    assert_eq!(
        TracedExprRec::Int(3),
        filtered_node.current().evaluated.clone()
    );
}

#[test]
fn test_combined_node() {
    let on_update = |_: &Context<NoOpDebugger>, new_value| {
        println!("New value is: {:?}", new_value)
    };

    // Setup our FRP graph with three nodes: Two input nodes, and a combined output node.

    let context = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

    let first_node_index = Node::new(
        Arc::new(on_update),
        &mut context.graph.lock(),
        ExprType::Int,
        TracedExprRec::Int(2).traced(),
    );

    let second_node_index = Node::new(
        Arc::new(on_update),
        &mut context.graph.lock(),
        ExprType::Int,
        TracedExprRec::Int(3).traced(),
    );

    let cloned_ctx = context.clone();

    let combined_node_index = combined_node(
        Arc::new(on_update),
        &context,
        first_node_index,
        second_node_index,
        ExprType::Int,
        Box::new(move |ctx, x, y| {
            apply_traced(ctx, TracedExpr::resolve(ctx, "*"), &[x, y]).unwrap()
        }),
    );

    let graph = context.graph.lock();

    let combined_node = &mut graph.node_weight(combined_node_index).unwrap();

    // Check initial state

    assert_eq!(TracedExprRec::Int(6), combined_node.current().evaluated);

    drop(graph);

    // Check state after updating one of the fields

    Node::update(first_node_index, &context, TracedExprRec::Int(3).traced());

    process_event_frame(&context);

    let graph = context.graph.lock();

    let combined_node = &mut graph.node_weight(combined_node_index).unwrap();

    assert_eq!(TracedExprRec::Int(9), combined_node.current().evaluated);
}

#[test]
fn test_fold_node() {
    let on_update = |_: &Context<NoOpDebugger>, new_value| {
        println!("New value is: {:?}", new_value)
    };

    // Setup our FRP graph with an input node and a folded node

    let context = ef3r_stdlib(NoOpDebugger::new(), BiMap::new());

    let event_node_index = Node::new(
        Arc::new(on_update),
        &mut context.graph.lock(),
        ExprType::Any,
        TracedExprRec::None.traced(),
    );

    let folded_node_index =
        fold_node(
            &context,
            Arc::new(on_update),
            event_node_index,
            TracedExprRec::Int(2).traced(),
            Box::new(|_, acc: TracedExpr<usize>, event: TracedExpr<usize>| {
                match (acc.evaluated, event.evaluated) {
                    (TracedExprRec::Int(a), TracedExprRec::Int(b)) => {
                        TracedExprRec::Int(a + b).traced()
                    }
                    _ => panic!("Expected integers"),
                }
            }),
        );

    let graph = context.graph.lock();

    // Verify initial state
    let folded_node = graph.node_weight(folded_node_index).unwrap();
    assert_eq!(TracedExprRec::Int(2), folded_node.current().evaluated);

    drop(graph);

    // Update input value

    Node::update(event_node_index, &context, TracedExprRec::Int(3).traced());

    process_event_frame(&context);

    // Verify folded value is updated
    let graph = context.graph.lock();
    let folded_node = graph.node_weight(folded_node_index).unwrap();
    assert_eq!(TracedExprRec::Int(5), folded_node.current().evaluated);
}
