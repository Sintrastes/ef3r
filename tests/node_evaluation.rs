use std::sync::{atomic::AtomicBool, Arc, Mutex};

use bimap::BiMap;
use ef3r::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    debugging::NoOpDebugger,
    frp::{
        combined_node, filter_node, fold_node, map_node, process_event_frame,
        with_lock, Node,
    },
    interpreter::apply_traced,
    stdlib::{ef3r_stdlib, INT_MUL_ID},
    types::ExprType,
};

#[test]
fn test_map_node() {
    let on_update = |new_value| println!("New value is: {:?}", new_value);

    let is_traced = Arc::new(AtomicBool::new(false));

    // Setup our FRP graph with two nodes: An input node and a mapped node.

    let context =
        Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new(), BiMap::new())));

    let mut context_lock = context.lock().unwrap();

    let context_cloned = context.clone();

    let graph = &mut context_lock.graph;

    let node_index = Node::new(
        on_update,
        is_traced.clone(),
        graph,
        ExprType::Int,
        TracedExprRec::Int(20).traced(),
    );

    drop(context_lock);

    let mapped_node_index = map_node(
        on_update,
        is_traced,
        context.clone(),
        node_index,
        ExprType::Int,
        Arc::new(Mutex::new(move |x| {
            apply_traced(
                context_cloned.clone(),
                TracedExprRec::BuiltinFunction(INT_MUL_ID).traced(),
                &[TracedExprRec::Int(2).traced(), x],
            )
            .unwrap()
        })),
    );

    let mut context_lock = context.lock().unwrap();
    let graph = &mut context_lock.graph;

    let node = graph.node_weight(node_index).unwrap();

    node.update(TracedExprRec::Int(21).traced());

    // Check the initial state of the mapped node is what we expect

    let mapped_node = graph.node_weight(mapped_node_index);

    assert_eq!(
        TracedExprRec::Int(40),
        mapped_node.unwrap().current().evaluated.clone()
    );

    drop(context_lock);

    // Update the input value and step through a single frame of the event loop.

    process_event_frame(context.clone());

    // Check that the mapped node has updated.

    let mut context_lock = context.lock().unwrap();
    let graph = &mut context_lock.graph;
    let mapped_node = graph.node_weight(mapped_node_index);

    assert_eq!(
        TracedExprRec::Int(42),
        mapped_node.unwrap().current().evaluated.clone()
    );
}

#[test]
fn test_filter_node() {
    let on_update = |new_value| println!("New value is: {:?}", new_value);

    let is_traced = Arc::new(AtomicBool::new(false));

    // Setup our FRP graph with two nodes: An input node and a mapped node.

    let context =
        Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new(), BiMap::new())));

    let (node_index, filtered_node_index) =
        with_lock(context.as_ref(), |lock| {
            let node_index = Node::new(
                on_update,
                is_traced.clone(),
                &mut lock.graph,
                ExprType::Int,
                TracedExprRec::Int(1).traced(),
            );

            let filtered_node_index = filter_node(
                on_update,
                is_traced,
                &mut lock.graph,
                node_index,
                Box::new(move |x| match x.evaluated {
                    TracedExprRec::Int(i) => i % 2 != 0,
                    _ => true,
                }),
            );

            let node = lock.graph.node_weight(node_index).unwrap();

            node.update(TracedExprRec::Int(2).traced());

            // Check the initial state of the mapped node is what we expect
            (node_index, filtered_node_index)
        });

    with_lock(context.as_ref(), |lock| {
        let filtered_node =
            lock.graph.node_weight(filtered_node_index).unwrap();

        assert_eq!(
            TracedExprRec::Int(1),
            filtered_node.current().evaluated.clone()
        );
    });

    // Update the input value and step through a single frame of the event loop.
    process_event_frame(context.clone());

    with_lock(context.as_ref(), |lock| {
        // Check that the mapped node has not updated, since we are filtering out even values..

        let filtered_node =
            lock.graph.node_weight(filtered_node_index).unwrap();

        assert_eq!(
            TracedExprRec::Int(1),
            filtered_node.current().evaluated.clone()
        );

        let node = lock.graph.node_weight(node_index).unwrap();

        node.update(TracedExprRec::Int(3).traced());
    });

    process_event_frame(context.clone());

    // Check that the mapped node has updated, since now we have updated to an odd value.

    with_lock(context.as_ref(), |lock| {
        let filtered_node =
            lock.graph.node_weight(filtered_node_index).unwrap();

        assert_eq!(
            TracedExprRec::Int(3),
            filtered_node.current().evaluated.clone()
        );
    });
}

#[test]
fn test_combined_node() {
    let on_update = |new_value| println!("New value is: {:?}", new_value);

    let is_traced = Arc::new(AtomicBool::new(false));

    // Setup our FRP graph with three nodes: Two input nodes, and a combined output node.

    let context =
        Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new(), BiMap::new())));

    let mut context_lock = context.lock().unwrap();

    let first_node_index = Node::new(
        on_update,
        is_traced.clone(),
        &mut context_lock.graph,
        ExprType::Int,
        TracedExprRec::Int(2).traced(),
    );

    let second_node_index = Node::new(
        on_update,
        is_traced.clone(),
        &mut context_lock.graph,
        ExprType::Int,
        TracedExprRec::Int(3).traced(),
    );

    drop(context_lock);

    let cloned_ctx = context.clone();

    let combined_node_index = combined_node(
        on_update,
        is_traced,
        context.clone(),
        first_node_index,
        second_node_index,
        ExprType::Int,
        Box::new(move |x, y| {
            apply_traced(
                cloned_ctx.clone(),
                TracedExprRec::BuiltinFunction(INT_MUL_ID).traced(),
                &[x, y],
            )
            .unwrap()
        }),
    );

    let context_lock = context.lock().unwrap();

    let combined_node =
        &mut context_lock.graph.node_weight(combined_node_index).unwrap();

    // Check initial state

    assert_eq!(TracedExprRec::Int(6), combined_node.current().evaluated);

    // Check state after updating one of the fields

    let first_node =
        &mut context_lock.graph.node_weight(first_node_index).unwrap();
    first_node.update(TracedExprRec::Int(3).traced());

    drop(context_lock);

    process_event_frame(context.clone());

    let context_lock = context.lock().unwrap();

    let combined_node =
        &mut context_lock.graph.node_weight(combined_node_index).unwrap();

    assert_eq!(TracedExprRec::Int(9), combined_node.current().evaluated);
}

#[test]
fn test_fold_node() {
    let on_update = |new_value| println!("New value is: {:?}", new_value);

    let is_traced = Arc::new(AtomicBool::new(false));

    // Setup our FRP graph with an input node and a folded node

    let context =
        Arc::new(Mutex::new(ef3r_stdlib(NoOpDebugger::new(), BiMap::new())));

    let mut context_lock = context.lock().unwrap();

    let event_node_index = Node::new(
        on_update,
        is_traced.clone(),
        &mut context_lock.graph,
        ExprType::Any,
        TracedExprRec::None.traced(),
    );

    drop(context_lock);

    let folded_node_index = fold_node(
        context.clone(),
        on_update,
        is_traced,
        event_node_index,
        TracedExprRec::Int(2).traced(),
        Box::new(|acc: TracedExpr<u32>, event: TracedExpr<u32>| {
            match (acc.evaluated, event.evaluated) {
                (TracedExprRec::Int(a), TracedExprRec::Int(b)) => {
                    TracedExprRec::Int(a + b).traced()
                }
                _ => panic!("Expected integers"),
            }
        }),
    );

    let context_lock = context.lock().unwrap();

    // Verify initial state
    let folded_node =
        context_lock.graph.node_weight(folded_node_index).unwrap();
    assert_eq!(TracedExprRec::Int(2), folded_node.current().evaluated);

    // Update input value
    let event_node = context_lock.graph.node_weight(event_node_index).unwrap();
    event_node.update(TracedExprRec::Int(3).traced());

    drop(context_lock);

    process_event_frame(context.clone());

    // Verify folded value is updated
    let context_lock = context.lock().unwrap();
    let folded_node =
        context_lock.graph.node_weight(folded_node_index).unwrap();
    assert_eq!(TracedExprRec::Int(5), folded_node.current().evaluated);
}
