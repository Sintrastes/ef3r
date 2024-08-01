use std::sync::{atomic::AtomicBool, Arc};

use ef3r::{
    ast::{Expr, TracedExpr},
    frp::{event_loop, filter_node, map_node, process_event_frame, Node},
    interpreter::apply_traced,
    stdlib::{ef3r_stdlib, MUL_ID},
};

#[test]
fn test_map_node() {
    let on_update = |new_value| println!("New value is: {:?}", new_value);

    let is_traced = Arc::new(AtomicBool::new(false));

    // Setup our FRP graph with two nodes: An input node and a mapped node.

    let mut context = ef3r_stdlib();

    let graph = &mut context.graph;

    let node_index = Node::new(
        on_update,
        is_traced.clone(),
        graph,
        TracedExpr {
            evaluated: Expr::Int(20),
            trace: None,
        },
    );

    let mapped_context = context.expressionContext.clone();

    let mapped_node_index = map_node(
        on_update,
        is_traced,
        graph,
        node_index,
        Box::new(move |x| {
            apply_traced(
                &mapped_context,
                Expr::BuiltinFunction(MUL_ID).traced(),
                &[Expr::Int(2).traced(), x],
            )
        }),
    );

    let node = context.graph.node_weight(node_index).unwrap();

    node.update(Expr::Int(21).traced());

    // Check the initial state of the mapped node is what we expect

    let mapped_node = context.graph.node_weight(mapped_node_index).unwrap();

    let expected = Expr::Int(40);
    let actual = mapped_node.value.read().unwrap().evaluated.clone();

    assert!(
        expected == actual,
        "Expected {}, actual {}",
        expected,
        actual
    );

    // Update the input value and step through a single frame of the event loop.

    process_event_frame(&context);

    // Check that the mapped node has updated.

    let actual = mapped_node.value.read().unwrap().evaluated.clone();
    let expected = Expr::Int(42);

    assert!(
        expected == actual,
        "Expected {}, actual {}",
        expected,
        actual
    );
}

#[test]
fn test_filter_node() {
    let on_update = |new_value| println!("New value is: {:?}", new_value);

    let is_traced = Arc::new(AtomicBool::new(false));

    // Setup our FRP graph with two nodes: An input node and a mapped node.

    let mut context = ef3r_stdlib();

    let graph = &mut context.graph;

    let node_index = Node::new(
        on_update,
        is_traced.clone(),
        graph,
        TracedExpr {
            evaluated: Expr::Int(1),
            trace: None,
        },
    );

    let mapped_context = context.expressionContext.clone();

    let filtered_node_index = filter_node(
        on_update,
        is_traced,
        graph,
        node_index,
        Box::new(move |x| match x.evaluated {
            Expr::Int(i) => i % 2 != 0,
            _ => true,
        }),
    );

    let node = context.graph.node_weight(node_index).unwrap();

    node.update(Expr::Int(2).traced());

    // Check the initial state of the mapped node is what we expect

    let filtered_node = context.graph.node_weight(filtered_node_index).unwrap();

    let expected = Expr::Int(1);
    let actual = filtered_node.value.read().unwrap().evaluated.clone();

    assert!(
        expected == actual,
        "Expected {}, actual {}",
        expected,
        actual
    );

    // Update the input value and step through a single frame of the event loop.

    process_event_frame(&context);

    // Check that the mapped node has not updated, since we are filtering out even values..

    let actual = filtered_node.value.read().unwrap().evaluated.clone();
    let expected = Expr::Int(1);

    assert!(
        expected == actual,
        "Expected {}, actual {}",
        expected,
        actual
    );

    node.update(Expr::Int(3).traced());

    process_event_frame(&context);

    // Check that the mapped node has updated, since now we have updated to an odd value.

    let actual = filtered_node.value.read().unwrap().evaluated.clone();
    let expected = Expr::Int(3);

    assert!(
        expected == actual,
        "Expected {}, actual {}",
        expected,
        actual
    );
}
