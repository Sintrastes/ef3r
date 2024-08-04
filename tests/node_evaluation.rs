use std::sync::{atomic::AtomicBool, Arc};

use ef3r::{
    ast::{Expr, TracedExpr},
    frp::{combined_node, filter_node, map_node, process_event_frame, Node},
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

    let mapped_context = context.expression_context.clone();

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
            .unwrap()
        }),
    );

    let node = context.graph.node_weight(node_index).unwrap();

    node.update(Expr::Int(21).traced());

    // Check the initial state of the mapped node is what we expect

    let mapped_node = context.graph.node_weight(mapped_node_index).unwrap();

    assert_eq!(Expr::Int(40), mapped_node.current().evaluated.clone());

    // Update the input value and step through a single frame of the event loop.

    process_event_frame(&context);

    // Check that the mapped node has updated.

    assert_eq!(Expr::Int(42), mapped_node.current().evaluated.clone());
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

    assert_eq!(Expr::Int(1), filtered_node.current().evaluated.clone());

    // Update the input value and step through a single frame of the event loop.

    process_event_frame(&context);

    // Check that the mapped node has not updated, since we are filtering out even values..

    assert_eq!(Expr::Int(1), filtered_node.current().evaluated.clone());

    node.update(Expr::Int(3).traced());

    process_event_frame(&context);

    // Check that the mapped node has updated, since now we have updated to an odd value.

    assert_eq!(Expr::Int(3), filtered_node.current().evaluated.clone());
}

#[test]
fn test_combined_node() {
    let on_update = |new_value| println!("New value is: {:?}", new_value);

    let is_traced = Arc::new(AtomicBool::new(false));

    // Setup our FRP graph with three nodes: Two input nodes, and a combined output node.

    let mut context = ef3r_stdlib();

    let first_node_index = Node::new(
        on_update,
        is_traced.clone(),
        &mut context.graph,
        TracedExpr {
            evaluated: Expr::Int(2),
            trace: None,
        },
    );

    let second_node_index = Node::new(
        on_update,
        is_traced.clone(),
        &mut context.graph,
        TracedExpr {
            evaluated: Expr::Int(3),
            trace: None,
        },
    );

    let expr_ctx = context.expression_context.clone();

    let combined_node_index = combined_node(
        on_update,
        is_traced,
        &mut context.graph,
        first_node_index,
        second_node_index,
        Box::new(move |x, y| {
            apply_traced(
                &expr_ctx,
                Expr::BuiltinFunction(MUL_ID).traced(),
                &[x, y],
            )
            .unwrap()
        }),
    );

    let combined_node =
        &mut context.graph.node_weight(combined_node_index).unwrap();

    // Check initial state

    assert_eq!(Expr::Int(6), combined_node.current().evaluated);

    // Check state after updating one of the fields

    let first_node = &mut context.graph.node_weight(first_node_index).unwrap();
    first_node.update(Expr::Int(3).traced());

    process_event_frame(&context);

    assert_eq!(Expr::Int(9), combined_node.current().evaluated);
}
