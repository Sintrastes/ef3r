use std::sync::{atomic::AtomicBool, Arc};

use ef3r::{
    ast::{Expr, TracedExpr},
    frp::{event_loop, map_node, process_event_frame, Node},
    interpreter::apply_traced,
    stdlib::{ef3r_stdlib, MUL_ID},
};

#[test]
fn test_map_node() {
    let on_update = |new_value| println!("New value is: {:?}", new_value);

    let is_traced = Arc::new(AtomicBool::new(false));

    // Setup our FRP graph with two nodes: An input node and a mapped node.

    let node = Node::new(
        on_update,
        is_traced.clone(),
        TracedExpr {
            evaluated: Expr::Int(20),
            trace: None,
        },
    );

    let mut context = ef3r_stdlib();

    let graph = &mut context.graph;

    let node_index = context.graph.add_node(node);

    let node = context.graph.node_weight(node_index).unwrap();

    let mapped_context = context.expressionContext.clone();

    let mapped_node = map_node(
        on_update,
        is_traced,
        node.value.read().unwrap().clone(),
        node_index,
        Box::new(move |x| {
            apply_traced(
                &mapped_context,
                Expr::BuiltinFunction(MUL_ID).traced(),
                &[Expr::Int(2).traced(), x],
            )
        }),
    );

    let mapped_node_index = context.graph.add_node(mapped_node);

    context
        .graph
        .add_edge(node_index, mapped_node_index, ())
        .unwrap();

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
