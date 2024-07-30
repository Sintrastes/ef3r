
use std::sync::Arc;

/// A node in the graph of functional reactive values.
/// 
/// This is a directed acyclic graph used to determine
///  how updates are propogated throughout the network.
///  
pub struct Node<T : Send> {
    id: i64,
    value: Arc<T>,
    derived_nodes: [i64] 
}