use macroquad::prelude::*;
use std::{
    borrow::BorrowMut,
    cell::RefCell,
    future::Future,
    pin::Pin,
    sync::{Arc, RwLock},
};

use node_visualizer_server::NodeVisualizer;
use tonic::{Request, Response, Status};

tonic::include_proto!("debugger");

#[derive(Clone)]
pub struct NodeVisualizerState {
    pub vertices: Arc<RwLock<Vec<VisualNode>>>,
}

impl NodeVisualizer for NodeVisualizerState {
    fn add_nodes<'a, 'b>(
        &'a self,
        request: Request<AddNodesRequest>,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<Response<AddNodesResponse>, Status>>
                + Send
                + 'b,
        >,
    >
    where
        'a: 'b,
        Self: 'b,
    {
        Box::pin(async move {
            let nodes = request.get_ref().nodes.iter();

            for node in nodes {
                self.vertices.write().unwrap().push(VisualNode {
                    id: node.id,
                    label: node.label.to_string(),
                    // TODO: Need an algorithm for determing where to place new nodes.
                    position: (0.0, 0.0),
                });
            }

            Ok(Response::new(AddNodesResponse {}))
        })
    }

    fn add_edges<'a, 'b>(
        &'a self,
        request: Request<AddEdgesRequest>,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<Response<AddEdgesResponse>, Status>>
                + Send
                + 'b,
        >,
    >
    where
        'a: 'b,
        Self: 'b,
    {
        todo!()
    }

    fn update_node_values<'a, 'b>(
        &'a self,
        request: Request<UpdateNodeValuesRequest>,
    ) -> Pin<
        Box<
            dyn Future<
                    Output = Result<Response<UpdateNodeValuesResponse>, Status>,
                > + Send
                + 'b,
        >,
    >
    where
        'a: 'b,
        Self: 'b,
    {
        Box::pin(async move {
            let updates = request.get_ref();

            for update in &updates.node_values {
                if let Some(vertex) = self
                    .vertices
                    .write()
                    .unwrap()
                    .iter_mut()
                    .find(|v| v.id == update.node_id)
                {
                    vertex.label = update.value.to_string();
                }
            }

            Ok(Response::new(UpdateNodeValuesResponse {}))
        })
    }
}

const VERTEX_SIZE: f32 = 32.0;
const VERTEX_COLOR: Color = BLACK;
const CAMERA_SPEED: f32 = 0.1;

pub struct VisualNode {
    pub position: (f32, f32),
    pub label: String,
    pub id: u64,
}

pub async fn node_visualization(state: NodeVisualizerState) {
    let mut camera_x = 0.0;
    let mut camera_y = 0.0;

    let mut zoom = 0.001;

    let aspect_ratio = screen_width() / screen_height();

    loop {
        let camera = Camera2D {
            target: vec2(camera_x, camera_y),
            offset: vec2(camera_x, camera_y),
            zoom: vec2(zoom, zoom * aspect_ratio),
            ..Default::default()
        };

        let mut vertices_guard = state.vertices.write().unwrap();

        let vertices: &mut Vec<VisualNode> = vertices_guard.as_mut();

        clear_background(WHITE);

        // For debugging purposes, we can place vertices by right-clicking.
        if is_mouse_button_released(MouseButton::Right) {
            let position = camera.screen_to_world(mouse_position().into());

            vertices.push(VisualNode {
                id: 0,
                label: "test".to_string(),
                position: (position.x, position.y),
            });
        }

        camera_control(&mut camera_x, &mut camera_y, aspect_ratio);

        scroll_handler(&mut zoom);

        set_camera(&camera);

        for vertex in vertices {
            draw_node(&vertex.label, vertex.position);
        }

        set_default_camera();

        draw_text("3f3r node debugger", 20.0, 20.0, 30.0, DARKGRAY);

        next_frame().await
    }
}

fn draw_node(name: &str, location: (f32, f32)) {
    draw_text(
        name,
        location.0 - 1.6 * VERTEX_SIZE,
        location.1 - 1.6 * VERTEX_SIZE,
        62.0,
        VERTEX_COLOR,
    );
    draw_circle(location.0, location.1, VERTEX_SIZE, VERTEX_COLOR);
}

fn camera_control(camera_x: &mut f32, camera_y: &mut f32, aspect_ratio: f32) {
    if is_key_down(KeyCode::Left) {
        *camera_x -= CAMERA_SPEED;
    }

    if is_key_down(KeyCode::Right) {
        *camera_x += CAMERA_SPEED;
    }

    if is_key_down(KeyCode::Up) {
        *camera_y -= CAMERA_SPEED / aspect_ratio;
    }

    if is_key_down(KeyCode::Down) {
        *camera_y += CAMERA_SPEED / aspect_ratio;
    }
}

fn scroll_handler(zoom: &mut f32) {
    match mouse_wheel() {
        (_x, y) if y != 0.0 => {
            // Normalize mouse wheel values is browser (chromium: 53, firefox: 3)
            #[cfg(target_arch = "wasm32")]
            let y = if y < 0.0 {
                -1.0
            } else if y > 0.0 {
                1.0
            } else {
                0.0
            };

            *zoom *= 1.1f32.powf(y);
        }
        _ => (),
    };
}
