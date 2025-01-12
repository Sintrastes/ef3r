use macroquad::prelude::*;
use node_visualizer_server::NodeVisualizer;
use parking_lot::RwLock;
use std::{future::Future, pin::Pin, sync::Arc};
use tonic::{Request, Response, Status};

tonic::include_proto!("debugger");

#[derive(Clone)]
pub struct NodeVisualizerState {
    pub vertices: Arc<RwLock<Vec<VisualNode>>>,
    pub nodes_added: Arc<RwLock<i32>>,
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

            let nodes_added = self.nodes_added.read().clone() as f32;

            for node in nodes {
                let mut vertex_writer = self.vertices.write();

                vertex_writer.push(VisualNode {
                    id: node.id,
                    label: node.label.to_string(),
                    // TODO: Need an algorithm for determing where to place new nodes.
                    position: (200.0 * nodes_added, 200.0 * nodes_added),
                });

                *self.nodes_added.write() += 1;
            }

            Ok(Response::new(AddNodesResponse {}))
        })
    }

    fn add_edges<'a, 'b>(
        &'a self,
        _request: Request<AddEdgesRequest>,
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
                    .iter_mut()
                    .find(|v| v.id == update.node_id)
                {
                    vertex.label = update.value.to_string();
                }
            }

            Ok(Response::new(UpdateNodeValuesResponse {}))
        })
    }

    fn begin_session<'life0, 'async_trait>(
        &'life0 self,
        _request: tonic::Request<BeginSessionRequest>,
    ) -> ::core::pin::Pin<
        Box<
            dyn ::core::future::Future<
                    Output = std::result::Result<
                        tonic::Response<()>,
                        tonic::Status,
                    >,
                > + ::core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        todo!()
    }

    fn end_session<'life0, 'async_trait>(
        &'life0 self,
        _request: tonic::Request<EndSessionRequest>,
    ) -> ::core::pin::Pin<
        Box<
            dyn ::core::future::Future<
                    Output = std::result::Result<
                        tonic::Response<()>,
                        tonic::Status,
                    >,
                > + ::core::marker::Send
                + 'async_trait,
        >,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        todo!()
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

        clear_background(WHITE);

        // For debugging purposes, we can place vertices by right-clicking.
        if is_mouse_button_released(MouseButton::Right) {
            let mut writer = state.vertices.write();
            let vertices_mut: &mut Vec<VisualNode> = writer.as_mut();

            let position = camera.screen_to_world(mouse_position().into());

            vertices_mut.push(VisualNode {
                id: 0,
                label: "test".to_string(),
                position: (position.x, position.y),
            });
        }

        camera_control(&mut camera_x, &mut camera_y, aspect_ratio);

        scroll_handler(&mut zoom);

        set_camera(&camera);

        let vertices = state.vertices.read();

        for vertex in vertices.iter() {
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
