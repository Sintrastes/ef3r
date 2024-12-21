use macroquad::prelude::*;

const VERTEX_SIZE: f32 = 0.03;
const VERTEX_COLOR: Color = BLACK;
const CAMERA_SPEED: f32 = 0.07;

pub async fn node_visualization() {
    let mut vertices: Vec<(f32, f32)> = vec![];

    let mut camera_x = 0.0;
    let mut camera_y = 0.0;

    let mut zoom = 1.0;

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
            let position = camera.screen_to_world(mouse_position().into());

            vertices.push((position.x, position.y));
        }

        camera_control(&mut camera_x, &mut camera_y, aspect_ratio);

        scroll_handler(&mut zoom);

        set_camera(&camera);

        for vertex in &mut vertices {
            draw_circle(vertex.0, vertex.1, VERTEX_SIZE, VERTEX_COLOR);
        }

        next_frame().await
    }
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
