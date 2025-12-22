use anochi::code_runner::CodeRunner;
use anochi::vm::tree_walk::raylib_backend::RayLibBackend;

fn main() {
    // Create a code runner with RayLib backend
    let mut runner = CodeRunner::new(RayLibBackend::new());
    
    // Initialize the backend (opens window and registers foreign functions)
    runner.initialize().expect("Failed to initialize RayLib backend");
    
    println!("RayLib window opened! Running bouncing ball simulation...");
    
    // Setup code: Initialize ball position and velocity
    let setup_code = r#"
        let x = 320;
        let y = 240;
        let vx = 5;
        let vy = 3;
        let radius = 20;
        let width = 640;
        let height = 480;
    "#;
    
    runner.run_statements(setup_code).expect("Failed to run setup code");
    
    // Main loop code: update physics and render
    let loop_code = r#"
        loop {
            if (window_should_close!{}) {
                break;
            }
            
            x = x + vx;
            y = y + vy;
            
            if (x - radius < 0) {
                x = radius;
                vx = 0 - vx;
            }
            
            if (x + radius > width) {
                x = width - radius;
                vx = 0 - vx;
            }
            
            if (y - radius < 0) {
                y = radius;
                vy = 0 - vy;
            }
            
            if (y + radius > height) {
                y = height - radius;
                vy = 0 - vy;
            }
            
            begin_drawing!{};
            clear_background!{};
            draw_circle!{x, y, radius};
            draw_fps!{};
            end_drawing!{};
        }
    "#;
    
    runner.run_statements(loop_code).expect("Failed to run game loop");
    
    println!("Simulation ended!");
}
