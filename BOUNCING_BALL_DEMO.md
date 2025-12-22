# Bouncing Ball Demo

This demo showcases the Anochi programming language running a simple bouncing ball physics simulation using the RayLib graphics backend.

## Features

- **Foreign Function Interface**: Demonstrates calling Rust functions (RayLib) from Anochi code
- **Game Loop**: Shows how to create a continuous game loop with Anochi's `loop` construct
- **Physics Simulation**: Simple velocity-based movement with boundary collision detection
- **Graphics**: Real-time rendering using RayLib's drawing functions

## How to Run

```bash
cargo run --bin bouncing_ball --release
```

## The Anochi Code

The demo uses the following Anochi foreign functions provided by the RayLib backend:

- `window_should_close()` - Returns true when the user closes the window
- `begin_drawing()` - Starts a new frame
- `end_drawing()` - Finishes the current frame
- `clear_background()` - Clears the screen with white color
- `draw_circle(x, y, radius)` - Draws a red circle at the given position
- `draw_fps()` - Displays the current FPS counter

### Code Structure

1. **Setup**: Initialize ball position, velocity, and screen dimensions
2. **Game Loop**: 
   - Update ball position based on velocity
   - Check for wall collisions and bounce (reverse velocity)
   - Render the ball on screen
   - Display FPS counter

The entire physics and rendering logic is written in Anochi, demonstrating the language's capability to handle real-time interactive applications!
