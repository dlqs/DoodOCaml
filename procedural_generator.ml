open Types

let layer_width = 22

let generate (state:state) : collidable list =
  [
    
  ]

let generate_debug =
  [
    Object.make_tile Green { x = 0; y = 0; } 0.;
    Object.make_tile Green { x = 216; y = 0; } 0.;
    Object.make_tile Blue { x = 0; y = 10; } 0.;
    Object.make_tile White { x = 40; y = 20; } 0.;
    Object.make_tile Yellow { x = 80; y = 20; } 0.;
  ]
