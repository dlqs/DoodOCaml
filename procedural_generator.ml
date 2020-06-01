open Types

let generate (state:state) : collidable list =
  []

let generate_debug =
  [
    Object.make_obst Barrier { x = 40; y = 40; };
    Object.make_item Health { x = 40; y = 80; };
  ]
