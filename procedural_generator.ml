open Types

let blockY = 50
let tile_width = 40
let layer_height = 20

let gp = 60
let bp = 40
let wp = 0
let yp = 0
       

let choose_tile_typ () =
  let tile_prob = 1 + Random.int 100 in
  match tile_prob with
  | prob when prob <= gp -> Green
  | prob when prob <= gp+bp -> Blue
  | prob when prob <= gp+bp+wp -> White
  | prob when prob <= gp+bp+wp+yp -> Yellow
  | _ -> failwith "does not add up to 100!"

let generate_tile pos created_at =
  let tt = choose_tile_typ () in
  Object.make_tile tt pos created_at

let generate_layer state tile_prob y =
  let p = Random.float 1.0 in
  let q = int_of_float (100.0 *. p) in
  if q <= tile_prob then
    let x = int_of_float (p *. float_of_int (state.vpt.dim.x - tile_width)) in
    [generate_tile { x; y; } state.time]
  else 
    []

let generate_block state startY endY =
  let rec helper startY endY generated_tiles =
    if startY + layer_height >= endY then generated_tiles else
    let layer = generate_layer state 20 startY in
    helper (startY+layer_height) endY generated_tiles@layer
  in
  let first_layer = generate_layer state 100 startY in
  let rest_layers = helper (startY+layer_height) endY [] in
  first_layer@rest_layers
  
let rec generate_green_blue_tiles state startY endY generated_tiles =
  if startY + blockY >= endY then generated_tiles else
  let block = generate_block state startY (startY+blockY) in
  generate_green_blue_tiles state (startY+blockY) endY generated_tiles@block

let generate (state:state) : collidable list =
  let startY = state.last_generated_height + 1 in
  let endY = state.next_generated_height in
  generate_green_blue_tiles state startY endY []

let generate_debug =
  [
    Object.make_tile Green { x = 0; y = 0; } 0.;
    Object.make_tile Green { x = 216; y = 0; } 0.;
    Object.make_tile Blue { x = 0; y = 10; } 0.;
    Object.make_tile White { x = 40; y = 20; } 0.;
    Object.make_tile Yellow { x = 80; y = 20; } 0.;
  ]
