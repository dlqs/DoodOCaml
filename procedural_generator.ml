open Types

let blockY = 40
let tile_width = 40
let tile_height = 10               


let generate_tiles
          ?greenP:(gp=0) ?blueP:(bp=0) ?whiteP:(wp=0) ?yellowP:(yp=0) ?density:(ds=50)
          state startY endY generated_tiles : collidable list =
  let randomly_choose_tile () =
    let tile_prob = 1 + Random.int 100 in
    match tile_prob with
    | prob when prob <= gp -> Green
    | prob when prob <= gp+bp -> Blue
    | prob when prob <= gp+bp+wp -> White
    | prob when prob <= gp+bp+wp+yp -> Yellow
    | _ -> failwith "does not add up to 100!"
  in                                            
  (* Helper to generate one layer of tiles*)
  let max_x = state.vpt.dim.x - tile_width in
  let max_gap = (30*max_x)/ds in
  let rec generate_tile_layer x y generated_tiles =
    if x = -1
      then generate_tile_layer (Random.int max_gap) y generated_tiles else
    if x >= max_x
      then generated_tiles else
    let tt = randomly_choose_tile () in
    let tile = Object.make_tile tt { x; y; } state.time in
    generate_tile_layer (x + tile_width + Random.int 200) y (tile::generated_tiles)
  in
  (* Helper to generate one block (i.e. a few layers) of tiles, 
     but guaranteed to be at least one tile per block *)
  let rec generate_tile_layers startY endY generated_tiles =
    if startY + tile_height >= endY then generated_tiles else
    let one_layer = generate_tile_layer (-1) startY [] in
    generate_tile_layers (startY + 2*tile_height + Random.int 30) endY one_layer@generated_tiles
  in
  (* actual *)
  if startY >= endY then generated_tiles else
  let first_layer = generate_tile_layer (-1) startY [] in
  let rest_of_block = generate_tile_layers (startY + tile_height) endY [] in
  first_layer@rest_of_block

let generate (state:state) : collidable list =
  let startY = state.last_generated_height + 1 in
  let endY = state.next_generated_height in
  generate_tiles ~greenP:100 ~density:100 state startY endY []

let generate_debug =
  [
    Object.make_tile Green { x = 0; y = 0; } 0.;
    Object.make_tile Green { x = 216; y = 0; } 0.;
    Object.make_tile Blue { x = 0; y = 10; } 0.;
    Object.make_tile White { x = 40; y = 20; } 0.;
    Object.make_tile Yellow { x = 80; y = 20; } 0.;
  ]
