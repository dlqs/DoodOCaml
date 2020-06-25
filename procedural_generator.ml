open Types

let blockY = 50
let tile_width = 40
let layer_height = 20

(* tile probabilities *)
type tile_prob = {g:int; b:int; w:int; y:int}

let get_tile_prob ?g:(g=0) ?b:(b=0) ?w:(w=0) ?y:(y=0) () =
  assert (g+b+w+y = 100);
  { g;b;w;y }

let choose_tile_typ (tp:tile_prob) : tile_typ =
  let tile_prob = 1 + Random.int 100 in
  let gc = tp.g in
  let bc = gc + tp.b in
  let wc = bc + tp.w in
  let yc = wc + tp.y in
  match tile_prob with
  | prob when prob <= gc -> Green
  | prob when prob <= bc -> Blue
  | prob when prob <= wc -> White
  | prob when prob <= yc -> Yellow
  | _ -> failwith "does not add up to 100!"

let generate_tile pos created_at tile_probs =
  let tt = choose_tile_typ tile_probs in
  Object.make_tile tt pos created_at

let generate_layer state tile_prob y tile_probs =
  let p = Random.float 1.0 in
  let q = int_of_float (100.0 *. p) in
  if q <= tile_prob then
    let x = int_of_float (p *. float_of_int (state.vpt.dim.x - tile_width)) in
    [generate_tile { x; y; } state.time tile_probs]
  else
    []

let generate_block state startY endY tile_probs =
  let rec helper startY endY generated_tiles =
    if startY + layer_height >= endY then generated_tiles else
    let layer = generate_layer state 20 startY tile_probs in
    helper (startY+layer_height) endY generated_tiles@layer
  in
  let first_layer = generate_layer state 100 startY tile_probs in
  let rest_layers = helper (startY+layer_height) endY [] in
  first_layer@rest_layers

let rec generate_green_blue_tiles state startY endY tile_probs generated_tiles =
  if startY + blockY >= endY then generated_tiles else
  let block = generate_block state startY (startY+blockY) tile_probs in
  generate_green_blue_tiles state (startY+blockY) endY tile_probs generated_tiles@block

let generate (state:state) : collidable list =
  let startY = state.last_generated_height + 1 in
  let endY = state.next_generated_height in
  if endY < 1025 then
    let tp = (get_tile_prob ~g:90 ~b:10 ()) in
    generate_green_blue_tiles state startY endY tp []
  else if endY < 2049 then
    let tp = (get_tile_prob ~g:80 ~b:20 ()) in
    generate_green_blue_tiles state startY endY tp []
  else
    let tp = (get_tile_prob ~g:25 ~b:25 ~w:25 ~y:25 ()) in
    generate_green_blue_tiles state startY endY tp []

let generate_debug =
  [
    Object.make_tile Green { x = 40; y = 20; } 0.;
    Object.make_tile Green { x = 120; y = 120; } 0.;
    Object.make_tile Green { x = 160; y = 160; } 0.;
    Object.make_tile Green { x = 160; y = 160; } 0.;
    Object.make_item Monster { x = 160; y = 170; } 0.;
  ]
