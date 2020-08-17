open Types

let blockY = 50
let tile_width = 40
let tile_height = 10
let layer_height = 20

(* tile probabilities *)
type tile_prob = {g:int; b:int; w:int; y:int}
(* item probabilities *)
type item_prob = {r:int; s:int; m:int;}

let get_tile_prob ?g:(g=0) ?b:(b=0) ?w:(w=0) ?y:(y=0) () =
  assert (g+b+w+y = 100);
  { g;b;w;y }

let get_item_prob ?r:(r=0) ?s:(s=0) ?m:(m=0) () =
  assert (r+s+m = 100);
  { r;s;m; }

let choose_item_typ (ip:item_prob) : item_typ =
  let item_prob = 1 + Random.int 100 in
  let rc = ip.r in
  let sc = rc + ip.s in
  let mc = sc + ip.m in
  match item_prob with
  | prob when prob <= rc -> Rocket
  | prob when prob <= sc -> Spring
  | prob when prob <= mc -> Monster
  | _ -> failwith "does not add up to 100!"

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

let rec generate_tiles state startY endY tile_probs generated_tiles =
  if startY + blockY >= endY then generated_tiles else
  let block = generate_block state startY (startY+blockY) tile_probs in
  generate_tiles state (startY+blockY) endY tile_probs generated_tiles@block

let rec place_items state tiles ip items =
  match tiles with
  | [] -> items
  | Tile(tt, _, t_o)::rest_tiles ->
      if (tt != Green) then place_items state rest_tiles ip items else
      let p = Random.float 1.0 in
      let q = int_of_float (100.0 *. p) in
      if q <= 20 then
      let tile_pos = t_o.pos in
      let pos = { tile_pos with y = tile_pos.y + tile_height } in
      let it = choose_item_typ ip in
      let item = Object.make_item it pos state.time in
      place_items state rest_tiles ip (item::items)
    else
      place_items state rest_tiles ip items
  | _ -> failwith ("cannot place items on else")

let generate (state:state) : collidable list =
  let startY = state.last_generated_height + 1 in
  let endY = state.next_generated_height in
  if endY < 1500 then
    let tp = (get_tile_prob ~g:90 ~b:10 ()) in
    let ip = (get_item_prob ~r:20 ~s:60 ~m:20 ()) in
    let tiles = generate_tiles state startY endY tp [] in
    let items = place_items state tiles ip [] in
    tiles@items
  else if endY < 3000 then
    let tp = (get_tile_prob ~g:80 ~b:20 ()) in
    let ip = (get_item_prob ~r:30 ~s:50 ~m:20 ()) in
    let tiles = generate_tiles state startY endY tp [] in
    let items = place_items state tiles ip [] in
    tiles@items
  else if endY < 4500 then
    let tp = (get_tile_prob ~g:25 ~b:25 ~w:25 ~y:25 ()) in
    let ip = (get_item_prob ~r:20 ~s:50 ~m:30 ()) in
    let tiles = generate_tiles state startY endY tp [] in
    let items = place_items state tiles ip [] in
    tiles@items
  else if endY < 6000 then
    let tp = (get_tile_prob ~g:20 ~b:20 ~w:30 ~y:30 ()) in
    let ip = (get_item_prob ~r:20 ~s:50 ~m:30 ()) in
    let tiles = generate_tiles state startY endY tp [] in
    let items = place_items state tiles ip [] in
    tiles@items
  else
    let tp = (get_tile_prob ~g:15 ~b:15 ~w:35 ~y:35 ()) in
    let ip = (get_item_prob ~r:20 ~s:50 ~m:30 ()) in
    let tiles = generate_tiles state startY endY tp [] in
    let items = place_items state tiles ip [] in
    tiles@items

let generate_debug =
  [
    Object.make_tile Green { x = 40; y = 20; } 0.;
    Object.make_tile Green { x = 120; y = 120; } 0.;
    Object.make_tile Green { x = 160; y = 160; } 0.;
    Object.make_tile Green { x = 160; y = 160; } 0.;
    Object.make_item Monster { x = 160; y = 170; } 0.;
  ]
