open Actors

(*Chooses what type of enemy should be instantiated given typ number*)
let choose_tile_typ (typ:int) : tile_typ =
  match typ with
  |0 -> Green
  |_ -> failwith "Invalid tile type"

(*Main function called to procedurally generate the level map. w and h args
* are in pixel form. Converts to block form to call generate_helper. Spawns
* the list of collidables received from generate_helper to display on canvas.
let generate_initial (w:float) (base:float)
                    (context:Dom_html.canvasRenderingContext2D Js.t) :
                    (collidable * collidable list) =
  Random.self_init();
  let obj_converted_ground_blocks = [] in
  let player = Object.spawn (SPlayer(SmallM,Standing)) context (w /. 2., base) in
  (player, obj_converted_ground_blocks)*)

let generate (bot_left:Object.xy) (top_right:Object.xy) : Object.obj_prefab list =
  let rec generate_helper x y limitX limitY acc =
    if y >= limitY then acc else
    if x >= limitX then generate_helper 0 (y + 1) limitX limitY acc else
    let open Object in
    let tile = (Actors.ATile(Green), { x; y }) in
    let w = snd (Sprite.get_s_frame_size (fst tile)) in
    generate_helper (x + w) y limitX limitY (tile::acc)
  in
  generate_helper bot_left.x bot_left.y top_right.x top_right.y []

