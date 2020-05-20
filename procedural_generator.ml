open Js_of_ocaml
open Actors
open Object
module Sprite = Sprite

(*Holds obj typ and its coordinates. (int, (x-coord, y-coord))*)
type obj_coord =  spawn_typ * (int * int)

(*Chooses what type of enemy should be instantiated given typ number*)
let choose_tile_typ (typ:int) : tile_typ =
  match typ with
  |0 -> Green
  |_ -> failwith "Invalid tile type"

(*Main function called to procedurally generate the level map. w and h args
* are in pixel form. Converts to block form to call generate_helper. Spawns
* the list of collidables received from generate_helper to display on canvas.*)
let generate_initial (w:float) (base:float)
                    (context:Dom_html.canvasRenderingContext2D Js.t) :
                    (collidable * collidable list) =
  Random.self_init();
  let obj_converted_ground_blocks = [] in
  let player = Object.spawn (SPlayer(SmallM,Standing)) context (w /. 2., base) in
  (player, obj_converted_ground_blocks)


let generate (bot_left:Actors.xy) (top_right:Actors.xy) : obj_coord list =
  let rec generate_helper x y limitX limitY acc =
    if y >= limitY then acc else
    if x >= limitX then generate_helper 0 (y + 1) limitX limitY acc else
    let tile = (ATile(Green), (x, y)) in
    let w = (Sprite.get_s_frame_size (fst tile)).x in
    generate_helper (x + w) y limitX limitY tile::acc
  in
  generate_helper bot_left.x bot_left.y top_right.x top_right.y []

