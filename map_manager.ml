open Js_of_ocaml
open Object

(*Holds obj typ and its coordinates. (int, (x-coord, y-coord))*)
type obj_coord =  int * (float * float)

let generate_helper w h cw ch ctx = []

(*Main function called to procedurally generate the level map. w and h args
* are in pixel form. Converts to block form to call generate_helper. Spawns
* the list of collidables received from generate_helper to display on canvas.*)
let load (levelIdx:int) (context:Dom_html.canvasRenderingContext2D Js.t) :
                    (collidable * collidable list * float * float) =
  (*let blockw = w/.16. in
  let blockh = (h/.16.) -. 1. in*)
  let collide_list = generate_helper 0 0 0. 0. context in
  let player = Object.spawn (SPlayer(SmallM,Standing)) context (100.,224.) in
  let w = 16. in
  let h = 16. in
  (player, collide_list, w, h)

let init () =
  Random.self_init();
