open Actors
open Object

let layer_width = 22

(*Chooses what type of enemy should be instantiated given typ number*)
let choose_tile_typ (typ:int) : tile_typ =
  match typ with
  |0 -> Green
  |_ -> failwith "Invalid tile type"

(*Main function called to procedurally generate the level map. w and h args
* are in pixel form. Converts to block form to call generate_helper. Spawns
* the list of collidables received from generate_helper to display on canvas.*)

let generate (bot_left: xy) (top_right: xy) : obj_prefab list =
  let rec generate_layer botY topY acc =
    if botY >= topY then acc else
      let generated = begin
        let p = Random.int 2 in
        let tile_prob = 1 in
        let actor_type = Actors.ATile(Green) in
        let prefab = (actor_type, { x = Random.int top_right.x ; y = botY }) in
        if p < tile_prob then
          [prefab]
        else
          []
        end
      in
      generate_layer (botY + layer_width) topY generated@acc
  in
  generate_layer bot_left.y top_right.y []

let generate_debug =
  [
    (Actors.ATile(Green), { x = 0; y = 0 });
    (Actors.ATile(Green), { x = 50; y = 0 })
  ]
