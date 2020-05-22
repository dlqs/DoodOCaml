open Actors

type xy = {
  x: int;
  y: int;
}

type obj_prefab = actor_typ * xy

type obj_state = {
    id: int;
    has_gravity: bool;
    pos: xy;
    vel: xy;
  }

type collidable =
  | Player of pl_typ * pl_state * Sprite.sprite * obj_state
  | Tile of tile_typ * Sprite.sprite * obj_state

(*Variables*)
let gravity = 0.2
let player_jump = 5.7

let id_counter = ref min_int

(*Used in object creation and to compare two objects.*)
let new_id () =
  id_counter := !id_counter + 1;
  !id_counter

(*Helper methods for getting sprites and objects from their collidables*)
let get_sprite = function
  | Player (_,_,s,_) | Tile(_,s,_) -> s

let get_obj = function
  | Player (_,_,_,o) | Tile(_,_,o) -> o

let setup =
  {
    id = new_id();
    has_gravity = false;
    pos = {x = 0; y = 0};
    vel = {x = 0; y = 0};
  }

let setup_player pos =
  {
    setup with
    pos = pos
  }

let setup_tile pos =
  {
    setup with
    pos = pos
  }

let make op =
  let typ = fst op in
  let pos = snd op in
  match typ with
  | APlayer(plt, pls) -> Player(plt, pls, Sprite.make typ, setup_player pos)
  | ATile(_) -> Tile(Green, Sprite.make typ, setup_tile pos)

let rec make_all ops : collidable list =
  match ops with
  | [] -> []
  | h::t -> (make h)::make_all t

