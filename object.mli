open Actors

(* Generic container for pair of integers *)
type xy = {
  x: int;
  y: int;
}

type fxy = {
    fx: float;
    fy: float;
}

(* Holds type and position of object yet-to-be instantiated: used by procedural generator *)
type obj_prefab = actor_typ * xy

type obj_state = {
    id: int;
    has_gravity: bool;
    pos: xy;
    vel: fxy;
  }

(* Holds an instantiated object (also because object is a reserved word) *)
type collidable =
  | Player of pl_typ * Sprite.sprite * obj_state
  | Tile of tile_typ * Sprite.sprite * obj_state

val get_obj: collidable -> obj_state

val get_sprite: collidable -> Sprite.sprite

val move: obj_state -> obj_state

val move_all: collidable list -> collidable list

val make: Sprite.imgMap_t -> obj_prefab -> collidable 

val make_all: Sprite.imgMap_t -> obj_prefab list -> collidable list

val initial_make_player: Sprite.imgMap_t -> int -> int -> collidable
