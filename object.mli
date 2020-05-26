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

type aabb = {
    pos: xy;
    dim: xy;
  }

(* Holds type and position of object yet-to-be instantiated: used by procedural generator *)
type obj_prefab = actor_typ * xy

type obj_state = {
    id: int;
    has_gravity: bool;
    has_friction: bool;
    pos: xy;
    vel: fxy;
    debug_pt: xy option;
  }

(* Holds an instantiated object (also because object is a reserved word) *)
type collidable =
  | Player of pl_typ * Sprite.sprite * obj_state
  | Tile of tile_typ * Sprite.sprite * obj_state

val get_obj: collidable -> obj_state

val get_sprite: collidable -> Sprite.sprite

val get_aabb_center: collidable -> xy

val update: ?spr:Sprite.sprite -> ?pos:xy -> ?vel:fxy -> ?debug_pt:xy option -> collidable -> collidable

val update_player: int -> collidable list -> Actors.controls list -> collidable -> collidable

val make: Sprite.imgMap_t -> obj_prefab -> collidable 

val make_all: Sprite.imgMap_t -> obj_prefab list -> collidable list

val move : int -> collidable -> collidable

val update_collid : int -> collidable -> collidable
