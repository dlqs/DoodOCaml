open Types

val make_player : pl_typ -> xy -> float -> collidable

val make_tile : tile_typ -> xy -> float -> collidable

val make_item : item_typ -> xy -> float -> collidable

val get_obj : collidable -> obj_state

val get_sprite : collidable -> sprite

val get_aabb_center : collidable -> xy

val update : ?plt:pl_typ -> ?it:item_typ -> ?tt:tile_typ ->
             ?spr:sprite -> ?pos:xy -> ?vel:fxy -> ?debug_pt:xy option -> ?killed:bool -> ?created_at:float
             -> collidable -> collidable

val move : state -> collidable -> collidable

val update_player_collids : state -> controls list -> collidable -> collidable list -> collidable * collidable list

val update_collid : state -> collidable -> collidable

val update_collid_second : state -> collidable -> collidable
