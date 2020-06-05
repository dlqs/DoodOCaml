open Types

val make_veh : veh_typ -> veh_dir -> xy -> collidable

val make_obst : obst_typ -> xy -> collidable

val make_item : item_typ -> xy -> collidable

val get_obj : collidable -> obj_state

val get_sprite : collidable -> sprite

val get_aabb_center : collidable -> xy

val update : ?vt:veh_typ -> ?vd:veh_dir -> ?it:item_typ -> ?ot:obst_typ ->
             ?spr:sprite -> ?pos:xy -> ?vel:fxy -> ?debug_pt:xy option -> ?killed:bool -> ?created_at:float
             -> collidable -> collidable

val move : state -> collidable -> collidable

val update_player_keys : controls list -> collidable -> collidable

val check_collisions : state -> collidable list -> collidable list

val update_collid_second : state -> collidable -> collidable
