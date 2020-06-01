open Types

val make_veh : veh_typ -> veh_dir -> sprite

val make_item : item_typ -> sprite

val make_obst : obst_typ -> sprite

(* Updates the sprite's animation *)
val update_animation : sprite -> unit

val update_max_ticks : sprite -> float -> unit

