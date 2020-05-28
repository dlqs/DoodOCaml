open Types

(* Makes a new viewport of viewport dimensions and map dimensions*)
val make : int*int -> viewport

val below_vpt : viewport -> collidable -> bool

val above_vpt : viewport -> collidable -> bool

val in_vpt : viewport -> collidable -> bool

val translate_for_draw : viewport -> collidable -> collidable

val move: viewport -> collidable -> viewport
