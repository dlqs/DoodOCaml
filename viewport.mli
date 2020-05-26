open Object
   
type viewport = {
  pos: Object.xy;     (* Absolute position of viewport relative to map *)
  v_dim: Object.xy;   (* Dimensions of viewport *)
}

(* Makes a new viewport of viewport dimensions and map dimensions*)
val make : int*int -> viewport

val filter_into_view: viewport -> collidable list -> collidable list

val move: viewport -> collidable -> viewport
