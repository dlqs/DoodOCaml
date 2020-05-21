type viewport = {
  pos: Object.xy;     (* Absolute position of viewport relative to map *)
  v_dim: Object.xy;   (* Dimensions of viewport *)
}

(* Makes a new viewport of viewport dimensions and map dimensions*)
val make : int*int -> viewport
