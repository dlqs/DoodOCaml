open Js_of_ocaml

(* Represents an xy vector *)
type pxy = int * int (* x, y *)

(* Inherent sprite parameters from which to create the sprite *)
type sprite_params =
  {
    max_frames: int;
    max_ticks: int;
    img_src: string;
    frame_size: pxy;
    src_offset: pxy;
    bbox_offset: pxy;
    bbox_size: pxy;
    loop: bool;
  }

(* Concrete sprite created to visually represent an object *)
type sprite = 
  {
    mutable params: sprite_params;
    frame: int ref;
    ticks: int ref;
    img: Dom_html.imageElement Js.t;
  }

val setup : Dom_html.canvasRenderingContext2D Js.t -> unit

(* Sets up a sprite to create *)
val setup_sprite : ?loop:bool -> ?bb_off:int*int-> ?bb_sz:int*int 
        -> string -> int -> int -> pxy -> pxy 
                          -> sprite_params 

val get_s_frame_size: Actors.actor_typ -> pxy

(* Creates a sprite given the actor type *)
val make : Actors.actor_typ -> sprite

(* Make a background *)
val make_bgd : Dom_html.canvasRenderingContext2D Js.t -> sprite

(* Updates the sprite's animation *)
val update_animation : sprite -> unit

