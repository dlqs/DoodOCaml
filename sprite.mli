open Js_of_ocaml

(* Represents an xy vector *)
type xy = int * int (* x, y *)

(* Inherent sprite parameters from which to create the sprite *)
type sprite_params =
  {
   max_frames: int;
    max_ticks: int;
    img_src: string;
    frame_size: xy;
    src_offset: xy;
    bbox_offset: xy;
    bbox_size: xy;
    loop: bool;
  }

(* Concrete sprite created to visually represent an object *)
type sprite = 
  {
    mutable params: sprite_params;
    context: Dom_html.canvasRenderingContext2D Js.t; 
    frame: int ref;
    ticks: int ref;
    mutable img: Dom_html.imageElement Js.t;
  }


(* Sets up a sprite to create *)
val setup_sprite : ?loop:bool -> ?bb_off:float*float-> ?bb_sz:float*float 
        -> string -> int -> int -> xy -> xy 
                          -> sprite_params 

val get_s_frame_size: Actors.spawn_typ -> xy

(* Creates a sprite given the actor type *)
val make : Actors.spawn_typ -> Actors.dir_1d 
   -> Dom_html.canvasRenderingContext2D Js.t
   -> sprite

(* Make a background *)
val make_bgd : Dom_html.canvasRenderingContext2D Js.t  -> sprite

(* Updates the sprite's animation *)
val update_animation : sprite -> unit

