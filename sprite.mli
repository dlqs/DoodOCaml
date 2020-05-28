open Types

(* Sets up a sprite to create *)
val setup_sprite : ?loop:bool -> ?bb_off:int*int-> ?bb_sz:int*int 
        -> string -> int -> int -> pxy -> pxy 
                          -> sprite_params 

(* Creates a sprite given the actor type *)
val make : sprite_typ -> sprite

(* Updates the sprite's animation *)
val update_animation : sprite -> unit

