open Js_of_ocaml
open Object

type obj_coord

val init: unit -> unit

val load: int -> Dom_html.canvasRenderingContext2D Js.t -> collidable * collidable list * float * float
