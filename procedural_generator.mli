open Js_of_ocaml
open Object

type obj_coord

(* Procedurally generates a new map of default size*)
val generate_initial : float -> float -> Dom_html.canvasRenderingContext2D Js.t ->
               collidable * collidable list

val continually_generate : float -> float -> float -> Dom_html.canvasRenderingContext2D Js.t ->
               collidable list
