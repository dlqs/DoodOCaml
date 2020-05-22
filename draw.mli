open Js_of_ocaml
open Object

(* Renders a given object on the canvas *)
val render :  Dom_html.canvasElement Js.t -> collidable list -> unit

(* Clears the canvas *)
val clear_canvas : Dom_html.canvasElement Js.t -> unit
