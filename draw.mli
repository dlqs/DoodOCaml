open Js_of_ocaml
open Object

(* Renders a given object on the canvas *)
val render :  collidable list -> Dom_html.canvasRenderingContext2D Js.t -> unit

(* Clears the canvas *)
val clear_canvas : Dom_html.canvasElement Js.t -> unit
