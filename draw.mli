open Js_of_ocaml
open Types

(* Renders a given object on the canvas *)
val render : state -> Dom_html.canvasElement Js.t -> collidable list -> unit

val show_score : Dom_html.canvasElement Js.t -> state -> unit

val show_game_over : Dom_html.canvasElement Js.t -> state -> unit

(* Clears the canvas *)
val clear_canvas : Dom_html.canvasElement Js.t -> unit
