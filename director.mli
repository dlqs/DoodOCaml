open Js_of_ocaml
(* Initiates the main game loop *)
val update_loop : Dom_html.canvasElement Js.t
                  -> unit

(* Keydown event handler function *)
val keydown : #Dom_html.keyboardEvent Js.t -> bool Js.t

(* Keyup event handler function *)
val keyup : #Dom_html.keyboardEvent Js.t -> bool Js.t
