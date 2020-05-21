open Js_of_ocaml
open Sprite
open Object

(* Represents the values of relevant key bindings. *)
type keys = {
  mutable left: bool;
  mutable right: bool;
  mutable up: bool;
  mutable down: bool;
  mutable bbox: int;
}

(*st represents the state of the game. It includes a background sprite (e.g.,
 * (e.g., hills), a context (used for rendering onto the page), a viewport
 * (used for moving the player's "camera"), a score (which is kept track
 * throughout the game), coins (also kept track through the game),
 * a multiplier (used for when you kill multiple enemies before ever touching
 * the ground, as in the actual Super Mario), and a game_over bool (which
 * is only true when the game is over). *)
type state = {
  bgd: sprite option;
  ctx: Dom_html.canvasRenderingContext2D Js.t;
  collids: collidable list;
  vpt: Viewport.viewport;
  score: int;
  game_over: bool;
}

(*pressed_keys instantiates the keys.*)
let pressed_keys = {
  left = false;
  right = false;
  up = false;
  down = false;
  bbox = 0;
}

(*let draw canvas vpt collids =
  Draw.clear_canvas canvas;
  let context = canvas##getContext (Dom_html._2d_) in
  let cw = float_of_int canvas##.width in
  let ch = float_of_int canvas##.height in*)

let setup canvas =
  let ctx = canvas##getContext (Dom_html._2d_) in
  let vpt = Viewport.make (canvas##.width, canvas##.height) in
  ignore(Sprite.setup ctx);
  let collids = Procedural_generator.generate {x = 0; y = 0} { x = canvas##.width; y = canvas##.height}
                |> Object.make_all 
  in
  {
    bgd = None;
    ctx;
    collids = collids;
    vpt = vpt;
    score = 0;
    game_over = false;
  }

let start canvas =
  let rec game_loop time state = begin
      (*draw canvas state.vpt state.collids;*)
      let collids = state.collids in
      let next_state = state in
      ignore (Dom_html.window##requestAnimationFrame 
                (Js.wrap_callback (fun (t:float) ->
                     game_loop t state));)
    end in
  game_loop 0. (setup canvas)

(* Keydown event handler translates a key press *)
let keydown evt =
  let () = match evt##.keyCode with
  | 38 | 32 | 87 -> pressed_keys.up <- true
  | 39 | 68 -> pressed_keys.right <- true
  | 37 | 65 -> pressed_keys.left <- true
  | 40 | 83 -> pressed_keys.down <- true
  | 66 -> pressed_keys.bbox <- (pressed_keys.bbox + 1) mod 2
  | _ -> ()
  in Js._true

(* Keyup event handler translates a key release *)
let keyup evt =
  let () = match evt##.keyCode with
  | 38 | 32 | 87 -> pressed_keys.up <- false
  | 39 | 68 -> pressed_keys.right <- false
  | 37 | 65 -> pressed_keys.left <- false
  | 40 | 83 -> pressed_keys.down <- false
  | _ -> ()
  in Js._true
