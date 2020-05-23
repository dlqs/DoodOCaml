open Js_of_ocaml
open Sprite
open Object

type controls =
  | CLeft
  | CRight

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
  cw: int;
  ch: int;
  collids: collidable list;
  imgMap: Sprite.imgMap_t;
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

(* Converts a keypress to a list of control keys, allowing more than one key
 * to be processed each frame. *)
let translate_keys () =
  let k = pressed_keys in
  let ctrls = [(k.left,CLeft);(k.right,CRight);] in
  List.fold_left (fun a x -> if fst x then (snd x)::a else a) [] ctrls

let filter_collisions collids =
  []

let move_collid collid =
  match collid with
  | Player(plt, s, o) ->
     print_endline ("player pos:"^(string_of_int o.pos.x)^" "^(string_of_int o.pos.y));
     Player(plt, s, Object.move o)
  | Tile(tt, s, o) -> Tile(tt, s, Object.move o)

let update_collid state collid collids =
  let obj = Object.get_obj collid in
  move_collid collid

let run_update_collid state collid =
  match collid with
  | Player(plt, s, o) ->
     update_collid state collid state.collids
  | _ -> update_collid state collid state.collids

let setup canvas =
  let ctx = canvas##getContext (Dom_html._2d_) in
  let cw = canvas##.width in
  let ch = canvas##.height in
  let imgMap = Sprite.setup ctx in
  let player = Object.initial_make_player imgMap cw ch in
  let vpt = Viewport.make (cw, ch) in
  let collids = Procedural_generator.generate {x = 0; y = 0} { x = cw; y = ch }
                |> Object.make_all imgMap
  in
  ({
      bgd = None;
      ctx;
      cw;
      ch;
      collids;
      imgMap;
      vpt = vpt;
      score = 0;
      game_over = false;
    },
   player)

let start canvas =
  let rec game_loop time state player = begin
      print_endline "tick";

      Draw.clear_canvas canvas;
      player::state.collids |> Viewport.filter_into_view state.vpt
                            |> Draw.render canvas;

      let player = run_update_collid state player in
      let collids = List.map (run_update_collid state) state.collids in

      let next_state = state in
      ignore (Dom_html.window##requestAnimationFrame 
                (Js.wrap_callback (fun (t:float) ->
                     game_loop t state player));)
    end in
  let (initial_state, initial_player) = setup canvas in
  game_loop 0. initial_state initial_player

(* Keydown event handler translates a key press *)
let keydown evt =
  let () = match evt##.keyCode with
  | 39 | 68 -> pressed_keys.right <- true
  | 37 | 65 -> pressed_keys.left <- true
  | _ -> ()
  in Js._true

(* Keyup event handler translates a key release *)
let keyup evt =
  let () = match evt##.keyCode with
  | 39 | 68 -> pressed_keys.right <- false
  | 37 | 65 -> pressed_keys.left <- false
  | _ -> ()
  in Js._true
