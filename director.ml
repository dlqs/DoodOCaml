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
  cw: int;
  ch: int;
  collids: collidable list;
  imgMap: Sprite.imgMap_t;
  vpt: Viewport.viewport;
  time: float;
  score: int;
  generated_h: int;
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

let check_bbox_enabled () = pressed_keys.bbox = 1

(* Converts a keypress to a list of control keys, allowing more than one key
 * to be processed each frame. *)
let translate_keys () =
  let k = pressed_keys in
  let ctrls = [(k.left,Actors.CLeft);(k.right,Actors.CRight);] in
  List.fold_left (fun a x -> if fst x then (snd x)::a else a) [] ctrls

let run_update_collid state collid =
  match collid with
  | Player(plt, ps, po) as player ->
     let keys = translate_keys () in
     Object.update_player state.cw state.collids keys player
  | _ as non_player -> Object.update_collid state.cw non_player

let run_update_collids state collids = List.map (run_update_collid state) collids

let run_generate_collids state : state =
  let prerender_height = state.vpt.pos.y + 2*state.vpt.v_dim.y in
  if (prerender_height > state.generated_h) then
    let new_collids = Object.make_all state.imgMap state.time
                        (Procedural_generator.generate { x = 0; y = state.generated_h }
                           { x = state.cw; y = state.generated_h + state.vpt.v_dim.y }) in
    let collids = state.collids@new_collids in
    let generated_h = state.generated_h + state.vpt.v_dim.y in
    { state with collids; generated_h }
  else state

let run_remove_collids state collids =
  let rec remove_helper remain_collids collids =
    match collids with
    | [] -> remain_collids
    | h::t -> if state.vpt.pos.y > (get_obj h).pos.y + 24
              then remove_helper remain_collids t
              else remove_helper (h::remain_collids) t
  in
  remove_helper [] collids

let setup canvas =
  let ctx = canvas##getContext (Dom_html._2d_) in
  let cw = canvas##.width in
  let ch = canvas##.height in
  let imgMap = Sprite.setup ctx in
  let player = Object.make imgMap 0. (APlayer(Standing), { x=cw/2; y = cw/8 }) in
  let vpt = Viewport.make (cw, ch) in
  let debug = true in
  let collids = Object.make_all imgMap 0.
                  (if debug
                  then Procedural_generator.generate_debug
                  else Procedural_generator.generate { x = 0; y = 0 } { x = cw; y = 2*ch }) in
  ({
      bgd = None;
      ctx;
      cw;
      ch;
      collids;
      imgMap;
      vpt = vpt;
      time = 0.;
      score = 0;
      generated_h = 2*ch;
      game_over = false;
    },
   player)

let start canvas =
  let rec game_loop time state player = begin
      (*Draw phase*)
      Draw.clear_canvas canvas;
      state.collids@[player] |> Viewport.filter_into_view state.vpt
                             |> Draw.render ~draw_bb:(check_bbox_enabled ()) canvas;
      (*Update phase*)
      let player = run_update_collid state player in
      let collids = run_update_collids state state.collids in
      let collids = run_remove_collids state collids in
      let vpt = Viewport.move state.vpt player in
      let state = { state with time; vpt; collids; } in

      (* Generate new collidables, remove old collidables *)
      let state = run_generate_collids state in

      (* Prepare next phase *)
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
  | 66 -> pressed_keys.bbox <- (pressed_keys.bbox + 1) mod 2
  | _ -> ()
  in Js._true

(* Keyup event handler translates a key release *)
let keyup evt =
  let () = match evt##.keyCode with
  | 39 | 68 -> pressed_keys.right <- false
  | 37 | 65 -> pressed_keys.left <- false
  | _ -> ()
  in Js._true
