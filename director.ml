open Js_of_ocaml
open Types
module Pg = Procedural_generator

(*pressed_keys instantiates the keys.*)
let pressed_keys = {
  left = false;
  right = false;
  up = false;
  down = false;
  bbox = false;
}

(* Converts a keypress to a list of control keys, allowing more than one key
 * to be processed each frame. *)
let translate_keys () =
  let k = pressed_keys in
  let ctrls = [(k.left,CLeft);(k.right,CRight);] in
  List.fold_left (fun a x -> if fst x then (snd x)::a else a) [] ctrls

(* Generates new collidables for next screen *)
let generate_collids state collids =
  let new_collids =
    if state.next_generated_height > state.last_generated_height
    then Pg.generate state (state.last_generated_height + 1) state.next_generated_height
    else [] in
  collids@new_collids

(* Returns collidables without removed ones *)
let remove_collids state collids =
  collids |> List.filter (fun collid -> not (Viewport.below_vpt state.vpt collid))
          |> List.filter (fun collid -> not (Object.get_obj collid).killed)

let update_draw_bb state =
  { state with draw_bb = pressed_keys.bbox }

let update_time time state =
  { state with time; }

(* Moves viewport upwards as player moves upwards *)
let update_viewport player state =
  let vpt = Viewport.move state.vpt player in
  { state with vpt; }

(* Reached current generated level: sets new height for the next one *)
let update_generated_height state =
  let last_generated_height =
    if state.last_generated_height < state.next_generated_height
    then state.next_generated_height
    else state.last_generated_height in
  let next_generated_height =
    if state.vpt.pos.y + state.vpt.dim.y >= state.next_generated_height
    then state.next_generated_height + state.vpt.dim.y
    else state.next_generated_height in
  { state with last_generated_height; next_generated_height; }

(* Handles player only and player-collids collisions *)
let update_player_collids state player collids =
  let keys = translate_keys() in
  Object.update_player_collids state keys player collids

(* Handles collids only (there are no collid-collid collisions) *)
let update_collids state collids =
  List.map (Object.update_collid state) collids

let setup canvas =
  let ctx = canvas##getContext (Dom_html._2d_) in
  let cw = canvas##.width in
  let ch = canvas##.height in
  let vpt = Viewport.make (cw, ch) in
  {
    bgd = None;
    ctx;
    vpt;
    time = 0.;
    score = 0;
    last_generated_height = ch;
    next_generated_height = ch;
    draw_bb = false;
    game_over = false;
  }

let start canvas =
  let rec game_loop time state player collids = begin
      let state = state |> update_time time
                        |> update_draw_bb
                        |> update_viewport player
                        |> update_generated_height 
      in

      (*Draw phase*)
      canvas   |> Draw.clear_canvas;
      collids  |> List.map (Viewport.translate_for_draw state.vpt)
               |> Draw.render state canvas;
      [player] |> List.map (Viewport.translate_for_draw state.vpt)
               |> Draw.render state canvas;

      (*Update existing collidables *)
      let (player,collids) = update_player_collids state player collids in
      let collids = update_collids state collids in

      (* Generate new collidables, remove old collidables *)
      let collids = collids |> generate_collids state
                            |> remove_collids state
      in
      (* Prepare next phase *)
      ignore (Dom_html.window##requestAnimationFrame 
              (Js.wrap_callback (fun (next_time:float) ->
               game_loop next_time state player collids));)
    end in
  let debug = true in
  let initial_state = setup canvas in
  let cw = initial_state.vpt.dim.x and ch = initial_state.vpt.dim.y in
  let initial_player = Object.make_player Standing { x = cw/2; y = cw/8 } 0. in
  let initial_collids = if debug then Pg.generate_debug
                        else Pg.generate initial_state 0 ch
  in
  game_loop 0. initial_state initial_player initial_collids

(* Keydown event handler translates a key press *)
let keydown evt =
  let () = match evt##.keyCode with
  | 39 | 68 -> pressed_keys.right <- true
  | 37 | 65 -> pressed_keys.left <- true
  | 66 -> pressed_keys.bbox <- not pressed_keys.bbox
  | _ -> ()
  in Js._true

(* Keyup event handler translates a key release *)
let keyup evt =
  let () = match evt##.keyCode with
  | 39 | 68 -> pressed_keys.right <- false
  | 37 | 65 -> pressed_keys.left <- false
  | _ -> ()
  in Js._true
