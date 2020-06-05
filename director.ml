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

let second_time = ref 0.
let check_second time = 
  if time >= !second_time +. 1000. then
  (second_time := time; true ) else false

(* Generates new collidables for next screen *)
let generate_collids state pre_generated =
  let new_collids =
    if state.next_generated_height > state.last_generated_height
    then Pg.generate state
    else [] in
  pre_generated@new_collids

(* Returns collidables without removed ones *)
let remove_collids state collids =
  collids |> List.filter (fun collid -> not (Viewport.below_vpt state.vpt collid))
          |> List.filter (fun collid -> not (Object.get_obj collid).killed)

let update_draw_bb state =
  { state with draw_bb = pressed_keys.bbox }

let update_time time state =
  { state with time; }

(* Moves viewport upwards as player moves upwards *)
let update_viewport state =
  let vpt = Viewport.move state in
  { state with vpt; }

(* Reached current generated level: sets new height for the next one *)
let update_generated_height state =
  let last_generated_height =
    if state.last_generated_height < state.next_generated_height
    then state.next_generated_height
    else state.last_generated_height in
  let next_generated_height =
    if state.vpt.pos.y + 2*state.vpt.dim.y >= state.next_generated_height
    then state.next_generated_height + 2*state.vpt.dim.y
    else state.next_generated_height in
  { state with last_generated_height; next_generated_height; }

(* Handles player only and player-collids collisions *)
let update_player_keys player =
  let keys = translate_keys() in
  Object.update_player_keys keys player 

let update_score state =
  { state with score = state.vpt.pos.y }

let update_collids state player collids =
  let all_collids = Object.update_collids state (player::collids) in
  (List.hd all_collids, List.tl all_collids)

let update_collids_per_second state player collids =
  let all_collids = Object.update_collids_per_second state (player::collids) in
  (List.hd all_collids, List.tl all_collids)

let move_from_pre_generated state collids pre_generated =
  let move_in = pre_generated |> List.filter (fun c -> not (Viewport.above_vpt state.vpt c))
                              |> List.map (Object.update ~created_at:state.time)
  in
  let pre_generated = pre_generated |> List.filter (Viewport.above_vpt state.vpt)
  in
  (collids@move_in, pre_generated)

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
    speed = 2;
    last_generated_height = 0;
    next_generated_height = 2*ch;
    draw_bb = false;
    game_over = false;
  }

let start canvas =
  let rec game_loop time state player collids pre_generated = begin
      let state = state |> update_time time
                        |> update_draw_bb
                        |> update_viewport
                        |> update_generated_height
                        |> update_score
      in

      (* Import in-view collids *)
      let (collids, pre_generated) = move_from_pre_generated state collids pre_generated in

      (*Draw*)
      canvas   |> Draw.clear_canvas;
      collids  |> List.map (Viewport.prepare_for_draw state.vpt)
               |> Draw.render state canvas;
      [player] |> List.map (Viewport.prepare_for_draw state.vpt)
               |> Draw.render state canvas;
      state    |> Draw.show_score canvas;

      (*Update existing collidables*)
      let player = update_player_keys player in
      let (player,collids) = update_collids state player collids in
      let (player,collids) = update_collids_per_second state player collids in
      let collids = remove_collids state collids in

      (* generate new collidables, but these will not be in-view yet*)
      let pre_generated = pre_generated |> generate_collids state in

      (* Prepare next phase *)
      ignore (Dom_html.window##requestAnimationFrame
              (Js.wrap_callback (fun (next_time:float) ->
               game_loop next_time state player collids pre_generated));)
    end in
  let debug = false in
  let initial_state = setup canvas in
  let cw = initial_state.vpt.dim.x and ch = initial_state.vpt.dim.y in
  let initial_player = Object.make_veh Player Str { x = cw/2; y = cw/3 } in
  let initial_collids = if debug then Pg.generate_debug
                        else Pg.generate initial_state
  in
  game_loop 0. initial_state initial_player [] initial_collids

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
