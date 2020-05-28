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
(*
let run_generate_collids state : state =
  let prerender_height = state.vpt.pos.y + state.vpt.dim.y in
  if (prerender_height > state.generated_h) then
    let new_collids = Object.make_all state.imgMap state.time
                        (Procedural_generator.generate { x = 0; y = state.generated_h }
                           { x = state.cw; y = state.generated_h + state.vpt.dim.y }) in
    let collids = state.collids@new_collids in
    let generated_h = state.generated_h + state.vpt.dim.y in
    { state with collids; generated_h }
  else state

let run_remove_collids state collids =
  let rec remove_helper remain_collids collids =
    match collids with
    | [] -> remain_collids
    | h::t -> let obj_st = get_obj h in
              if (obj_st.killed || 
                 (state.vpt.pos.y > obj_st.pos.y + 24))
              then remove_helper remain_collids t
              else remove_helper (h::remain_collids) t
  in
  remove_helper [] collids
 *)
let update_draw_bb state =
  { state with draw_bb = pressed_keys.bbox }
let update_time time state =
  { state with time; }
(* Handle player only and player-collids collisions *)
let update_player_collids state player collids =
  let keys = translate_keys() in
  Object.update_player_collids state keys player collids
(* Handle collids only (there are no collid-collid collisions) *)
let update_collids state collids =
  List.map (Object.update_collid state) collids
let update_viewport state player = state

  
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
    generated_h = ch;
    draw_bb = false;
    game_over = false;
  }

let start canvas =
  let rec game_loop time state player collids = begin
      let state = state |> update_time time
                        |> update_draw_bb
      in
      (*Draw phase*)
      canvas   |> Draw.clear_canvas;
      collids  |> Viewport.filter_into_view state.vpt
               |> Draw.render state canvas;
      [player] |> Viewport.filter_into_view state.vpt
               |> Draw.render state canvas;

      (*Update phase*)
      let (player,collids) = update_player_collids state player collids in
      let collids = update_collids state collids in

      let vpt = Viewport.move state.vpt player in

      (* Generate new collidables, remove old collidables *)

      (* Prepare next phase *)
      ignore (Dom_html.window##requestAnimationFrame 
              (Js.wrap_callback (fun (next_time:float) ->
               game_loop next_time state player collids));)
    end in
  let debug = false in
  let initial_state = setup canvas in
  let cw = initial_state.vpt.dim.x and ch = initial_state.vpt.dim.y in
  let initial_player = Object.make_player Standing { x = cw/2; y = cw/8 } 0. in
  let initial_collids = if debug then Pg.generate_debug
                        else Pg.generate initial_state 0 ch in
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
