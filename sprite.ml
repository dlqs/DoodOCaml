open Js_of_ocaml
open Types
module StringMap = Map.Make(String)

let imgDir = "./sprites/"
let imgSrcs = ["vehicles.png";"obstacles.png";"items.png"]
let imgMap_opt = ref None

(* Helper to create sprite image elements only once then cache. *)
let get_imgMap () =
  match !imgMap_opt with
  | Some imgMap -> imgMap
  | None -> 
     let rec helper srcs imgMap =
       match srcs with
       | [] -> imgMap
       | h::t ->
          let imgSrc = imgDir ^ h in
          let img = (Dom_html.createImg Dom_html.document) in
          img##.src := (Js.string imgSrc);
          helper t (StringMap.add imgSrc img imgMap)
     in
     let imgMap = helper imgSrcs StringMap.empty in
     imgMap_opt := Some imgMap;
     imgMap

(*setup_sprite is used to initialize a sprite.*)
let setup_sprite ?loop:(loop=true) ?bb_off:(bbox_offset=(0,0)) ?bb_sz:(bbox_size=(0,0))
      img_src max_frames max_ticks frame_size src_offset =
  let bbox_size = if bbox_size = (0,0) then frame_size else bbox_size in
  let img_src = "./sprites/" ^ img_src in
  {
    img_src;
    max_frames;
    max_ticks;
    frame_size;
    src_offset;
    bbox_offset;
    bbox_size;
    loop;
  }

(* Makes a sprite from provided [params]. *)
let make_from_params params =
  let imgMap = get_imgMap () in
  let img = StringMap.find params.img_src imgMap in
  {
    params;
    frame = ref 0;
    ticks = ref 0;
    img;
  }

let make_veh (vt:veh_typ) (vd:veh_dir) : sprite =
  let params = match vt with
    | Player -> begin match vd with
                | Str ->   setup_sprite "vehicles.png" ~bb_off:(0, 0) ~bb_sz:(40, 60) 1 (ref 0) (40, 60) (0, 0)
                | Left ->  setup_sprite "vehicles.png" ~bb_off:(0, 0) ~bb_sz:(40, 60) 1 (ref 0) (40, 60) (0, 60)
                | Right -> setup_sprite "vehicles.png" ~bb_off:(0, 0) ~bb_sz:(40, 60) 1 (ref 0) (40, 60) (0, 120)
                end
    | Police -> setup_sprite "vehicles.png" ~bb_off:(0, 0) ~bb_sz:(40, 60) 1 (ref 0) (40, 60) (0, 180)
  in
  make_from_params params

let make_obst (ot:obst_typ) : sprite =
  let params = match ot with
    | Barrier -> setup_sprite "obstacles.png" ~bb_off:(0, 0) ~bb_sz:(60, 20) 1 (ref 0) (60, 20) (0, 0)
  in
  make_from_params params
                
let make_item (it:item_typ) : sprite =
  let params = match it with
    | Health -> setup_sprite "items.png" ~bb_off:(0, 0) ~bb_sz:(40, 40) 1 (ref 0) (40, 40) (0, 0)
  in
  make_from_params params

(*update_animation is the main method to cycle through sprite animations*)
let update_animation (spr: sprite) =
  (* Only advance frame when ticked *)
  let curr_ticks = !(spr.ticks) in
  let max_ticks = !(spr.params.max_ticks) in
  if curr_ticks >= max_ticks then begin
    spr.ticks := 0;
    if spr.params.loop then
    spr.frame := (!(spr.frame) + 1) mod spr.params.max_frames
  end else spr.ticks := curr_ticks + 1

let update_max_ticks (spr:sprite) (coeff:float) =
  spr.params.max_ticks := int_of_float (coeff *. float_of_int !(spr.params.max_ticks))
  

