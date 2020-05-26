open Js_of_ocaml
open Actors
module StringMap = Map.Make(String)

type pxy = int * int (* x, y *)

type imgMap_t = Dom_html.imageElement Js.t Map.Make(String).t

type sprite_params =
  {
    max_frames: int;
    max_ticks: int;
    img_src: string;
    frame_size: pxy;
    src_offset: pxy;
    bbox_offset: pxy;
    bbox_size: pxy;
    loop: bool;
  }

type sprite =
  {
    mutable params: sprite_params;
    frame: int ref;
    ticks: int ref;
    img: Dom_html.imageElement Js.t;
  }

let imgdir = "./sprites/"
let imgsrcs = ["green_tile.png";"doodle.png";"blue_tile.png"]

(* Creates the HTML image elements first *)
let setup ctx =
  let rec setup_helper imgsrcs imgMap =
    match imgsrcs with
    | [] -> imgMap
    | h::t ->
      let imgsrc = imgdir ^ h in
      let img = (Dom_html.createImg Dom_html.document) in
      img##.src := (Js.string imgsrc);
      setup_helper t (StringMap.add imgsrc img imgMap)
  in
  setup_helper imgsrcs StringMap.empty

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

(*Get sprite frame size of a actor type.*)
let get_s_frame_size (typ: actor_typ) =
  match typ with
  | APlayer(plt) -> begin match plt with
                      | Standing -> (30, 45)
                      end
  | ATile(tt) -> (40, 12)

(*The following functions are used in order to define sprite animations
 *from their sprite sheets. Also creates bounding boxes if necessary.*)

let make_tile (typ) =
  let fs = get_s_frame_size (ATile(typ)) in
  match typ with
  | Green -> setup_sprite "green_tile.png" ~bb_off:(0, 0) ~bb_sz:(40, 12) 1 0 fs (0,0)
  | Blue -> setup_sprite "blue_tile.png" ~bb_off:(0, 0) ~bb_sz:(40, 12) 1 0 fs (0,0)

(*Calls to set sprite for either big or small mario.*)
let make_player plt = 
  let fs = get_s_frame_size (APlayer(Standing)) in
  match plt with
  | Standing -> setup_sprite "doodle.png" 1 0 fs (0,0)

(* Makes a sprite from provided [params]. *)
let make_from_params params imgMap =
  let img = StringMap.find params.img_src imgMap in
  {
    params;
    frame = ref 0;
    ticks = ref 0;
    img;
  }

(*Make is the wrapper function to cycle through sprite animations*)
let make actor imgMap =
  let params = match actor with
    | APlayer(plt) -> make_player plt
    | ATile(tt) -> make_tile tt
  in
  make_from_params params imgMap

(* Make a background *)
let make_bgd ctx imgMap =
  let params = setup_sprite "bgd-1.png" 1 0 (512,256) (0,0) in
  make_from_params params imgMap

(*update_animation is the main method to cycle through sprite animations*)
let update_animation (spr: sprite) =
  (* Only advance frame when ticked *)
  let curr_ticks = !(spr.ticks) in
  if curr_ticks >= spr.params.max_ticks then begin
    spr.ticks := 0;
    if spr.params.loop then
    spr.frame := (!(spr.frame) + 1) mod spr.params.max_frames
  end else spr.ticks := curr_ticks + 1

  

