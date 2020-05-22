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

let imgsrcs = ["green_tile.png"]
let imgdir = "sprites/"

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
  | APlayer(t, _) -> begin match t with
                      | SmallM -> (16, 16)
                      | BigM -> (16, 27)
                      end
  | ATile(t) -> (40, 12)

(*The following functions are used in order to define sprite animations
 *from their sprite sheets. Also creates bounding boxes if necessary.*)

(*Sets sprite for small mario.*)
let make_small_player pls =
  let fs = get_s_frame_size (APlayer(SmallM, Standing)) in
  match pls with
  (* 16x16 grid with 0x0 offset*)
  | Standing -> setup_sprite "mario-small.png" ~bb_off:(3,1) ~bb_sz:(11,15) 1 0 fs (0,0)
  | Jumping -> setup_sprite "mario-small.png" ~bb_off:(2,1) ~bb_sz:(13,15) 2 10 fs (16,16)
  | Running -> setup_sprite "mario-small.png" ~bb_off:(2,1) ~bb_sz:(12,15) 3 5 fs (16,0)
  | Crouching -> setup_sprite "mario-small.png" ~bb_off:(1,5) ~bb_sz:(14,10) 1 0 fs (0,64)

let make_tile (typ) =
  let fs = get_s_frame_size (ATile(typ)) in
  match typ with
  | Green -> setup_sprite "green_tile.png" 1 0 fs (0,0)

(*Calls to set sprite for either big or small mario.*)
let make_player plt pls = 
  match plt with
  | SmallM -> make_small_player pls
  | _ -> failwith "invalid"


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
    | APlayer(plt,pls) -> make_player plt pls
    | ATile t -> make_tile t
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

  

