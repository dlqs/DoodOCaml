open Js_of_ocaml
open Actors

type xy = int * int

type sprite_params =
  {
    max_frames: int;
    max_ticks: int;
    img_src: string;
    frame_size: xy;
    src_offset: xy;
    bbox_offset: xy;
    bbox_size: xy;
    loop: bool;
  }

type sprite =
  {
    mutable params: sprite_params;
    context: Dom_html.canvasRenderingContext2D Js.t;
    frame: int ref;
    ticks: int ref;
    mutable img: Dom_html.imageElement Js.t;
  }

(*setup_sprite is used to initialize a sprite.*)
let setup_sprite  ?loop:(loop=true) ?bb_off:(bbox_offset=(0,0))
          ?bb_sz:(bbox_size=(0,0))
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
  | AEnemy(t) -> (16, 16)
  | AItem(t) -> (16, 16)
  | ABlock(t) -> (16, 16)
  | ATile(t) -> (40, 12)

(*The following functions are used in order to define sprite animations
 *from their sprite sheets. Also creates bounding boxes if necessary.*)

(*Sets sprite for small mario.*)
let make_small_player (typ, dir) =
  let fs = get_s_frame_size (SPlayer(SmallM, Standing)) in
  match dir with
    (* 16x16 grid with 0x0 offset*)
    | Left -> begin match typ with
      | Standing -> setup_sprite "mario-small.png" ~bb_off:(3,1) ~bb_sz:(11,15) 1 0 fs (0,0)
      | Jumping -> setup_sprite "mario-small.png" ~bb_off:(2,1) ~bb_sz:(13,15) 2 10 fs (16,16)
      | Running -> setup_sprite "mario-small.png" ~bb_off:(2,1) ~bb_sz:(12,15) 3 5 fs (16,0)
      | Crouching -> setup_sprite "mario-small.png" ~bb_off:(1,5) ~bb_sz:(14,10) 1 0 fs (0,64)
      end
    | Right -> begin match typ with
      | Standing -> setup_sprite "mario-small.png" ~bb_off:(1,1) ~bb_sz:(11,15) 1 0 fs (0,32)
      | Jumping -> setup_sprite "mario-small.png" ~bb_off:(2,1) ~bb_sz:(13,15) 2 10 fs (16,48)
      | Running -> setup_sprite "mario-small.png" ~bb_off:(2,1) ~bb_sz:(12,15) 3 5 fs (16,32)
      | Crouching -> setup_sprite "mario-small.png" ~bb_off:(1,5) ~bb_sz:(14,10) 1 0 fs (0,64)
      end

let make_tile (typ) =
  let fs = get_s_frame_size (STile(typ)) in
  match typ with
  | Green -> setup_sprite "green_tile.png" 1 0 fs (0,0)

(*Calls to set sprite for either big or small mario.*)
let make_player pt spr_type =
  match pt with
  | SmallM -> make_small_player spr_type
  | _ -> failwith "invalid"

(*Calls to set sprites for each type of object.*)
let make_type typ (dir : Actors.dir_1d) =
  match typ with
  | SPlayer(pt,st) -> make_player pt (st,dir)
  | STile t -> make_tile t
  | _ -> failwith "invalid"
     

(* Makes a sprite from provided [params]. *)
let make_from_params params context =
  let img = (Dom_html.createImg Dom_html.document) in
  img##.src := (Js.string params.img_src) ;
  {
    params;
    context;
    img;
    frame = ref 0;
    ticks = ref 0;
  }

(*Make is the wrapper function to cycle through sprite animations*)
let make spawn dir context  =
  let params = make_type spawn dir in
  make_from_params params context

(* Make a background *)
let make_bgd context =
  let params = setup_sprite "bgd-1.png" 1 0 (512,256) (0,0) in
  make_from_params params context

(*update_animation is the main method to cycle through sprite animations*)
let update_animation (spr: sprite) =
  (* Only advance frame when ticked *)
  let curr_ticks = !(spr.ticks) in
  if curr_ticks >= spr.params.max_ticks then begin
    spr.ticks := 0;
    if spr.params.loop then
    spr.frame := (!(spr.frame) + 1) mod spr.params.max_frames
  end else spr.ticks := curr_ticks + 1

