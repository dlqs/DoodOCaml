open Js_of_ocaml
   
type xy = {
    x: int;
    y: int;
  }

type fxy = {
    fx: float;
    fy: float;
  }

type box = {
    pos: xy;
    dim: xy;
  }
type viewport = box
type aabb = box

type controls =
  | CLeft
  | CRight

type pl_typ =
  | Standing

type tile_typ =
  | Green
  | Blue
  | Yellow
  | White

type sprite_typ =
  | PStanding
  | TGreen
  | TBlue
  | TYellow
  | TWhite

type imgMap_t = Dom_html.imageElement Js.t Map.Make(String).t

type pxy = int * int (* x, y *)
type sprite_params = {
    max_frames: int;
    max_ticks: int ref;
    img_src: string;
    frame_size: pxy;
    src_offset: pxy;
    bbox_offset: pxy;
    bbox_size: pxy;
    loop: bool;
  }

type sprite = {
    mutable params: sprite_params;
    frame: int ref;
    ticks: int ref;
    img: Dom_html.imageElement Js.t;
  }

type obj_state = {
    id: int;
    pos: xy;
    vel: fxy;
    created_at: float;
    killed: bool;
    debug_pt: xy option;
  }

type collidable =
  | Player of pl_typ * sprite * obj_state
  | Tile of tile_typ * sprite * obj_state



(* Represents the values of relevant key bindings. *)
type keys = {
  mutable left: bool;
  mutable right: bool;
  mutable up: bool;
  mutable down: bool;
  mutable bbox: bool;
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
  vpt: viewport;
  time: float;
  score: int;
  next_generated_height: int;
  last_generated_height: int;
  draw_bb: bool;
  game_over: bool;
}
