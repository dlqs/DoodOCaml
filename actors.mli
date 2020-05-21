type dir_1d = | Left | Right 
type dir_2d = | North | South | East | West

(* Controls correspond to keyboard input *)
type controls =
  | CLeft
  | CRight
  | CUp
  | CDown

(* Player ability type *)
type pl_typ =
  | BigM
  | SmallM

type tile_typ =
  | Green

(* Player action type *)
type pl_state =
  | Standing
  | Jumping
  | Running
  | Crouching

type actor_typ =
  | APlayer of pl_typ * pl_state
  | ATile of tile_typ

