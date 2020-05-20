type dir_1d = | Left | Right 
type dir_2d = | North | South | East | West

(* Generic xy record for easy position access *)
type xy = {
  x: int;
  y: int;
}

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
type player_typ =
  | Standing
  | Jumping
  | Running
  | Crouching

type actor_typ =
  | APlayer of pl_typ * player_typ
  | ATile of tile_typ

