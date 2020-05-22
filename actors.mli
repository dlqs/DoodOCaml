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
  | Standing

type tile_typ =
  | Green

type actor_typ =
  | APlayer of pl_typ
  | ATile of tile_typ

