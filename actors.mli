type dir_1d = | Left | Right 
type dir_2d = | North | South | East | West

type controls =
  | CLeft
  | CRight

(* Player ability type *)
type pl_typ =
  | Standing

type tile_typ =
  | Green
  | Blue

type actor_typ =
  | APlayer of pl_typ
  | ATile of tile_typ

