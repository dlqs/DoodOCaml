type dir_1d = | Left | Right
type dir_2d = | North | South | East | West

type xy = {
  x: int;
  y: int;
}

type controls =
  | CLeft
  | CRight
  | CUp
  | CDown

type pl_typ =
  | BigM
  | SmallM

type tile_typ =
  | Green

type player_typ =
  | Standing
  | Jumping
  | Running
  | Crouching

(* actors are abstract objects *)
type actor_typ =
  | APlayer of pl_typ * player_typ
  | ATile of tile_typ
