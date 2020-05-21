type dir_1d = | Left | Right
type dir_2d = | North | South | East | West

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

type pl_state =
  | Standing
  | Jumping
  | Running
  | Crouching

(* actors are abstract objects *)
type actor_typ =
  | APlayer of pl_typ * pl_state
  | ATile of tile_typ
