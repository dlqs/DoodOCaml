open Actors

type xy = {
    x: int;
    y: int;
}

type fxy = {
    fx: float;
    fy: float;
}

type obj_prefab = actor_typ * xy

type obj_state = {
    id: int;
    has_gravity: bool;
    pos: xy;
    vel: fxy;
  }

type collidable =
  | Player of pl_typ * Sprite.sprite * obj_state
  | Tile of tile_typ * Sprite.sprite * obj_state

(*Variables*)
let gravity = -0.1
let player_jump = 5.7

let id_counter = ref min_int

(*Used in object creation and to compare two objects.*)
let new_id () =
  id_counter := !id_counter + 1;
  !id_counter

let update_pos obj_st =
  {
    obj_st with pos = {
      x = max (obj_st.pos.x + int_of_float obj_st.vel.fx) 0;
      y = max (obj_st.pos.y + int_of_float obj_st.vel.fy) 0;
    }
  }
let update_vel obj_st =
  if obj_st.has_gravity then
  {
    obj_st with vel = {
      fx = obj_st.vel.fx;
      fy = obj_st.vel.fy +. gravity
    }
  } else obj_st

let move obj_st =
  obj_st |> update_pos |> update_vel


let move_all collids =
  List.map (fun collid ->
      match collid with
      | Player(plt, s, o) -> Player(plt, s, move o)
      | Tile(tt, s, o) -> Tile(tt, s, move o)
    ) collids

(*Helper methods for getting sprites and objects from their collidables*)
let get_sprite = function
  | Player (_,s,_) | Tile(_,s,_) -> s

let get_obj = function
  | Player (_,_,o) | Tile(_,_,o) -> o

let setup =
  {
    id = new_id();
    has_gravity = false;
    pos = {x = 0; y = 0};
    vel = {fx = 0.; fy = 0.};
  }

let setup_player pos =
  {
    setup with
    pos = pos
  }

let setup_tile pos =
  {
    setup with
    pos = pos
  }

let make imgMap op =
  let typ = fst op in
  let pos = snd op in
  match typ with
  | APlayer(plt) -> Player(plt, Sprite.make typ imgMap, setup_player pos)
  | ATile(_) -> Tile(Green, Sprite.make typ imgMap, setup_tile pos)

let rec make_all imgMap ops: collidable list =
  match ops with
  | [] -> []
  | h::t -> (make imgMap h)::make_all imgMap t

let initial_make_player imgMap cw ch =
  let obj_st = setup in
  let obj_st = {
    obj_st with pos = { x = cw/2; y = cw/8 };
                vel = { fx = 0.; fy = 5. };
                has_gravity = true
  } in
  let s = Sprite.make (APlayer(Standing)) imgMap in
  Player(Standing, s, obj_st)

