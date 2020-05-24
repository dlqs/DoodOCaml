open Actors

type xy = {
    x: int;
    y: int;
}

type fxy = {
    fx: float;
    fy: float;
  }

type aabb = {
    pos: xy;
    dim: xy;
  }

type obj_prefab = actor_typ * xy

type obj_state = {
    id: int;
    has_gravity: bool;
    has_friction: bool;
    pos: xy;
    vel: fxy;
  }

type collidable =
  | Player of pl_typ * Sprite.sprite * obj_state
  | Tile of tile_typ * Sprite.sprite * obj_state

(*Variables*)
let gravity = -0.1
let friction_coef = 0.92
let pl_jmp_vel = 5.
let pl_lat_vel = 2.
let pl_max_lat_vel = 4.

let id_counter = ref min_int

(*Used in object creation and to compare two objects.*)
let new_id () =
  id_counter := !id_counter + 1;
  !id_counter

(*Helper methods for getting sprites and objects from their collidables*)
let get_sprite = function
  | Player (_,s,_) | Tile(_,s,_) -> s

let get_obj = function
  | Player (_,_,o) | Tile(_,_,o) -> o

let update_player_keys obj_st controls =
  match controls with
  | [] -> obj_st
  | ctrl::t -> let fx = match ctrl with
               | Actors.CLeft ->   max (obj_st.vel.fx -. pl_lat_vel) pl_max_lat_vel*.(-1.0)
               | Actors.CRight ->  min (obj_st.vel.fx +. pl_lat_vel) pl_max_lat_vel
               in
               { obj_st with vel = { obj_st.vel with fx; }}
                  

let update_player controls player =
  match player with
  | Player(plt, s, o) -> 
     let o = update_player_keys o controls in
     Player(plt, s, o)
  | _ -> failwith "Method called with non-player collidable"

let update_pos obj_st =
  {
    obj_st with pos = {
      x = max (obj_st.pos.x + int_of_float obj_st.vel.fx) 0;
      y = max (obj_st.pos.y + int_of_float obj_st.vel.fy) 0;
    }
  }
let update_vel obj_st =
  let fy = obj_st.vel.fy +. if obj_st.has_gravity then gravity else 0. in
  let fx = obj_st.vel.fx *. friction_coef
  in
  {
    obj_st with vel = { fx; fy }
  }

let move obj_st =
  obj_st |> update_pos |> update_vel


let move_all collids =
  List.map (fun collid ->
      match collid with
      | Player(plt, s, o) -> Player(plt, s, move o)
      | Tile(tt, s, o) -> Tile(tt, s, move o)
    ) collids


let setup =
  {
    id = new_id();
    has_gravity = false;
    has_friction = false;
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
                has_gravity = true;
                has_friction = true;
  } in
  let s = Sprite.make (APlayer(Standing)) imgMap in
  Player(Standing, s, obj_st)

let get_aabb collid =
  let spr = ((get_sprite collid).params) in
  let obj_st = get_obj collid in
  let (offx, offy) = spr.bbox_offset in
  let (bx,by) = (obj_st.pos.x+offx,obj_st.pos.y+offy) in
  let (sx,sy) = spr.bbox_size in
  {
    pos = { x = bx; y = by };
    dim = { x = sx; y = sy };
  }

(* move bounding box b by vector v *)
let move_aabb (b: aabb) (v: fxy) : aabb =
  {
    b with pos = {
      x = b.pos.x + (int_of_float v.fx); 
      y = b.pos.y + (int_of_float v.fy); 
    }
  }

let vec_minus v1 v2 =
  {
    fx = v1.fx -. v2.fx;
    fy = v1.fy -. v2.fy;
  }

(* Returns true if the bounding boxes are touching *)
let bb_collide (b1:aabb) (b2:aabb) : bool =
  let b1ay = b1.pos.y and b1by = b1.pos.y + b1.dim.y in
  let b2ay = b2.pos.y and b2by = b2.pos.y + b2.dim.y in
  if ((b2ay <= b1ay && b1ay <= b2by) || (b2ay <= b1by && b1by <= b2by)) then
    let b1ax = b1.pos.x and b1bx = b1.pos.x + b1.dim.x in
    let b2ax = b2.pos.x and b2bx = b2.pos.x + b2.dim.x in
    if ((b2ax <= b1ax && b1ax <= b2bx) || (b2ax <= b1bx && b1bx <= b2bx)) then
      true
    else
      false
  else
    false

(* Returns true if the collidables are currently colliding *)
let currently_colliding c1 c2 =
  let b1 = get_aabb c1 in
  let b2 = get_aabb c2 in
  bb_collide b1 b2

(* Returns true if the collidables will collide based on their velocity vectors *)
let will_collide c1 c2 =
  let b1 = get_aabb c1 in
  let b2 = get_aabb c2 in
  let v1 = (get_obj c1).vel in
  let v2 = (get_obj c2).vel in
  let b1 = vec_minus v1 v2 |> move_aabb b1 in
  bb_collide b1 b2
    
(* One-body collision *)
let check_collision c1 c2 =
  match c1 with
  | Player(plt, ps, po) as p ->
     begin match c2 with
     | Tile(tt, ts, t_o) as t -> 
        if ((will_collide p t) &&
            (not (currently_colliding p t)) &&
            (po.vel.fy < 0.)
           )then
          let fy = pl_jmp_vel in
          let po = { po with vel = { po.vel with fy }} in
          Player(plt, ps, po)
        else p
     | _ -> c1
     end
  | _ -> c1
  

let move_collid collid =
  match collid with
  | Player(plt, s, o) ->
     Player(plt, s, move o)
  | Tile(tt, s, o) -> Tile(tt, s, move o)

let rec update_collid collids collid =
  match collids with
  | [] -> collid
  | h::t -> update_collid t (check_collision collid h)
