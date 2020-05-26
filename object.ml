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
    debug_pt: xy option;
  }

type collidable =
  | Player of pl_typ * Sprite.sprite * obj_state
  | Tile of tile_typ * Sprite.sprite * obj_state

(*Variables*)
let gravity = -0.1
let friction_coef = 0.92
let pl_jmp_vel = 4.
let pl_lat_vel = 2.
let pl_max_lat_vel = 4.

let id_counter = ref min_int

let fi = float_of_int

(*Used in object creation and to compare two objects.*)
let new_id () =
  id_counter := !id_counter + 1;
  !id_counter

let setup () =
  {
    id = new_id();
    has_gravity = false;
    has_friction = false;
    pos = {x = 0; y = 0};
    vel = {fx = 0.; fy = 0.};
    debug_pt = None;
  }

let update ?spr ?pos ?vel ?debug_pt (collid:collidable) =
  (* Helpers *)
  let may ~f x y =
    match y with
    | None -> x
    | Some y -> f x y in
  let may2 x y =
    match y with
    | None -> x
    | Some y -> y in
  let set_vel o vel = { o with vel; } in
  let set_pos o pos = { o with pos; } in
  let set_debug_pt o debug_pt = { o with debug_pt; } in
  match collid with
  | Player(plt, ps, po) ->
     let po = may ~f:set_vel po vel in
     let po = may ~f:set_pos po pos in
     let po = may ~f:set_debug_pt po debug_pt in
     let ps = may2 ps spr in
     Player(plt, ps, po)
  | Tile(tt, ts, t_o) -> 
     let t_o = may ~f:set_vel t_o vel in
     let t_o = may ~f:set_pos t_o pos in
     let t_o = may ~f:set_debug_pt t_o debug_pt in
     let ts = may2 ts spr in
     Tile(tt, ts, t_o)

(*Helper methods for getting sprites and objects from their collidables*)
let get_sprite = function
  | Player (_,s,_) | Tile(_,s,_) -> s

let get_obj = function
  | Player (_,_,o) | Tile(_,_,o) -> o

let rec update_player_keys collid controls =
  match controls with
  | [] -> collid
  | ctrl::t -> let vel = (get_obj collid).vel in
               let fx = match ctrl with
               | Actors.CLeft ->   max (vel.fx -. pl_lat_vel) pl_max_lat_vel*.(-1.0)
               | Actors.CRight ->  min (vel.fx +. pl_lat_vel) pl_max_lat_vel
               in
               update_player_keys (update ~vel:{ vel with fx=fx } collid) t

let move_normally collid =
  let obj_st = get_obj collid in
  let pos =  {
      x = max (obj_st.pos.x + int_of_float obj_st.vel.fx) 0;
      y = max (obj_st.pos.y + int_of_float obj_st.vel.fy) 0;
    } in
  let vel = {
      fy = obj_st.vel.fy +. if obj_st.has_gravity then gravity else 0.;
      fx = obj_st.vel.fx *. friction_coef;
    } in
  update ~vel ~pos collid 

let make imgMap prefab =
  let typ = fst prefab in
  let pos = snd prefab in
  match typ with
  | APlayer(plt) ->
     let po = { (setup ()) with
                has_gravity = true;
                has_friction = true;
                vel = { fx = 0.; fy = 5. };
                pos;
              } in
     Player(plt, Sprite.make typ imgMap, po)
  | ATile(tt) ->
     let t_o = { (setup ()) with
                 pos;
               } in
     Tile(tt, Sprite.make typ imgMap, t_o)

let rec make_all imgMap ops: collidable list =
  match ops with
  | [] -> []
  | h::t -> (make imgMap h)::make_all imgMap t

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

let get_aabb_center collid =
  let b = get_aabb collid in
  {
    x = b.pos.x + (b.dim.x/2);
    y = b.pos.y + (b.dim.y/2);
  }

(* move bounding box b by vector v *)
let bb_move (b: aabb) (v: fxy) : aabb =
  {
    b with pos = {
      x = b.pos.x + (int_of_float v.fx); 
      y = b.pos.y + (int_of_float v.fy); 
    }
  }

let dist_collid (c1: collidable) (c2: collidable): float =
  let center1 = get_aabb_center c1 in
  let center2 = get_aabb_center c2 in
  let s1 = (fi (center1.x - center2.x)) ** 2. in
  let s2 = (fi (center1.y - center2.y)) ** 2. in
  sqrt (s1 +. s2)

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
  let b1 = vec_minus v1 v2 |> bb_move b1 in
  bb_collide b1 b2

(* One-body collision *)
let do_collision c1 c2 =
  match c1 with
  | Player(plt, ps, po) as p ->
     begin match c2 with
     | Tile(tt, ts, t_o) as t -> 
        if ((will_collide p t) &&
            (not (currently_colliding p t)) &&
            (po.vel.fy < 0.)
           )
        then
          let po = { po with vel = { po.vel with fy = pl_jmp_vel };
                             pos = { x = (po.pos.x + (int_of_float (po.vel.fx/.2.)));
                                     y = (po.pos.y + (int_of_float (po.vel.fy/.2.)))}
                   } in
          Player(plt, ps, po)
        else
          p
     | _ -> c1
     end
  | _ -> c1

let find_closest_collidable
      (player: collidable)
      (collids: collidable list)
      ~(ignore_tiles: bool)
    : collidable option = 
  let rec helper closest collids ~ignore_tiles =
    match collids with
    | [] -> closest
    | candidate::t ->
       match candidate with
       | Tile(_,_,_) when ignore_tiles=true -> helper closest t ~ignore_tiles:ignore_tiles
       | _ -> begin
           match closest with
           | None -> helper (Some candidate) t ~ignore_tiles:ignore_tiles
           | Some current_closest ->
              if ((dist_collid player current_closest) < (dist_collid player candidate))
              then helper closest t ~ignore_tiles:ignore_tiles
              else helper (Some candidate) t ~ignore_tiles:ignore_tiles
         end
  in
  let closest_collidable = helper None collids ~ignore_tiles in
  match closest_collidable with
  | Some c -> if (will_collide player c) && not (currently_colliding player c) then Some c else None
  | None -> None

let update_debug_pt (co:collidable option) (player:collidable): collidable =
  match co with
  | Some ct ->
     update ~debug_pt:(Some (get_aabb_center ct)) player
  | None ->
     player

let update_player collids controls player =
  let player = update_player_keys player controls in
  let po = get_obj player in
  let ignore_tiles = if po.vel.fy < 0. then true else false in
  let closest_collidable = find_closest_collidable player collids ~ignore_tiles in
  let player = update_debug_pt closest_collidable player in
  (*let player = begin match closest_collidable with
               | Some (Tile(tt, ts, t_o) as tile) -> 
                  let tab = get_aabb tile in
                  let center = get_aabb_center tile in
                  let dy = tab.pos.y + tab.dim.y in
                  let po = { po with vel = { po.vel with fy = pl_jmp_vel };
                           } in
                  Player(plt, ps, po)
               | _ ->
                  Player(plt, ps, po)
               end in
   *)
  move_normally player

let update_collid (collids: collidable list) (c1:collidable) : collidable =
  let to_collide = find_closest_collidable c1 collids ~ignore_tiles:false in
  match to_collide with
  | Some c2 -> do_collision c1 c2
  | None -> move_normally c1


