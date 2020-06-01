open Types

(*Variables*)
let gravity = -0.15 (* constant change in velocity along y axis only *)
let friction_coef = 0.92  (* coefficient of velocity along x axis only *)
let pl_jmp_vel = 7.0
let pl_lat_vel = 2.
let pl_max_lat_vel = 4.

let id_counter = ref min_int

let fi = float_of_int

(*Used in object creation and to compare two objects.*)
let new_id () =
  id_counter := !id_counter + 1;
  !id_counter

let setup () : obj_state =
  {
    id = new_id();
    pos = {x = 0; y = 0};
    vel = {fx = 0.; fy = 0.};
    created_at = 0.;
    killed = false;
    debug_pt = None;
  }

(*Helper methods for getting sprites and objects from their collidables*)
let get_sprite = function
  | Vehicle (_,_,s,_) | Obstacle(_,s,_) | Item(_,s,_) -> s

let get_obj = function
  | Vehicle (_,_,_,o) | Obstacle(_,_,o) | Item(_,_,o) -> o

let make_veh (typ:veh_typ) (dir:veh_dir) (pos:xy) : collidable =  
  let vo = { (setup ()) with
             vel = { fx = 0.; fy = pl_jmp_vel };
             pos;
           } in
  match typ with
  | Player -> 
     Vehicle(Player, dir, Sprite.make_veh Player Str, vo)
  | Police ->
     Vehicle(Police, dir, Sprite.make_veh Police Str, vo)

let make_obst (typ:obst_typ) (pos:xy) : collidable =  
  let oo = { (setup ()) with
             pos;
           } in
  match typ with
  | Barrier -> Obstacle(Barrier, Sprite.make_obst Barrier, oo)

let make_item (typ:item_typ) (pos:xy) : collidable =
  let sprite = match typ with
    | Health -> Sprite.make_item Health
  in
  let io = { (setup ()) with
             pos;
           } in
  Item(typ, sprite, io)

let update ?vt:v_t ?vd:v_d ?it:i_t ?ot:o_t
      ?spr ?pos ?vel ?debug_pt ?killed ?created_at (collid:collidable) =
  (* Helpers *)
  let may ~f x y =
    match x with
    | None -> y
    | Some x -> f x y in
  let may2 x y =
    match y with
    | None -> x
    | Some y -> y in
  let set_vel vel o = { o with vel; } in
  let set_pos pos o = { o with pos; } in
  let set_debug_pt debug_pt o = { o with debug_pt; } in
  let set_killed killed o = { o with killed; } in
  let set_created_at created_at o = { o with created_at; } in
  (* Actual *)
  let o = get_obj collid |> may ~f:set_vel vel
                         |> may ~f:set_pos pos
                         |> may ~f:set_debug_pt debug_pt
                         |> may ~f:set_killed killed
                         |> may ~f:set_created_at created_at
  in
  let s = may2 (get_sprite collid) spr in
  match collid with
  | Vehicle(vt, vd, _, _) ->
     let vt = may2 vt v_t in
     let vd = may2 vd v_d in
     Vehicle(vt, vd, s, o)
  | Obstacle(ot, _, _) ->
     let ot = may2 ot o_t in
     Obstacle(ot, s, o)
  | Item(it, _, _) ->
     let it = may2 it i_t in
     Item(it, s, o)

let update_animation collid =
  Sprite.update_animation (get_sprite collid); collid
     
let update_max_ticks coeff collid =
  Sprite.update_max_ticks (get_sprite collid) coeff ; collid
  
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

(* Returns true if bb of c1 is above c2*)
let is_bbox_above c1 c2 =
  let b1 = get_aabb c1 in
  let b2 = get_aabb c2 in
  b1.pos.y > (b2.pos.y + b2.dim.y)

(* Returns the closer bbox c2/c3 to c1 *)
let closer_bbox c1 c2 c3 =
  if (dist_collid c1 c2) < (dist_collid c1 c3) then c2 else c3

let rec update_player_keys controls collid =
  match controls with
  | [] -> collid
  | ctrl::t -> let vel = (get_obj collid).vel in
               let collid = match ctrl with
                 | CLeft ->
                    let vel = { vel with
                                fx = max (vel.fx -. pl_lat_vel) pl_max_lat_vel*.(-1.0) } in
                    update ~vel ~vd:Left collid
                 | CRight ->
                    let vel = { vel with
                                fx = min (vel.fx +. pl_lat_vel) pl_max_lat_vel } in
                    update ~vel ~vd:Left collid
               in
               update_player_keys t collid
          
let update_debug_pt (co:collidable option) (player:collidable): collidable =
  match co with
  | Some ct -> update ~debug_pt:(Some (get_aabb_center ct)) player
  | None -> player

(* Returns a moved collidable, depending on its movement semantics *)
let move state collid =
  let add_fxy_to_xy a b = { x = a.x + int_of_float b.fx;
                            y = a.y + int_of_float b.fy; } in
  (* Helper to wraparound position along the x-axis including width of obj *)
  let wraparound_x width max_x pos = let midx = pos.x + width/2 in
                              { pos with x =
                              if midx < 0 then max_x - width else
                              if midx > max_x then 0 else pos.x } in
  (* Helper to bounce (i.e.flip) velocity along the x-axis, including width of obj*)
  let bouncearound_x width max_x pos vel = { vel with fx =
                 if (pos.x <= 0 && vel.fx < 0.) || (pos.x + width >= max_x && vel.fx > 0.)
                 then vel.fx*.(-1.0) else vel.fx } in
  let vpt_width = state.vpt.dim.x in
  (* actual *)
  match collid with
  | Vehicle(Player, _, _, vo) as veh ->
     let pos = add_fxy_to_xy vo.pos vo.vel in
     update ~pos veh
  | _ -> failwith "Not implemented"

let find_closest_collidable (player:collidable) (collids:collidable list) : collidable option =
  let match_helper (collid:collidable) : collidable option = 
    match collid with
    | Obstacle(Barrier,_,_) ->
       if (will_collide collid player) then Some collid else None
    | Item(_,_,_) ->
       if (will_collide collid player) then Some collid else None
    | _ -> None
  in
  let rec helper (current_closest:collidable option) (collids:collidable list) : collidable option =
    match collids with
    | [] -> current_closest
    | h::t ->
       let j = match_helper h in
       match current_closest with
       | None -> begin match j with
                 | None -> helper None t
                 | Some j -> helper (Some j) t end
       | Some i -> begin match j with
                   | None -> helper (Some i) t
                   | Some j ->
                      let closer = closer_bbox player i h in
                      helper (Some closer) t end
  in
  helper None collids

  
let handle_collision state player collid : collidable * collidable =
  let po = get_obj player in
  let collid = match collid with
    | Vehicle(Player,_,_,_) -> failwith "Player cannot collid with itself" 
    | Vehicle(Police,_,_,_) -> print_endline "hit police"; collid
    | Obstacle(_,_,_) -> print_endline "hit obst"; collid
    | Item(_,_,_) -> print_endline "hit item"; collid
  in
  (player, collid)
          

let update_player_typ state player =
  match player with
  | _ -> player

let update_player_collids state keys player collids : collidable * collidable list =
  let closest_collidable = find_closest_collidable player collids in
  let player = player |> update_player_keys keys
                      |> update_debug_pt closest_collidable
                      |> update_player_typ state
  in
  match closest_collidable with
  | None -> (player |> move state, collids)
  | Some closest_collidable -> 
     let (player, collided) = handle_collision state player closest_collidable in
     let collids = List.map (fun collid ->
                     if (get_obj collid).id = (get_obj collided).id then collided else collid) collids in
     (player, collids)
      
(* collid only (no collid-collid interactions) *)
let update_collid state collid =
  match collid with
  | Vehicle(Player,_,_,_) as player -> failwith "Call update_player instead"
  | Vehicle(Police,_,_,_) as police ->
     police |> move state
  | _ -> collid

let update_collid_second state collid =
  match collid with
  | _ -> collid
