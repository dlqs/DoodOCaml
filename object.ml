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
  | Player (_,s,_) | Tile(_,s,_) | Item(_,s,_) -> s

let get_obj = function
  | Player (_,_,o) | Tile(_,_,o) | Item(_,_,o) -> o

let make_player (typ:pl_typ) (pos:xy) (created_at:float) : collidable =
  match typ with
  | Standing ->
     let po = { (setup ()) with
                vel = { fx = 0.; fy = pl_jmp_vel };
                pos;
                created_at;
              } in
     Player(Standing, Sprite.make PStanding, po)
  | Rocketing ->
     let po = { (setup ()) with
                vel = { fx = 0.; fy = pl_jmp_vel };
                pos;
                created_at;
              } in
     Player(Rocketing, Sprite.make PRocketing, po)

let make_tile (typ:tile_typ) (pos:xy) (created_at:float) : collidable =
  let sprite = match typ with
    | Green -> Sprite.make TGreen
    | Yellow -> Sprite.make TYellow
    | White -> Sprite.make TWhite
    | Blue -> Sprite.make TBlue
  in
  let vel = match typ with
    | Green | Yellow | White -> { fx = 0. ; fy = 0.; }
    | Blue -> { fx = 1.; fy = 0.; }
  in
  let t_o = { (setup ()) with
              vel;
              pos;
              created_at;
            } in
  Tile(typ, sprite, t_o)

let make_item (typ:item_typ) (pos:xy) (created_at:float) : collidable =
  let sprite = match typ with
    | Rocket -> Sprite.make IRocket
    | Spring -> Sprite.make ISpring
    | Monster -> Sprite.make IMonster
  in
  let io = { (setup ()) with
             pos;
             created_at;
           } in
  Item(typ, sprite, io)

let update ?plt:p_t ?it:i_t ?tt:t_t
      ?spr ?pos ?vel ?debug_pt ?killed ?created_at (collid:collidable) =
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
  let set_killed o killed = { o with killed; } in
  let set_created_at o created_at = { o with created_at; } in
  (* Actual *)
  match collid with
  | Player(plt, ps, po) ->
     let po = may ~f:set_vel po vel in
     let po = may ~f:set_pos po pos in
     let po = may ~f:set_debug_pt po debug_pt in
     let po = may ~f:set_killed po killed in
     let po = may ~f:set_created_at po created_at in
     let ps = may2 ps spr in
     let plt = may2 plt p_t in
     Player(plt, ps, po)
  | Tile(tt,ts,t_o) ->
     let t_o = may ~f:set_vel t_o vel in
     let t_o = may ~f:set_pos t_o pos in
     let t_o = may ~f:set_debug_pt t_o debug_pt in
     let t_o = may ~f:set_killed t_o killed in
     let t_o = may ~f:set_created_at t_o created_at in
     let ts = may2 ts spr in
     let tt = may2 tt t_t in
     Tile(tt, ts, t_o)
  | Item(it, is, io) ->
     let io = may ~f:set_vel io vel in
     let io = may ~f:set_pos io pos in
     let io = may ~f:set_debug_pt io debug_pt in
     let io = may ~f:set_killed io killed in
     let io = may ~f:set_created_at io created_at in
     let is = may2 is spr in
     let it = may2 it i_t in
     Item(it, is, io)

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
               let fx = match ctrl with
               | CLeft  -> max (vel.fx -. pl_lat_vel) pl_max_lat_vel*.(-1.0)
               | CRight -> min (vel.fx +. pl_lat_vel) pl_max_lat_vel
               in
               update_player_keys t (update ~vel:{ vel with fx=fx } collid)

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
  | Player(Standing, ps, po) as player ->
     (* Player killed *)
     if po.killed then
       let pos = add_fxy_to_xy po.pos po.vel in
       let vel = { fy = po.vel.fy +. gravity; fx = 0. } in
       update ~vel ~pos player
     else
     (* Player will wraparound and is the only collidable subject to friction, gravity. *)
     let player_width = fst ps.params.frame_size in
     let pos = wraparound_x player_width vpt_width (add_fxy_to_xy po.pos po.vel) in
     let vel = {
         fy = po.vel.fy +. gravity;
         fx = po.vel.fx *. friction_coef;
       } in
     update ~vel ~pos player
  | Player(Rocketing, ps, po) as player ->
     let player_width = fst ps.params.frame_size in
     let pos = wraparound_x player_width vpt_width (add_fxy_to_xy po.pos po.vel) in
     let vel = {
         fy = po.vel.fy;
         fx = po.vel.fx *. friction_coef;
       } in
     update ~vel ~pos player
  | Tile(Blue,ts,t_o) as tile ->
     let pos = add_fxy_to_xy t_o.pos t_o.vel in
     let width = fst (get_sprite tile).params.bbox_size in
     let vel = bouncearound_x width vpt_width pos t_o.vel in
     update ~vel ~pos tile
  | _ -> failwith "Not implemented"

let find_closest_collidable player collids =
  let rec helper current_closest collids =
    match collids with
    | [] -> current_closest
    | h::t ->
       let j = match h with
               | Tile(_,_,_) as tile ->
                  if ((is_bbox_above player tile) && (will_collide player tile))
                  then Some tile else None
               | Item(it,_,_) as item ->
                  begin match it with
                  | Rocket | Monster ->
                     if (will_collide player item) then Some item else None
                  | Spring ->
                     if ((is_bbox_above player item) && (will_collide player item))
                     then Some item else None
                  end
               | _ -> None
       in
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


let handle_collision state player collid =
  let po = get_obj player in
  match collid with
  | Player(_,_,_) -> failwith "Player cannot collid with itself"
  | Tile(tt,_,_) ->
     let killed = match tt with
       | White -> true
       | _ -> false
     in
     let vel = { po.vel with fy = pl_jmp_vel } in
     let player = update ~vel player in
     let collid = update ~killed collid in
     (player, collid)
  | Item(it,_,_) ->
     match it with
     | Rocket ->
        let player = make_player Rocketing po.pos state.time in
        let collid = update ~killed:true collid in
        (player, collid)
     | Spring ->
        let vel = { po.vel with fy = 2.*.pl_jmp_vel } in
        let player = update ~vel player in
        (player, collid)
     | Monster ->
        if (is_bbox_above player collid)
        then
          let collid = update ~killed:true collid in
          (player, collid)
        else
          let player = update ~killed:true player in
          (player, collid)

let update_player_typ state player =
  match player with
  | Player(Rocketing, ps, po) ->
     if state.time >= po.created_at +. 5000.
     then make_player Standing po.pos state.time
     else player
  | _ -> player

let update_player_collids state keys player collids : collidable * collidable list =
  if (get_obj player).killed then (player |> move state, collids) else
  let closest_collidable = find_closest_collidable player collids in
  let player = player |> update_player_keys keys
                      |> update_debug_pt closest_collidable
                      |> update_animation
                      |> update_player_typ state
  in
  match closest_collidable with
  | None -> (player |> move state, collids)
  | Some closest_collidable ->
     let (player, collided) = handle_collision state player closest_collidable in
     let collids = List.map (fun collid ->
                     if (get_obj collid).id = (get_obj collided).id then collided else collid) collids in
     (player |> move state , collids)

(* collid only (no collid-collid interactions) *)
let update_collid state collid =
  match collid with
  | Player(_,_,_) -> failwith "Call update_player instead"
  | Tile(Blue,_,_) as tile ->
     tile |> move state
  | Tile(Yellow,_,t_o) as tile ->
     let explode_time = 8000. in
     if state.time > t_o.created_at +. explode_time
       then tile |> update ~killed:true else
       tile |> update_animation
  | Tile(White,_,_) as tile ->
     tile
  | _ -> collid

let update_collid_second state collid =
  match collid with
  | Tile(Yellow,_,t_o) as tile ->
       tile |> (update_max_ticks 0.80)
  | _ -> collid
