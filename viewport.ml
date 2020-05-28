open Types
open Object

let pl_y_offset_ratio = 0.35

let make (vx,vy) = 
  {
    pos = {x = 0; y = 0;};
    dim = {x = vx; y = vy};
  }

let in_view (vpt:viewport) (collid:collidable) : bool =
  let obj_pos = (Object.get_obj collid).pos in
  not (obj_pos.x < vpt.pos.x || obj_pos.x > vpt.pos.x + vpt.dim.x
    || obj_pos.y < vpt.pos.y || obj_pos.y > vpt.pos.y + vpt.dim.y)

let translate_debug_pt (vpt:viewport) (debug_pt:xy option) : xy option =
  match debug_pt with
  | None -> None
  | Some pt ->
    Some { pt with y = vpt.pos.y + vpt.dim.y - pt.y }

let translate_coords (vpt:viewport) (collid:collidable) : collidable  =
  let obj_st = Object.get_obj collid in
  let obj_pos = obj_st.pos in
  let dy = snd (Object.get_sprite collid).params.frame_size in
  let new_pos = { obj_st.pos with
                  y = vpt.pos.y + vpt.dim.y - obj_st.pos.y - dy } in
  let new_debug_pt = translate_debug_pt vpt obj_st.debug_pt in
  match collid with
  | Player(plt, s, o) -> Player(plt, s, { o with pos = new_pos;
                                                 debug_pt = new_debug_pt;
                           })
  | Tile(tt, s, o) -> Tile(tt, s, { o with pos = new_pos })

let filter_into_view vpt collids =
  collids |> List.filter (fun collid -> in_view vpt collid)
          |> List.map (fun collid -> translate_coords vpt collid)

let move (vpt:viewport) (player:collidable) : viewport =
  let botY = (get_obj player).pos.y - (int_of_float (
                                           float_of_int vpt.dim.y *. pl_y_offset_ratio
                                      )) in
  let y = max vpt.pos.y botY in
  { vpt with pos = { vpt.pos with y }}

