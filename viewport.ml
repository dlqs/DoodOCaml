open Types
open Object

let pl_y_offset_ratio = 0.35

let make (vx,vy) =
  {
    pos = {x = 0; y = 0;};
    dim = {x = vx; y = vy};
  }

let below_vpt (vpt:viewport) (collid:collidable) : bool =
  let frame_size_y = snd (Object.get_sprite collid).params.frame_size in
  let obj = (Object.get_obj collid) in
  vpt.pos.y > obj.pos.y + frame_size_y

let above_vpt (vpt:viewport) (collid:collidable) : bool =
  let obj = (Object.get_obj collid) in
  obj.pos.y > vpt.pos.y + vpt.dim.y

let in_vpt (vpt:viewport) (collid:collidable) : bool =
  (not (above_vpt vpt collid)) && (not (below_vpt vpt collid))

let translate_debug_pt (vpt:viewport) (debug_pt:xy option) : xy option =
  match debug_pt with
  | None -> None
  | Some pt -> Some { pt with y = vpt.pos.y + vpt.dim.y - pt.y }

let prepare_for_draw (vpt:viewport) (collid:collidable) : collidable  =
  let obj = Object.get_obj collid in
  let frame_size_y = snd (Object.get_sprite collid).params.frame_size in
  let pos = { obj.pos with
              y = vpt.pos.y + vpt.dim.y - obj.pos.y - frame_size_y } in
  let debug_pt = translate_debug_pt vpt obj.debug_pt in
  Object.update ~pos ~debug_pt collid

let move (vpt:viewport) (player:collidable) : viewport =
  let botY = (get_obj player).pos.y - (int_of_float (
                                           float_of_int vpt.dim.y *. pl_y_offset_ratio
                                      )) in
  let y = max vpt.pos.y botY in
  { vpt with pos = { vpt.pos with y }}
