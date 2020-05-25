open Object
   
type viewport = {
  pos: Object.xy;
  v_dim: Object.xy;
}

let make (vx,vy) = 
  {
    pos = {x = 0; y = 0;};
    v_dim = {x = vx; y = vy};
  }

let in_view vpt collid :bool =
  let obj_pos = (Object.get_obj collid).pos in
  not (obj_pos.x < vpt.pos.x || obj_pos.x > vpt.pos.x + vpt.v_dim.x
    || obj_pos.y < vpt.pos.y || obj_pos.y > vpt.pos.y + vpt.v_dim.y)

let translate_debug_pt vpt debug_pt =
  match debug_pt with
  | None -> None
  | Some pt ->
    Some { pt with y = vpt.pos.y + vpt.v_dim.y - pt.y }

let translate_coords vpt collid =
  let obj_st = Object.get_obj collid in
  let obj_pos = obj_st.pos in
  let dy = snd (Object.get_sprite collid).params.frame_size in
  let new_pos = { obj_st.pos with
                  y = vpt.pos.y + vpt.v_dim.y - obj_st.pos.y - dy } in
  let new_debug_pt = translate_debug_pt vpt obj_st.debug_pt in
  match collid with
  | Player(plt, s, o) -> Player(plt, s, { o with pos = new_pos;
                                                 debug_pt = new_debug_pt;
                           })
  | Tile(tt, s, o) -> Tile(tt, s, { o with pos = new_pos })

let filter_into_view vpt collids =
  collids |> List.filter (fun collid -> in_view vpt collid)
          |> List.map (fun collid -> translate_coords vpt collid)

