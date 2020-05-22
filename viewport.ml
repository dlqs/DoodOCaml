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

let translate_coords vpt collid =
  let obj_pos = (Object.get_obj collid).pos in
  let obj_dim = (Object.get_sprite collid).params.frame_size in
  let new_pos = { x = obj_pos.x;
                  y = vpt.pos.y + vpt.v_dim.y - obj_pos.y - snd obj_dim } in
  match collid with
  | Player(plt, s, o) -> Player(plt, s, { o with pos = new_pos })
  | Tile(tt, s, o) -> Tile(tt, s, { o with pos = new_pos })

let filter_into_view vpt collids =
  collids |> List.filter (fun collid -> in_view vpt collid)
          |> List.map (fun collid -> translate_coords vpt collid)

