open Types

let obj_blank = 50
let obj_height_apart = 20

type item_prob = {
  ba: int;
  he: int;
}

let get_prob ?ba:(ba=0) ?he:(he=0) () =
  assert ((ba + he ) = 100);
  { ba; he; }

let choose_object prob currY widthX =
  let r = 1 + Random.int 100 in
  let bac = prob.ba in
  let hec = bac + prob.he in
  let obj = match r with
            | q when q <= bac -> Object.make_obst Barrier { x = 0; y = 0 }
            | q when q <= hec -> Object.make_item Health { x = 0; y = 0 }
            | _ -> failwith "probabilities don't add up to 100" in
  let maxX = widthX - fst (Object.get_sprite obj).params.bbox_size in
  let pos = { y = currY; x = Random.int maxX } in
  Object.update ~pos obj

let generate_object startY endY widthX  ~ba ~he =
  let prob = get_prob ~ba ~he () in
  let rec generate_helper currY objects =
    if currY + obj_height_apart >= endY then objects else
    let r = Random.int 100 in
    let blank_prob = 75 in
    if r > blank_prob then
      let obj = choose_object prob currY widthX in
      let obj_height = snd (Object.get_sprite obj).params.bbox_size in
      generate_helper (currY + obj_height + obj_height_apart) (obj::objects)
    else
      generate_helper (currY + obj_blank + obj_height_apart) objects
  in
  generate_helper startY []


let generate (state:state) : collidable list =
  let startY = state.last_generated_height in
  let endY = state.next_generated_height in
  let widthX = state.vpt.dim.x in
  match state.next_generated_height with
  | y when y < 1025 -> generate_object startY endY widthX ~ba:80 ~he:20
  | y when y < 2049 -> generate_object startY endY widthX ~ba:80 ~he:20
  | _ -> generate_object startY endY widthX ~ba:80 ~he:20

let generate_debug =
  [
    Object.make_obst Barrier { x = 40; y = 40; };
    Object.make_item Health { x = 40; y = 80; };
  ]
