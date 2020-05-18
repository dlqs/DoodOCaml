open Js_of_ocaml
open Actors
open Object

(*Note: Canvas is 512 by 256 (w*h) -> 32 by 16 blocks*)

(*Holds obj typ and its coordinates. (int, (x-coord, y-coord))*)
type obj_coord =  int * (float * float)

(*Checks if the given location checkloc is already part of the list of locations
* in loclist.*)
let rec mem_loc (checkloc: float * float) (loclist: obj_coord list) : bool =
  match loclist with
  |[] -> false
  |h::t -> if (checkloc = (snd h)) then true
           else mem_loc checkloc t

(*Converts list of locations from blocksize to pixelsize by multiplying (x,y) by
* 16.*)
let rec convert_list (lst:obj_coord list) :obj_coord list =
  match lst with
  |[] -> []
  |(h::t) -> [(fst h, ((fst (snd h))*.16.,(snd (snd h))*.16.))]@(convert_list t)

(*Chooses what type of enemy should be instantiated given typ number*)
let choose_enemy_typ (typ:int) : enemy_typ =
  match typ with
  |0 -> RKoopa
  |1 -> GKoopa
  |2 -> Goomba
  |_ -> failwith "Invalid enemy type"

(*Chooses what type of enemy should be instantiated given typ number*)
let choose_tile_typ (typ:int) : tile_typ =
  match typ with
  |0 -> Green
  |_ -> failwith "Invalid tile type"

(*Chooses what type of block should be instantiated given typ number*)
let choose_sblock_typ (typ:int) : block_typ =
  match typ with
  |0 -> Brick
  |1 -> UnBBlock
  |2 -> Cloud
  |3 -> QBlock Mushroom
  |4 -> Ground
  |_ -> failwith "Invalod sblock type"

(*Optimizes lst such that there are no two items in the list that have the same
* coordinates. If there is one, it is removed.*)
let rec avoid_overlap (lst:obj_coord list) (currentLst:obj_coord list)
                      : obj_coord list =
  match lst with
  |[] -> []
  |h::t -> if(mem_loc (snd h) currentLst) then avoid_overlap t currentLst
           else [h]@(avoid_overlap t currentLst)

(*Gets rid of objects with coordinates in the ending frame, within 128 pixels of
* the start, at the very top, and two blocks from the ground.*)
let rec trim_edges (lst: obj_coord list) (blockw:float) (blockh: float)
                   : obj_coord list =
  match lst with
  |[] -> []
  |h::t -> let cx = fst(snd h) in
           let cy = snd(snd h) in
           let pixx = blockw*.16. in
           let pixy = blockh*.16. in
           if(cx<128. || pixx-.cx<528. || cy = 0. || pixy-.cy<48.)
            then trim_edges t blockw blockh
           else [h]@trim_edges t blockw blockh

(*Generates a stair formation with block typ being dependent on typ. This type
* of stair formation requires that the first step be on the ground.*)
let generate_ground_stairs cbx cby typ =
  let four = [(typ, (cbx, cby));(typ, (cbx+.1., cby));(typ, (cbx+.2., cby));
             (typ, (cbx+.3., cby))] in
  let three = [(typ,(cbx +. 1., cby -. 1.));(typ,(cbx +. 2., cby -. 1.));
              (typ,(cbx +. 3., cby -. 1.))] in
  let two = [(typ,(cbx +. 2., cby -. 2.));(typ,(cbx +. 3., cby -. 2.))] in
  let one = [(typ,(cbx +. 3., cby -. 3.))] in
  four@three@two@one

(*Generates a stair formation going upwards.*)
let generate_airup_stairs cbx cby typ =
  let one = [(typ,(cbx, cby));(typ,(cbx +. 1., cby))] in
  let two = [(typ,(cbx +. 3., cby -. 1.));(typ,(cbx +. 4., cby -. 1.))] in
  let three = [(typ,(cbx +. 4., cby -. 2.));(typ,(cbx +. 5., cby -. 2.));
              (typ,(cbx +. 6., cby -. 2.))] in
  one@two@three

(*Generates a stair formation going downwards*)
let generate_airdown_stairs cbx cby typ =
  let three = [(typ,(cbx, cby));(typ,(cbx +. 1., cby));(typ,(cbx +. 2., cby))]in
  let two = [(typ,(cbx +. 2., cby +. 1.));(typ,(cbx +. 3., cby +. 1.))] in
  let one = [(typ,(cbx +. 5., cby +. 2.));(typ,(cbx +. 6., cby +. 2.))] in
  three@two@one

(*Generates a cloud block platform with some length num.*)
let rec generate_clouds cbx cby typ num =
  if(num = 0) then []
  else [(typ,(cbx, cby))]@generate_clouds (cbx+.1.) cby typ (num-1)

(*Generates an obj_coord list (typ, coordinates) of coins to be placed.*)
let rec generate_coins (block_coord: obj_coord list) : obj_coord list =
  let place_coin = Random.int 2 in
  match block_coord with
  |[] -> []
  |h::t ->  if(place_coin = 0) then
              let xc = fst(snd h) in
              let yc = snd(snd h) in
              [(0,(xc,(yc-.16.)))]@generate_coins t
            else generate_coins t

(*Chooses the form of the blocks to be placed.
* When called, leaves a 1 block gap from canvas size.
* 1. If current xblock or yblock is greater than canvas width or height
*    respectively, return an empty list.
* 2. If current xblock or yblock is within 10 blocks of the left and right sides
*    of the level map, prevent any objects from being initialized.
* 3. Else call helper methods to created block formations and return obj_coord
*    list.
**)
let choose_block_pattern (blockw:float) (blockh: float) (x:float) (y:float)
                         (prob:int) : obj_coord list=
  let block_typ = Random.int 4 in
  let stair_typ = Random.int 2 in
  let life_block_chance = Random.int 5 in
  let middle_block = if(life_block_chance = 0) then 3 else stair_typ in
  let obj_coord =
  match prob with
  |1 -> let num_clouds = (Random.int 5) + 5 in
        if(x < 5.) then generate_clouds x y 2 num_clouds
        else []
  |_ -> failwith "Shouldn't reach here" in
  obj_coord

(*Generates a list of enemies to be placed on the ground.*)
let rec generate_enemies (blockw: float) (blockh: float) (cbx: float)
                    (cby: float) (acc: obj_coord list) =
  if(cbx > (blockw-.32.)) then []
  else if (cby > (blockh-. 1.) ||  cbx < 15.) then
    generate_enemies blockw blockh (cbx +. 1.) 0. acc
  else if(mem_loc (cbx, cby) acc || cby = 0.) then
    generate_enemies blockw blockh cbx (cby+.1.) acc
  else
    let prob = Random.int 30 in
    let enem_prob = 3 in
      if(prob < enem_prob && (blockh -. 1. = cby)) then
        let enemy = [(prob,(cbx*.16.,cby*.16.))] in
        enemy@(generate_enemies blockw blockh cbx (cby+.1.) acc)
      else generate_enemies blockw blockh cbx (cby+.1.) acc

(*Generates a list of enemies to be placed upon the block objects.*)
let rec generate_block_enemies (block_coord: obj_coord list) : obj_coord list =
  let place_enemy = Random.int 20 in
  let enemy_typ = Random.int 3 in
  match block_coord with
  |[] -> []
  |h::t ->  if(place_enemy = 0) then
              let xc = fst(snd h) in
              let yc = snd(snd h) in
              [(enemy_typ,(xc,(yc-.16.)))]@generate_block_enemies t
            else generate_block_enemies t

(*Generates an obj_coord list (typ, coordinates) of blocks to be placed.*)
let rec generate_block_locs (blockw: float) (blockh: float)
          (x: float) (y: float) (y1: float) (acc: obj_coord list) : obj_coord list =
  if(blockw< x +. 33.) then acc
  else if (y >= y1) then
    generate_block_locs blockw blockh (x +. 1.) 0. y1 acc
  else if mem_loc (x, y) acc then
    generate_block_locs blockw blockh x (y +. 1.) y1 acc
  else
    let prob = Random.int 100 in
    let block_prob = 5 in
      if(prob < block_prob) then
        let newacc = choose_block_pattern blockw blockh x y prob in
        let undup_lst = avoid_overlap newacc acc in
        let called_acc = acc@undup_lst in
        generate_block_locs blockw blockh x (y +. 1.) y1 called_acc
      else generate_block_locs blockw blockh x (y +. 1.) y1 acc
  
(*Generates the ending item panel at the end of the level. Games ends upon
* collision with player.*)
let generate_panel (context:Dom_html.canvasRenderingContext2D Js.t)
                   (blockw: float) (blockh: float) : collidable =
  let ob = Object.spawn (SBlock Panel) context
    ((blockw*.16.)-.256., (blockh *. 16.)*.2./.3.) in
  ob

(*Generates the list of brick locations needed to display the ground.
* 1/10 chance that a ground block is skipped each call to create holes.*)
let rec generate_ground (blockw:float) (blockh:float) (inc:float)
                        (acc: obj_coord list) : obj_coord list =
  if(inc > blockw) then acc
  else
    let newacc = acc@[(4, (inc*. 16.,blockh *. 16.))] in
    generate_ground blockw blockh (inc +. 1.) newacc

let rec convert_to_tile_obj (lst:obj_coord list)
  (context:Dom_html.canvasRenderingContext2D Js.t) : collidable list =
  match lst with
  |[] -> []
  |h::t ->
    let stile_typ = choose_tile_typ 0 in
    let ob = Object.spawn (STile stile_typ) context (snd h) in
    [ob]@(convert_to_tile_obj t context)

(*Converts the obj_coord list called by generate_block_locs to a list of objects
* with the coordinates given from the obj_coord list. *)
let rec convert_to_block_obj (lst:obj_coord list)
  (context:Dom_html.canvasRenderingContext2D Js.t) : collidable list =
  match lst with
  |[] -> []
  |h::t ->
    let sblock_typ = choose_sblock_typ (fst h) in
    let ob = Object.spawn (SBlock sblock_typ) context (snd h) in
    [ob]@(convert_to_block_obj t context)

(*Converts the obj_coord list called by generate_enemies to a list of objects
* with the coordinates given from the obj_coord list. *)
let rec convert_to_enemy_obj (lst:obj_coord list)
            (context:Dom_html.canvasRenderingContext2D Js.t) : collidable list =
  match lst with
  |[] -> []
  |h::t ->
    let senemy_typ = choose_enemy_typ (fst h) in
    let ob = Object.spawn (SEnemy senemy_typ) context (snd h) in
    [ob]@(convert_to_enemy_obj t context)

(*Converts the list of coordinates into a list of Coin objects*)
let rec convert_to_coin_obj (lst:obj_coord list)
            (context:Dom_html.canvasRenderingContext2D Js.t) : collidable list =
  match lst with
  |[] -> []
  |h::t ->
    let sitem_typ = Coin in
    let ob = Object.spawn (SItem sitem_typ) context (snd h) in
    [ob]@(convert_to_coin_obj t context)

(*Procedurally generates a list of collidables given canvas width, height and
* context. Arguments block width (blockw) and block height (blockh) are in
* block form, not pixels.
let generate_helper (blockw:float) (blockh:float) (cx:float) (cy:float)
            (context:Dom_html.canvasRenderingContext2D Js.t) : collidable list =
  let block_locs = generate_block_locs blockw blockh 0. 0. [] in
  let converted_block_locs = trim_edges (convert_list block_locs)
    blockw blockh in
  let obj_converted_block_locs = convert_to_block_obj converted_block_locs
    context in
  let ground_blocks = generate_ground blockw blockh 0. [] in
  let obj_converted_ground_blocks = convert_to_block_obj ground_blocks
    context in
  let all_blocks = obj_converted_block_locs@obj_converted_ground_blocks in
  let obj_panel = generate_panel context blockw blockh in
  all_blocks@[obj_panel]*)

let generate_helper (blockw:float) (blockh:float) (cx:float) (cy:float)
            (context:Dom_html.canvasRenderingContext2D Js.t) : collidable list =
  let ground_blocks = generate_ground blockw blockh (0.) [] in
  let obj_converted_ground_blocks = convert_to_block_obj ground_blocks
    context in
  obj_converted_ground_blocks

(*Main function called to procedurally generate the level map. w and h args
* are in pixel form. Converts to block form to call generate_helper. Spawns
* the list of collidables received from generate_helper to display on canvas.*)
let generate_initial (w:float) (base:float)
                    (context:Dom_html.canvasRenderingContext2D Js.t) :
                    (collidable * collidable list) =
  Random.self_init();
  let blockw = w/.16. in
  let blockh = (256./.16.) -. 1. in
  let ground_blocks = generate_ground blockw blockh (0.) [] in
  let obj_converted_ground_blocks = convert_to_block_obj ground_blocks context in
  let player = Object.spawn (SPlayer(SmallM,Standing)) context (w /. 2., base) in
  (player, obj_converted_ground_blocks)


let rec generate_tile_layer (currW:float) (limitW:float) (currY:float) (acc: obj_coord list)
        : obj_coord list =
  if (currW +. 8. >= limitW) then acc else
  let tile = (5, (currW, currY)) in
  generate_tile_layer (currW +. 40.) limitW currY (tile::acc)

(* Generate tiles between baseY and baseY+offsetY *)
let continually_generate (cw:float)(baseY:float) (offsetY:float)
      (context:Dom_html.canvasRenderingContext2D Js.t)
  : collidable list = 
  let rec generate_layer currY limitY (acc:obj_coord list) =
    if currY >= limitY then acc else
      let tiles = generate_tile_layer 0. cw currY [] in
      generate_layer (currY +. 16.) limitY tiles@acc
  in let objs = generate_layer baseY (baseY +. offsetY) [] in
  (convert_to_tile_obj objs context)
