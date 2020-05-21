open Js_of_ocaml
open Sprite
module Html = Dom_html
let jstr = Js.string

let render collids context =
  List.iter(fun collid ->
      let sprite = Object.get_sprite collid in
      let pos = (Object.get_obj collid).pos in
      let (sx, sy) = sprite.params.src_offset in
      let (sw, sh) = sprite.params.frame_size in
      let (dx, dy) = (pos.x,pos.y) in
      let (dw, dh) = sprite.params.frame_size in
      let sx = sx + (!(sprite.frame)) * sw in
      (*print_endline (string_of_int !(sprite.frame));*)
      (*context##clearRect(0.,0.,sw, sh);*)
      context##drawImage_full (sprite.img) sx sy sw sh dx dy dw dh
    )

(*Used for animation updating. Canvas is cleared each frame and redrawn.*)
let clear_canvas canvas =
  let context = canvas##getContext (Dom_html._2d_) in
  let cwidth = float_of_int canvas##.width in
  let cheight = float_of_int canvas##.height in
  ignore (context##clearRect 0 0 cwidth cheight)


let _draw_background_color canvas = failwith "todo"
let _debug f = Printf.ksprintf (fun s -> Firebug.console##log (jstr s)) f
let _alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith "poo") f

