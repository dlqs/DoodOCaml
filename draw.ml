open Js_of_ocaml
open Types

let jstr = Js.string
let fi = float_of_int

let render_debug_pt context collid (debug_pt: xy option) =
  let center = Object.get_aabb_center collid in
  match debug_pt with
  | None -> ()
  | Some pt -> context##beginPath;
               context##moveTo (fi center.x) (fi center.y);
               context##lineTo (fi pt.x) (fi pt.y);
               context##stroke;
               ()
                  
let render state canvas collids =
  List.iter(fun collid ->
      let sprite = Object.get_sprite collid in
      let obj_st = Object.get_obj collid in
      let context = canvas##getContext (Dom_html._2d_) in
      let pos = obj_st.pos in
      let (sx, sy) = sprite.params.src_offset in
      let (sw, sh) = sprite.params.frame_size in
      let (dx, dy) = (pos.x,pos.y) in
      let (dw, dh) = sprite.params.frame_size in
      let sx = sx + (!(sprite.frame)) * sw in
      ignore(context##drawImage_full (sprite.img)
               (fi sx)
               (fi sy)
               (fi sw)
               (fi sh)
               (fi dx)
               (fi dy)
               (fi dw)
               (fi dh)
        );
      if state.draw_bb then
      let (bbox,bboy) = sprite.params.bbox_offset in
      let (bbsx,bbsy) = sprite.params.bbox_size in
      context##.strokeStyle := (Js.string "#FF0000");
      ignore(context##strokeRect
          (fi (dx))
          (fi (dy-bbsy+sh))
          (fi bbsx)
          (fi bbsy)
        );
      ignore(render_debug_pt context collid obj_st.debug_pt);
    ) collids

(*Used for animation updating. Canvas is cleared each frame and redrawn.*)
let clear_canvas canvas =
  let context = canvas##getContext (Dom_html._2d_) in
  let cw = canvas##.width in
  let ch = canvas##.height in
  ignore (context##clearRect
            (fi 0)
            (fi 0)
            (fi cw)
            (fi ch)
    )

let _draw_background_color canvas = failwith "todo"
let _debug f = Printf.ksprintf (fun s -> Firebug.console##log (jstr s)) f
let _alert f = Printf.ksprintf (fun s -> Dom_html.window##alert(Js.string s); failwith "poo") f

