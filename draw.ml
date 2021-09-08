open Js_of_ocaml
open Chess_board
open Tile
module Html = Dom_html

type o =
  | Coor of int
  | Unselected

type selection = {
  mutable x : o;
  mutable y : o;
  mutable empty : bool;
}

let sel1 = { x = Unselected; y = Unselected; empty = true }

let sel2 = { x = Unselected; y = Unselected; empty = true }

let counter = ref 0

let b = init

let () = initialize b (string_to_lists starterboard) 0

let img src =
  let i = Dom_html.createImg Dom_html.document in
  i##.src := Js.string src;
  i

let clear_canvas (c : Dom_html.canvasElement Js.t) =
  let ctx = c##getContext Html._2d_ in
  ctx##clearRect 0.0 0.0
    (float_of_int c##.width)
    (float_of_int c##.height)

let draw_piece
    (c : Dom_html.canvasElement Js.t)
    (x : float)
    (y : float)
    (b : board) =
  let ctx = c##getContext Html._2d_ in
  match get_img b.(int_of_float x).(int_of_float y) with
  | Image a -> ctx##drawImage (img a) (y *. 60.0) (x *. 60.0)
  | No -> ()

let rec draw_row_pieces
    (c : Dom_html.canvasElement Js.t)
    (x : float)
    (y : float)
    (b : board) : unit =
  match x with
  | 8.0 -> ()
  | a ->
      draw_piece c a y b;
      draw_row_pieces c (a +. 1.0) y b

let rec draw_pieces_helper
    (c : Dom_html.canvasElement Js.t)
    (y : float)
    board : unit =
  match y with
  | 8.0 -> ()
  | a ->
      draw_row_pieces c 0.0 a board;
      draw_pieces_helper c (a +. 1.0) board

let rec draw_pieces (c : Dom_html.canvasElement Js.t) (b : board) =
  draw_pieces_helper c 0.0 b

let rec draw_row
    (c : Dom_html.canvasElement Js.t)
    (x : float)
    (y : float) =
  let ctx = c##getContext Html._2d_ in
  let () = ctx##beginPath in
  let () = ctx##rect (x *. 60.0) (y *. 60.0) 60.0 60.0 in
  let () =
    if int_of_float (x +. y) mod 2 = 0 then
      ctx##.fillStyle := Js.string "CornflowerBlue"
    else ctx##.fillStyle := Js.string "GhostWhite"
  in
  ctx##fill;
  if x >= 0.0 then draw_row c (x -. 1.0) y else ()

let rec draw_board (c : Dom_html.canvasElement Js.t) (y : float) =
  draw_row c 7.0 y;
  if y >= 0.0 then draw_board c (y -. 1.0)

let draw_outline (c : Dom_html.canvasElement Js.t) =
  let ctx = c##getContext Html._2d_ in
  let () = ctx##beginPath in
  let () = ctx##rect 0.0 0.0 480.0 480.0 in
  ctx##stroke

let draw_cursor (c : Dom_html.canvasElement Js.t) (s : selection) =
  let ctx = c##getContext Html._2d_ in
  match (s.x, s.y) with
  | Coor x, Coor y ->
      let _ = ctx##beginPath in
      let _ =
        ctx##rect
          (float_of_int x *. 60.0)
          (float_of_int y *. 60.0)
          60.0 60.0
      in
      let _ = ctx##.fillStyle := Js.string "Navy" in
      ctx##fill;
      let _ = ctx##beginPath in
      let _ =
        if (x + y) mod 2 = 0 then
          ctx##.fillStyle := Js.string "CornflowerBlue"
        else ctx##.fillStyle := Js.string "GhostWhite"
      in
      let _ =
        ctx##rect
          ((float_of_int x *. 60.0) +. 4.0)
          ((float_of_int y *. 60.0) +. 4.0)
          52.0 52.0
      in
      ctx##fill
  | _, _ -> ()

let draw_check (c : Dom_html.canvasElement Js.t) color s =
  let ctx = c##getContext Html._2d_ in
  let _ = ctx##.fillStyle := Js.string color in
  let _ = ctx##.font := Js.string "30px Arial" in
  ctx##fillText (Js.string s) 190.0 252.0

let get_coor (s : o) =
  match s with Coor x -> x | Unselected -> failwith "notfound"

let check b x y x2 y2 c =
  if
    check_validity b (get_coor sel1.y) (get_coor sel1.x)
      (get_coor sel2.y) (get_coor sel2.x) c
  then
    let a =
      move_piece b (get_coor sel1.y) (get_coor sel1.x) (get_coor sel2.y)
        (get_coor sel2.x)
    in
    if a then counter := !counter + 1 else ()
  else ()

let rec update_loop (c : Dom_html.canvasElement Js.t) : unit =
  clear_canvas c;
  draw_board c 7.0;
  if sel1.empty = false && sel2.empty = false then (
    check b (get_coor sel1.y) (get_coor sel1.x) (get_coor sel2.y)
      (get_coor sel2.x) !counter;
    let _ = sel1.empty <- true in
    sel2.empty <- true)
  else draw_cursor c sel1;
  draw_pieces c b;
  draw_outline c;
  let bl = in_check Tile.Black in
  let wh = in_check Tile.White in
  if is_checkmate b && bl then draw_check c "light" "White Wins"
  else if is_checkmate b && wh then draw_check c "black" "Black wins"
  else if bl then draw_check c "black" "Check"
  else if wh then draw_check c "lightgrey" "Check"

let onclick c (evt : #Dom_html.mouseEvent Js.t) =
  let rect = c##getBoundingClientRect in
  let _ =
    if sel1.empty = true then
      let _ =
        sel1.x <- Coor ((evt##.clientX - int_of_float rect##.left) / 60)
      in
      let _ =
        sel1.y <- Coor ((evt##.clientY - int_of_float rect##.left) / 60)
      in
      sel1.empty <- false
    else
      let _ =
        sel2.x <- Coor ((evt##.clientX - int_of_float rect##.left) / 60)
      in
      let _ =
        sel2.y <- Coor ((evt##.clientY - int_of_float rect##.left) / 60)
      in
      sel2.empty <- false
  in
  let _ = update_loop c in
  Js._true

let load _ =
  let canvas_id = "canvas" in
  let canvas =
    Js.Opt.get
      (Js.Opt.bind
         (Html.document##getElementById (Js.string canvas_id))
         Html.CoerceTo.canvas)
      (fun () ->
        Printf.printf "cant find canvas %s \n" canvas_id;
        failwith "fail")
  in
  let _ =
    Html.addEventListener Html.document Html.Event.click
      (Html.handler (onclick canvas))
      Js._true
  in
  update_loop canvas

let run = load ()
