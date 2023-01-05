open Core
module S = Stdlib

(* Parse all the stuff *)
let input = "./input"
let testinput = "./testinput"

let drop_last l =
  let open Stdlib in
  l |> List.rev |> List.tl |> List.rev

let lines_of_file file =
  let contents = S.In_channel.with_open_bin file In_channel.input_all in
  drop_last @@ S.String.split_on_char '\n' contents

let rec flatten ll = match ll with [] -> [] | x :: xs -> x @ flatten xs

type parse_result = Up of int | Down of int | Left of int | Right of int
[@@deriving show]

exception Invalid_input

let parse_cmd c : parse_result option =
  (* this is either a file name and size or a direectory name *)
  let open String in
  let l = length c in
  let first = sub c 0 1 in
  let last = int_of_string @@ sub c 2 1 in
  match first with
  | "U" -> Some (Up last)
  | "D" -> Some (Down last)
  | "L" -> Some (Left last)
  | "R" -> Some (Right last)
  | _ -> None

type command = Up' | Down' | Left' | Right' [@@deriving show]

let expand_cmd cmd =
  match cmd with
  | Up mag -> List.init mag ~f:(fun _ -> Up')
  | Down mag -> List.init mag ~f:(fun _ -> Down')
  | Left mag -> List.init mag ~f:(fun _ -> Left')
  | Right mag -> List.init mag ~f:(fun _ -> Right')

let print_mat mat x y =
  let open Array in
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      let num = mat.(i).(j) in
      print_string (if num = 0 then ". " else "# ");
      ()
    done;
    print_newline ()
  done

let todo s = Printf.printf "unimplemented: %s\n" s

type position = { x : int; y : int }

let update_tail board start_pos =
  let h, t = start_pos in
  let dx, dy = (h.x - t.x, h.y - t.y) in


  if dx = 1 && dy = 2 then { x = succ t.x; y = succ t.y }
  else if dx = -1 && dy = 2 then { x = pred t.x; y = succ t.y }
  else if dx = 1 && dy = -2 then { x = succ t.x; y = pred t.y }
  else if dx = -1 && dy = -2 then { x = pred t.x; y = pred t.y }
  else if dx = 2 && dy = 1 then { x = succ t.x; y = succ t.y }
  else if dx = 2 && dy = -1 then { x = succ t.x; y = pred t.y }
  else if dx = -2 && dy = 1 then { x = pred t.x; y = succ t.y }
  else if dx = -2 && dy = -1 then { x = pred t.x; y = pred t.y }
  else if dx = 2 then { t with x = succ t.x }
  else if dx = -2 then { t with x = pred t.x }
  else if dy = 2 then { t with y = succ t.y }
  else if dy = -2 then { t with y = pred t.y }
  else (
    Printf.printf "dx: %i, dy: %i\n" dx dy;
    t)

let step_sim board start_pos cmd =
  (* this is functional except for the board update *)
  Printf.printf "%s  : " @@ show_command cmd;
  let hpos, tpos = start_pos in
  (*   let () = print_mat board 6 6 in *)
  let new_tpos = update_tail board (hpos, tpos) in
  let new_hpos =
    match cmd with
    | Up' -> { hpos with x = pred hpos.x }
    | Down' -> { hpos with x = succ hpos.x }
    | Left' -> { hpos with y = pred hpos.y }
    | Right' -> { hpos with y = succ hpos.y }
  in

  board.(new_tpos.x).(new_tpos.y) <- 1;
  (new_hpos, new_tpos)

let add_board b x y =
  let open Array in
  let acc = ref 0 in

  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      let num = b.(i).(j) in
      if num = 0 then () else acc := succ !acc;
      if num <> 0 && num <> 1 then print_endline "ZZZZZZZ"
    done
  done;
  !acc

let wsize = 12
let head_board = Array.make_matrix wsize wsize 0
let _ = head_board.(wsize / 2).(wsize / 2) <- 1
let lines = lines_of_file testinput
let cmds_dirty = List.filter_map lines ~f:parse_cmd
let cmds = flatten @@ List.map cmds_dirty ~f:expand_cmd

(* let _ = print_mat head_board wsize wsize *)
let _ =
  List.fold cmds
    ~init:({ x = wsize / 2; y = wsize / 2 }, { x = wsize / 2; y = wsize / 2 })
    ~f:(fun pos cmd -> step_sim head_board pos cmd)

(* let _ = List.iter cmds ~f:(fun x -> step_sim board { x = 5; y = 5 } x) *)
let _ = print_mat head_board wsize wsize
let () = Printf.printf "\n%i\n" @@ add_board head_board wsize wsize

(*
let _ = step_sim board {x=15; y=15} (List.hd_exn cmds)
*)
