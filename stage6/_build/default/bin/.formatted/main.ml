open Core
module S = Stdlib

(* Parse all the stuff *)
let input = "./input"
let inputstacks = "./inputstacks"
let testinput = "./testinput"
let testinputstacks = "./testinputstacks"

let drop_last l =
  let open Stdlib in
  l |> List.rev |> List.tl |> List.rev

let lines_of_file file =
  let contents = S.In_channel.with_open_bin file In_channel.input_all in
  drop_last @@ S.String.split_on_char '\n' contents

type move = { count : int; from : int; onto : int }

let parse_move_string ms =
  let open List in
  let words = String.split ~on:' ' ms in
  let c, f, t = (nth_exn words 1, nth_exn words 3, nth_exn words 5) in
  let ios = int_of_string in
  { count = ios c; from = ios f; onto = ios t }

let execute_move m sl =
  (*   TODO *)
  let open List in
  match m with
  | { count = c; from = f; onto = t } ->
      for i = 0 to c - 1 do
        let item = Stack.pop_exn (nth_exn sl (f - 1)) in
        Stack.push (nth_exn sl (t - 1)) item
      done

let stacklist = lines_of_file inputstacks |> List.map ~f:(String.split ~on:' ')
let stacks = List.map ~f:Stack.of_list stacklist
let rawmoves = lines_of_file input
let moves = List.map rawmoves ~f:parse_move_string
let _ = List.iter moves ~f:(fun x -> execute_move x stacks)

let tops =
  List.init (List.length stacks) ~f:(fun i ->
      Stack.pop_exn (List.nth_exn stacks i))

let sol = List.fold tops ~init:"" ~f:(fun x y -> x ^ y)
(*
let lines = lines_of_file input
let _ = print_endline @@ Option.value ~default:"hello" @@ List.hd lines
*)
