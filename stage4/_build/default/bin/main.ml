open Core
module S = Stdlib

let input = "./input"
(* let testinput = "./testinput" *)

exception Commas of string

let drop_last l =
  let open Stdlib in
  l |> List.rev |> List.tl |> List.rev

let lines_of_file file =
  let contents = S.In_channel.with_open_bin file In_channel.input_all in
  drop_last @@ S.String.split_on_char '\n' contents

let parse_line l =
  let ios = int_of_string in
  let s = String.split ~on:',' l in
  let pair =
    match s with
    | [ l; r ] -> (
        ( (match String.split ~on:'-' l with
          | [ l2; r2 ] -> (ios l2, ios r2)
          | _ -> raise (Commas "too many")),
          match String.split ~on:'-' r with
          | [ l3; r3 ] -> (ios l3, ios r3)
          | _ -> raise (Commas "too many") ))
    | _ -> raise (Commas "too many")
  in
  pair

let encapsulated_pred p =
  match p with
  | lp, rp ->
      let lpl, lpr = match lp with l, r -> (l, r) in
      let rpl, rpr = match rp with l, r -> (l, r) in
      if (lpl >= rpl && lpr <= rpr) || (lpl <= rpl && lpr >= rpr) then true
      else false

let lines = lines_of_file input
let pairs = List.map lines ~f:parse_line
let encaps = List.map pairs ~f:encapsulated_pred
let sol = List.count encaps ~f:(fun x -> x)
let _ = print_endline @@ string_of_int sol
