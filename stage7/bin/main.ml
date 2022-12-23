open Core
module S = Stdlib

(* Parse all the stuff *)
let input = "./input"
let testinput = "./testinput"
let testinput2 = "./testinput2"

let drop_last l =
  let open Stdlib in
  l |> List.rev |> List.tl |> List.rev

let lines_of_file file =
  let contents = S.In_channel.with_open_bin file In_channel.input_all in
  drop_last @@ S.String.split_on_char '\n' contents

type parse_result = Dir of string | File of { name : string; size : int }

exception ParseError

let parse_cmd c : parse_result option =
  (* this is either a file name and size or a direectory name *)
  let open String in
  let l = length c in
  let first = sub c 0 1 in
  match first with
  | "$" -> (
      (*     print_string "command: ";  *)
      match sub c 2 1 with
      | "c" -> (
          (*       print_string "change dir ";  *)
          match sub c 5 (l - 5) with s -> (* print_endline s; *) Some (Dir s))
      | _ -> (* print_endline "not change dir"; *) None)
  | _ -> (
      (*     print_string "not a command: ";  *)
      match sub c 0 l with
      | s -> (
          match split ~on:' ' s with
          | [ l; r ] ->
              if String.contains l 'd' then None
              else Some (File { name = r; size = int_of_string l })
          | _ -> (* print_endline "bad filename"; *) None)
      | _ -> (* print_endline "unrecognized"; *) None)

let stack_concat_dir s dir =
  let l = Stack.to_list s in
  let s = List.fold (List.rev l) ~init:"" ~f:(fun x y -> x ^ y) in
  s ^ dir

let rec count_fs fs_list counts_tbl dir_stack =
  let module Ht = Hashtbl in
  let module St = Stack in
  let count cmd counts ds =
    match cmd with
    | Dir dir -> (
        match dir with
        (* need to pop the stack here *)
        | ".." -> ignore @@ St.pop_exn ds
        (* need to create a tbl entry and
           add the current dir to the stack here *)
        | d ->
            Ht.add_exn counts ~key:(stack_concat_dir ds d) ~data:0;
            St.push ds (stack_concat_dir ds d))
    | File { name; size } ->
        (* need to iter through the add stack and add current file size
           to each of those directory totals in the coutns table *)
        let update_tbl key add_size =
          Ht.update counts key ~f:(fun x ->
              match x with
              | Some value -> value + add_size
              | None -> failwith ("hash not existent" ^ name))
        in
        St.iter ds ~f:(fun k -> update_tbl k size)
  in

  match fs_list with
  | [] -> ()
  | x :: xs ->
      count x counts_tbl dir_stack;
      count_fs xs counts_tbl dir_stack

let counts = Hashtbl.create (module String)
let (dir_stack : string Stack.t) = Stack.create ()
let cmds = lines_of_file input
let fs = List.filter (List.map cmds parse_cmd) Option.is_some
let fs_clean = List.map fs (fun x -> Option.value ~default:(Dir "broken") x)
let _ = count_fs fs_clean counts dir_stack

let counts_list =
  Hashtbl.to_alist counts
  |> List.map ~f:(fun (_, y) -> y)
  |> List.filter ~f:(fun x -> if x <= 100000 then true else false)
  |> List.fold ~init:0 ~f:(fun x y -> x + y)
