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

type fs_details = { name : string; size : int }

type dir_details = { name : string; size : int; tree : fs_object list }
and fs_object = Dir of dir_details | File of fs_details

let rec construct_fs cmds = match cmds with [] -> [] | _ -> []
let cmds = lines_of_file testinput
let fs = construct_fs cmds
