open Core
module S = Stdlib

(* Parse all the stuff *)
let input = "./input"
(* let testinput = "./testinput" *)

let drop_last l =
  let open Stdlib in
  l |> List.rev |> List.tl |> List.rev

let lines_of_file file =
  let contents = S.In_channel.with_open_bin file In_channel.input_all in
  drop_last @@ S.String.split_on_char '\n' contents

let lines = lines_of_file input
let _ = print_endline @@ Option.value ~default:"hello" @@ List.hd lines
