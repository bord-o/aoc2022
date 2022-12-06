open Core
module S = Stdlib

let input = "./input"
let testinput = "./testinput"

let lines_of_file file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let drop_last l =
  let open Stdlib in
  l |> List.rev |> List.tl |> List.rev

