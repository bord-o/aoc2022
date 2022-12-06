open List
exception Empty

let input = "./input"
let testinput = "./testinput"

let lines_of_file file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let drop_last = function
  [] -> raise Empty
  | l -> l |> List.rev |> List.tl |> List.rev

let compartments_of_line s = 
  let open String in
  let l = length s in
  let left = sub s 0 (l/2) in
  let right = sub s (l/2) (l/2) in
  (left, right)
 
let common_in_pair p =
  let open String in
  let common = ref [] in
  match p with 
  l, r -> (iter (fun chr -> if contains r chr then common := chr::!common else ()) l); !common
  
let score_of_char c = 
(*   lower 96 upper 64 *)
  let charCode = Char.code c in
  if (charCode < 123 && charCode > 96) then
    charCode - 96
  else
    charCode - 38

let lines = lines_of_file input
let compartments = map compartments_of_line lines 
let common = map common_in_pair compartments
let scores = (map hd (drop_last common)) |> map score_of_char
let total = fold_left (fun x y -> x + y) 0 scores

(* let _ = List.iter print_endline (drop_last lines) *)



