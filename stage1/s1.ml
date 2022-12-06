open List

let input = "./input"

let testlist = [
  "1000"
; "2000"
; "3000"
; ""
; "4000"
; ""
; "5000"
; "6000"
; ""
; "7000"
; "8000"
; "9000"
; ""
; "10000"]

let listMaxInt l  = 
  let rec aux l cur =
    match l with
    | [] -> cur
    | x::xs -> aux xs (Int.max x cur)
  in aux l Int.min_int

let rec groupCalories (lines: string list) = 
  let l = length lines in
  let elfnum = ref 0 in
  let elftotals = ref (List.init l (fun _ -> ref 0)) in
  for i=0 to l-1 do
    let elfstring = nth lines i in
    if elfstring <> "" then let n = (nth !elftotals (!elfnum)) in n := !n + (int_of_string elfstring);
    else elfnum := !elfnum + 1;
  done;

  map (fun reference -> !reference) !elftotals

let lines_of_file file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let lines = lines_of_file input
let solution = 
  lines |> groupCalories |> listMaxInt |> print_int
(* List.iter print_endline lines *)

