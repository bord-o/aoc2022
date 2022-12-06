
let input = "./input"

let testpairs = [
"A Y"
;"B X"
;"C Z"
]

let lines_of_file file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

type hand = Rock | Paper | Scissors

let evalhand hnd = 
  (* will say score for the player *)
  let winner h = 
    match h with
    (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) -> 6
    | (x, y) when x = y -> 3
    | _ -> 0
  in
  let whichthrow t = 
    match t with 
    (_, Rock) -> 1
    | (_, Paper) -> 2 
    | (_, Scissors) -> 3
  in

  whichthrow hnd + winner hnd

let hands_of_pair p = 
  let code_of_string s = 
    Char.code s.[0]
  in
  let hand_of_code c =
    match c with 
    | 65 | 88 -> Rock 
    | 66 | 89 -> Paper
    | 67 | 90 -> Scissors
    | _ -> failwith "not a valid hand"
  in
  match p with 
  (x, y) -> (x |> code_of_string |> hand_of_code, y |> code_of_string |> hand_of_code)


let parsePairs lines = 
  let open List in
  let parse p = 
    let t = p |> String.split_on_char ' ' |> (fun l -> match l with x::y::[] -> Some(x, y) | _ -> None) in
    t
  in 

  let l = map parse lines in
  l |> filter Option.is_some |> map (fun x -> Option.value ~default:("a","a") x)



let lines = lines_of_file input
let pairs = (parsePairs lines)
let hands = List.map hands_of_pair pairs
let scores = List.map evalhand hands
let totalscore = List.fold_left (fun x y -> x + y) 0 scores
(*   assert ((hands_of_pair p) = (Rock, Paper)) *)


(* let _ = List.iter (fun (x, y) -> print_string x; print_endline y) parsed *)

