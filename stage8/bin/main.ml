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

let lines = lines_of_file input
let ll = pred @@ String.length @@ List.hd_exn lines
let lines_arr = List.to_array (List.map lines ~f:String.to_array)

let forest = 
  let open Array in
  let ints_of_char_arr c = 
    map c ~f:(fun x -> Char.get_digit_exn x)
  in
  map lines_arr ~f:ints_of_char_arr 



(* all indexing is from 'top left' *)
let vis_right forest x y = 
  let vis = ref true in
  let height = forest.(x).(y) in
  let r = forest.(x) in
  for j=y to ll do 
    let comp_height =r.(j) in
    if comp_height >= height && j<>y then
      vis := false
    else
      ()
  done;
  !vis

let vis_left forest x y = 
  let vis = ref true in
  let height = forest.(x).(y) in
  let r = forest.(x) in
  for j=0 to y do 
    let comp_height =r.(j) in
    if comp_height >= height && j<>y then
      vis := false
    else
      ()
  done;
  !vis

let vis_up forest x y =
  let vis = ref true in
  let height = forest.(x).(y) in
  for i=0 to x do 
    let comp_height = forest.(i).(y) in
    if comp_height >= height && i<>x then
      vis := false
    else
      ()
  done;
  !vis

let vis_down forest x y =
  let vis = ref true in
  let height = forest.(x).(y) in
  for i=x to ll do 
    let comp_height = forest.(i).(y) in
    if comp_height >= height && i<>x then
      vis := false
    else
      ()
  done;
  !vis

let visible forest x y =
  vis_left forest x y || vis_right forest x y
  || vis_up forest x y || vis_down forest x y


let count_forest f = 
  let open Array in
  let visi = Stack.create () in

  for i=0 to ll do 
    for j=0 to ll do 
      Stack.push visi (visible f i j);
      ()
    done;
  done;
(*
  Stack.iter visi ~f:(fun b -> if b then 
    print_endline "visible" 
  else 
    print_endline "invisible"
  );
*)

  let id x = x in 
  Stack.count visi ~f:id

let print_forest f= 
  let open Array in
  for i=0 to ll do 
    for j=0 to ll do 
      print_int( f.(i).(j));
      ()
    done;
    print_newline ();
  done

let sol = count_forest forest
let _ = print_int sol; print_endline
