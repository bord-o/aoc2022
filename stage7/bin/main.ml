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


type parse_result = Dir of string | File of { name: string; size: int }
exception ParseError
let parse_cmd c: parse_result option = 
  (* this is either a file name and size or a direectory name *)
  let open String in
  let l = length c in
  let first = sub c 0 1 in
  match first with 
  | "$" -> begin 
(*     print_string "command: ";  *)
    match sub c 2 1 with 
    | "c" -> begin 
(*       print_string "change dir ";  *)
      match sub c 5 (l-5) with 
      | s -> (* print_endline s; *) Some(Dir(s))
    end
    | _ -> (* print_endline "not change dir"; *) None
  end

  | _ -> begin 
(*     print_string "not a command: ";  *)
    match sub c 0 l with 
    | s -> begin
      match split ~on:' ' s with 
      | [l; r] -> begin
        if String.contains l 'd' then 
          None 
        else
          Some(File {name= r; size= (int_of_string l)})
      end
      | _ -> (* print_endline "bad filename"; *) None
    end
    | _ -> (* print_endline "unrecognized"; *) None
  end

let rec count fs dir_map i = 
  match fs with 
  | [] -> dir_map
  | x::xs -> 
      match x with 
      | Dir name -> 
          if (String.compare name "..") = 0 then
            count xs dir_map (pred i)
          else
            count xs ((name, 0) :: dir_map) (succ i)
      | File {name; size} -> 
          let (l, r) = List.nth_exn dir_map (i-1) in
          count xs ((l, r+size) ::  dir_map) (i)

let cmds = lines_of_file testinput
let fs = List.filter (List.map cmds parse_cmd) Option.is_some
let fs_clean = List.map fs (fun x -> Option.value ~default:(Dir "broken") x )
