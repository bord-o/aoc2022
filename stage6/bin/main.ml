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

let all_uniq l = List.contains_dup l ~compare:compare_char |> not

exception NoPacket
let rec start_of_packet chrs index = 
  match chrs with 
  (* LMFAO *)
  | a::b::c::d::e::f::g::h::i::j::k::l::m::n::rest -> begin
    if all_uniq (a::b::c::d::e::f::g::h::i::j::k::l::m::n::[]) then 
      index
    else
      start_of_packet (b::c::d::e::f::g::h::i::j::k::l::m::n::rest) (succ index)
    end
  | _ -> raise NoPacket

let line = List.hd_exn @@ lines_of_file input
let chrs = String.to_list line
(* solution uses index number equal to seq length *)
let sol = start_of_packet chrs 14 
