open Base
open Stdio

let read_bytes file_name =
  let ic = In_channel.create file_name in
  let rec read_all acc =
    match In_channel.input_byte ic with
    | None -> List.rev acc
    | Some b -> read_all (b :: acc)
  in
  let bytes = read_all [] in
  In_channel.close ic;
  bytes

