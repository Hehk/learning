open Base
open Stdio

let (%) f g x = f (g x)

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

let read_asm file_name =
  file_name
    |> In_channel.read_lines
    |> List.map ~f:String.strip
    |> List.filter ~f:(not % String.is_empty)
    |> List.filter ~f:(not % String.is_prefix ~prefix:";")
    |> List.filter ~f:(not % String.is_prefix ~prefix:"bits")

let show_asm_file instructions =
  "bits 16\n" ^ String.concat ~sep:"\n" instructions
