open Cpu

let (%) f g x = f (g x)

let print_asm = print_endline % show_asm

let print_asm_file instructions =
  print_endline "bits 16";
  List.iter print_asm instructions


let parse_args argv =
  match argv with
  | [| _; file_name |] -> file_name
  | _ -> failwith "Usage: ./hexdump <file_name>"

let () =
  Base.Sys.get_argv () |> parse_args |> read_bytes |> from_bytes
  |> print_asm_file
