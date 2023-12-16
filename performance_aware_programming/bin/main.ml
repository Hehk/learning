open Cpu

(* let (%) f g x = f (g x) *)

(* let print_asm = print_endline % Cpu.show_asm *)

let parse_args argv =
  match argv with
  | [| _; file_name |] -> file_name
  | _ -> failwith "Usage: ./hexdump <file_name>"

let () = Base.Sys.get_argv () |> parse_args |> read_bytes |> show_bytes |> print_endline

(* let () = *)
  (* Base.Sys.get_argv () |> parse_args |> Cpu.read_bytes |> Cpu.from_bytes *)
  (* |> List.iter (print_asm) *)
