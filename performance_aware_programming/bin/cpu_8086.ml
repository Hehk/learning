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

let print_bytes bytes =
  let rec print_bytes_aux bytes =
    match bytes with
    | [] -> ()
    | _ ->
        let bytes_to_print, rest = List.split_n bytes 16 in
        let hex =
          List.map bytes_to_print ~f:(fun b -> Printf.sprintf "%02x" b)
        in
        let hex_str = String.concat hex ~sep:" " in
        printf "%-48s" hex_str;
        print_bytes_aux rest
  in
  print_bytes_aux bytes

let parse_args argv =
  match argv with
  | [| _; file_name |] -> file_name
  | _ -> failwith "Usage: ./hexdump <file_name>"

type register =
  | AX
  | CX
  | DX
  | BX
  | SP
  | BP
  | SI
  | DI
  | AL
  | CL
  | DL
  | BL
  | AH
  | CH
  | DH
  | BH
[@@deriving show]


let parse_registers word reg =
  match (word, reg) with
  | true, 0b000 -> AX
  | true, 0b001 -> CX
  | true, 0b010 -> DX
  | true, 0b011 -> BX
  | true, 0b100 -> SP
  | true, 0b101 -> BP
  | true, 0b110 -> SI
  | true, 0b111 -> DI
  | false, 0b000 -> AL
  | false, 0b001 -> CL
  | false, 0b010 -> DL
  | false, 0b011 -> BL
  | false, 0b100 -> AH
  | false, 0b101 -> CH
  | false, 0b110 -> DH
  | false, 0b111 -> BH
  | _ -> failwith "Invalid register"

type params = { direction : bool; word : bool; mode : int; reg : int; rm : int }
[@@deriving show]

type instruction =
  | Mov of (register * register)
  | Add of (register * register)
  | Other of (int * params)
[@@deriving show]

let parse_instruction b1 b2 =
  let opcode = (b1 land 0b1111_1100) lsr 2 in
  let direction = (b1 land 0b0000_0010) lsr 1 = 1 in
  let word = b1 land 0b0000_0001 = 1 in
  let mode = (b2 land 0b1100_0000) lsr 6 in
  let reg = (b2 land 0b0011_1000) lsr 3 in
  let rm = b2 land 0b0000_0111 in
  let params = { direction; word; mode; reg; rm } in
  match opcode with
  | 0b1000_10 -> Mov (parse_registers word rm, parse_registers word reg)
  (* TODO actually implement this *)
  | 0x01 -> Add (parse_registers word rm, parse_registers word reg)
  | _ -> Other (opcode, params)

let rec parse_bytes bytes =
  match bytes with
  | [] -> []
  | _ :: [] -> []
  | b1 :: b2 :: rest ->
      let instr = parse_instruction b1 b2 in
      instr :: parse_bytes rest

let print_opcodes opcodes =
  printf "Opcodes:\n";
  List.iter opcodes ~f:(fun instr -> printf "%s\n" (show_instruction instr))

let print_asm_register = function
  | AX -> "ax"
  | CX -> "cx"
  | DX -> "dx"
  | BX -> "bx"
  | SP -> "sp"
  | BP -> "bp"
  | SI -> "si"
  | DI -> "di"
  | AL -> "al"
  | CL -> "cl"
  | DL -> "dl"
  | BL -> "bl"
  | AH -> "ah"
  | CH -> "ch"
  | DH -> "dh"
  | BH -> "bh"

let print_asm_opcode opcodes =
  match opcodes with
  | Mov (r1, r2) ->
      Printf.sprintf "mov %s, %s" (print_asm_register r1) (print_asm_register r2)
  | Add (r1, r2) ->
      Printf.sprintf "add %s, %s" (print_asm_register r1) (print_asm_register r2)
  | Other (opcode, _) -> Printf.sprintf "other %d" opcode

let print_asm operations =
  let output = "bits 16\n\n" in
  let rec pretty_print_aux operations =
    match operations with
    | [] -> ""
    | op :: rest ->
        let op_str = print_asm_opcode op in
        op_str ^ "\n" ^ pretty_print_aux rest
  in
  output ^ pretty_print_aux operations |> printf "%s"

let () =
  Sys.get_argv () |> parse_args |> read_bytes |> parse_bytes |> print_asm
