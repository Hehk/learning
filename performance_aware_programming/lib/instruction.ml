open Base

module P = struct
  type direction = bool [@@deriving show, eq]
  type word = bool [@@deriving show, eq]
  type mode = int [@@deriving show, eq]
  type reg = int [@@deriving show, eq]
  type rm = int [@@deriving show, eq]
  type displacement = int [@@deriving show, eq]
  type data = int [@@deriving show, eq]
  type address = int [@@deriving show, eq]
  type sr = int [@@deriving show, eq]
end

type t =
  | Move_Register_Or_Memory_To_Or_From_Register of {
      direction : bool;
      word : bool;
      mode : int;
      reg : int;
      rm : int;
      displacement : int;
    }
  | Move_Immediate_To_Register_Or_Memory of {
      word : bool;
      mode : int;
      rm : int;
      displacement : int;
      data : int;
    }
  | Move_Immediate_To_Register of (P.word * P.reg * P.data)
  | Move_Memory_To_Accumulator of (P.word * P.address)
  | Move_Accumulator_To_Memory of (P.word * P.address)
  | Move_Register_Or_Memory_To_Segment_Register of
      (P.mode * P.sr * P.rm * P.displacement)
  | Move_Segment_To_Register_Or_Memory of
      (P.mode * P.sr * P.rm * P.displacement)
[@@deriving show, eq]

let to_binary n =
  let b i = (n land (1 lsl i)) lsr i in
  (b 7, b 6, b 5, b 4, b 3, b 2, b 1, b 0)

let show_binary b =
  let b0, b1, b2, b3, b4, b5, b6, b7 = to_binary b in
  Printf.sprintf "%d%d%d%d%d%d%d%d" b7 b6 b5 b4 b3 b2 b1 b0

let detailed_debug b1 b2 opcode =
  let b1 = show_binary b1 in
  let b2 = show_binary b2 in
  let opcode = show_binary opcode in
  Printf.sprintf "b1: %s\nb2: %s \nopcode: %s\n" b1 b2 opcode

let show_binary =
  let rec show_binary_aux i b =
    match i with
    | 8 -> ""
    | i when i >= 0 && i < 8 ->
        let bit = (b land (1 lsl i)) lsr i in
        Printf.sprintf "%s%d" (show_binary_aux (i + 1) b) bit
    | _ -> failwith "Invalid bit"
  in
  show_binary_aux 0

let take_bits start len b =
  let b = b lsr (8 - (start + len)) in
  let mask = (1 lsl len) - 1 in
  b land mask

let combine = List.fold_left ~f:(fun acc b -> (acc lsl 1) + b) ~init:0

let show_asm instruction =
  match instruction with
  | Move_Register_Or_Memory_To_Or_From_Register x ->
      let rm = Register.parse_rm x.mode x.displacement x.word x.rm in
      let rm = Register.show_rm rm in
      let reg = Register.show_register x.word x.reg in
      if x.direction then Printf.sprintf "mov %s, %s" reg rm
      else Printf.sprintf "mov %s, %s" rm reg
  | Move_Immediate_To_Register_Or_Memory x ->
      let rm = Register.parse_rm x.mode x.displacement x.word x.rm in
      let rm = Register.show_rm rm in
      let data = Register.show_data x.word x.data in
      Printf.sprintf "mov %s, %s" rm data
  | Move_Immediate_To_Register (w, reg, data) ->
      Printf.sprintf "mov %s, %d" (Register.show_register w reg) data
  | Move_Memory_To_Accumulator (w, address) ->
      Printf.sprintf "mov %s, [%d]" (Register.show_register w 0) address
  | Move_Accumulator_To_Memory (w, address) ->
      Printf.sprintf "mov [%d], %s" address (Register.show_register w 0)
  | x -> show x

let decode_displacement mode rm rest =
  match (mode, rm, rest) with
  | 0, 6, b3 :: b4 :: rest -> ((b4 * 256) + b3, rest)
  | 3, _, _ -> (0, rest)
  | 0, _, _ -> (0, rest)
  | 1, _, b3 :: rest ->
      let complement = if b3 > 127 then 256 else 0 in
      ((b3 - complement), rest)
  | 2, _, b3 :: b4 :: rest -> ((b4 * 256) + b3, rest)
  | _ -> failwith "Invalid displacement"

let decode_data w rest =
  match (w, rest) with
  | 0, data :: rest -> (data, rest)
  | 1, data_1 :: data_2 :: rest -> ((data_2 * 256) + data_1, rest)
  | _ -> failwith "Invalid data"

let decode_direct_address bytes =
  match bytes with
  | b2 :: b3 :: rest ->
      let address = (b3 * 256) + b2 in
      (address, rest)
  | _ -> failwith "Direct Address: No second byte"

let decode_move_register_or_memory_to_or_from_register d w bytes =
  let direction = d = 1 in
  let word = w = 1 in
  match bytes with
  | b2 :: rest ->
      let mode = take_bits 0 2 b2 in
      let reg = take_bits 2 3 b2 in
      let rm = take_bits 5 3 b2 in
      let displacement, rest = decode_displacement mode rm rest in
      ( Move_Register_Or_Memory_To_Or_From_Register
          { direction; word; mode; reg; rm; displacement },
        rest )
  | _ -> failwith "Move Register or Memory to or from Register: No second byte"

let decode_move_immidiate_to_register w reg rest =
  let data, rest = decode_data w rest in
  (Move_Immediate_To_Register (w = 1, reg, data), rest)

let decode_move_immidiate_to_register_or_memory w rest =
  match rest with
  | b2 :: rest ->
      let mode = take_bits 0 2 b2 in
      let rm = take_bits 5 3 b2 in
      let displacement, rest = decode_displacement mode rm rest in
      let data, rest = decode_data w rest in
      ( Move_Immediate_To_Register_Or_Memory
          { word = w = 1; mode = mode; rm = rm; displacement; data; },
        rest )
  | _ -> failwith "Move Immediate to Register or Memory: No second byte"

let decode_move_to_accumulator w rest =
  let address, rest = decode_direct_address rest in
  (Move_Memory_To_Accumulator (w = 1, address), rest)

let decode_move_accumulator_to_memory w rest =
  let address, rest = decode_direct_address rest in
  (Move_Accumulator_To_Memory (w = 1, address), rest)

let decode_instruction binary rest =
  match to_binary binary with
  | 1, 0, 0, 0, 1, 0, d, w ->
      decode_move_register_or_memory_to_or_from_register d w rest
  | 1, 1, 0, 0, 0, 1, 1, w ->
      decode_move_immidiate_to_register_or_memory w rest
  | 1, 0, 1, 1, w, reg_1, reg_2, reg_3 ->
      let reg = combine [ reg_1; reg_2; reg_3 ] in
      decode_move_immidiate_to_register w reg rest
  | 1, 0, 1, 0, 0, 0, 0, w ->
      decode_move_to_accumulator w rest
  | 1, 0, 1, 0, 0, 0, 1, w ->
      decode_move_accumulator_to_memory w rest
  | 1, 0, 0, 0, 1, 1, 1, 0 ->
      failwith "Move Register or Memory to Segment Register: Not implemented"
  | 1, 0, 0, 0, 1, 1, 0, 0 ->
      failwith "Move Segment to Register or Memory: Not implemented"
  | _ -> failwith @@ "Unknown opcode: " ^ show_binary binary

let show_raw_bytes bytes =
  bytes |> List.map ~f:show_binary
  |> List.mapi ~f:(fun i b -> (i, b))
  |> List.map ~f:(fun (i, b) -> Printf.sprintf "%d: %s\n" (i + 1) b)
  |> String.concat

let rec from_bytes = function
  | [] -> []
  | b1 :: rest ->
      let instruction, rest = decode_instruction b1 rest in
      instruction :: from_bytes rest
