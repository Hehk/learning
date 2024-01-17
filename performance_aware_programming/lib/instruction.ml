open Base

type immediate_instruction = {
  sign : int;
  word : bool;
  mode : int;
  reg : int;
  rm : int;
  displacement : int;
  data : int;
}
[@@deriving show, eq]

type register_or_memory = {
  direction : bool;
  word : bool;
  mode : int;
  reg : int;
  rm : int;
  displacement : int;
}
[@@deriving show, eq]

type memory_to_accumulator = { direction : bool; word : bool; address : int }
[@@deriving show, eq]

type immediate_to_register = { word : bool; reg : int; data : int }
[@@deriving show, eq]

type immediate_to_accumulator = { direction : bool; word : bool; data : int }
[@@deriving show, eq]

type t =
  | Move_reg_or_mem_to_reg_or_mem of register_or_memory
  | Move_imm_to_reg_or_mem of immediate_instruction
  | Move_imm_to_reg of immediate_to_register
  | Move_mem_to_acc of memory_to_accumulator
  | Move_acc_to_mem of memory_to_accumulator
  | Move_reg_or_mem_to_seg_reg of register_or_memory
  | Move_seg_reg_to_reg_or_mem of register_or_memory
  (* Skipping Push *)
  (* Skipping Pop *)
  (* Skiping exchange *)
  (* Skipping In *)
  (* Skipping Out *)
  | Add_reg_or_mem_to_reg_or_mem of register_or_memory
  | Add_imm_to_reg_or_mem of immediate_instruction
  | Add_imm_to_acc of immediate_to_accumulator
  | Adc_reg_or_mem_to_reg_or_mem of register_or_memory
  | Adc_imm_to_reg_or_mem of immediate_instruction
  | Adc_imm_to_acc of immediate_to_accumulator
  (* Skipping Inc *)
  | Sub_reg_or_mem_to_reg_or_mem of register_or_memory
  | Sub_imm_from_reg_or_mem of immediate_instruction
  | Sub_imm_from_acc of immediate_to_accumulator
  | Sbb_reg_or_mem_to_reg_or_mem of register_or_memory
  | Sbb_imm_from_reg_or_mem of immediate_instruction
  | Sbb_imm_from_acc of immediate_to_accumulator
  (* Skipping Dec *)
  | Comp_reg_or_mem_with_reg_or_mem of register_or_memory
  | Comp_imm_with_reg_or_mem of immediate_instruction
  | Comp_imm_with_acc of immediate_to_accumulator
  (* Skipping Neg *)
  (* Skipping Mul *)
  (* Skipping Imul *)
  (* Skipping Div *)
  (* Skipping logic *)
  | And_reg_or_mem_with_reg_or_mem of register_or_memory
  | And_imm_with_reg_or_mem of immediate_instruction
  | And_imm_with_acc of immediate_to_accumulator
  (* Skipping Test *)
  | Or_reg_or_mem_with_reg_or_mem of register_or_memory
  | Or_imm_with_reg_or_mem of immediate_instruction
  | Or_imm_to_acc of immediate_to_accumulator
  (* Skipping xor *)
  (* Skipping String minip *)
  (* Skipping Control Transfer *)
  (* Skipping Unconditional jump *)
  (* Skilling return *)
  | Jump_on_equal of int
  | Jump_on_less of int
  | Jump_on_less_or_equal of int
  | Jump_on_below of int
  | Jump_on_below_or_equal of int
  | Jump_on_parity of int
  | Jump_on_overflow of int
  | Jump_on_sign of int
  | Jump_on_not_equal of int
  | Jump_on_not_less of int
  | Jump_on_not_less_or_equal of int
  | Jump_on_not_below of int
  | Jump_on_not_below_or_equal of int
  | Jump_on_not_parity of int
  | Jump_on_not_overflow of int
  | Jump_on_not_sign of int
  | Loop_cx_times of int
  | Loop_while_equal of int
  | Loop_while_not_equal of int
  | Jump_on_cx_zero of int
(* Skipping interrupt *)
(* Skipping processor control *)
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

let format_address address =
  if address + 2 > 0 then
    let address = address + 2 in
    Printf.sprintf "$+%d" address
  else if address + 2 = 0 then "$"
  else Printf.sprintf "$%d" (address + 2)

let show_asm_data data =
  let show direction a b =
    if direction then Printf.sprintf "%s, %s" a b
    else Printf.sprintf "%s, %s" b a
  in
  match data with
  | `Register_or_memory x ->
      let rm = Register.parse_rm x.mode x.displacement x.word x.rm in
      let rm = Register.show_rm rm in
      let reg = Register.show_register x.word x.reg in
      show x.direction reg rm
  | `Immediate_to_register_or_memory (x : immediate_instruction) ->
      let rm = Register.parse_rm x.mode x.displacement x.word x.rm in
      let rm = Register.show_rm rm in
      let data = Register.show_data x.word x.data in
      Printf.sprintf "%s, %s" rm data
  | `Immediate_to_register { word; reg; data } ->
      Printf.sprintf "%s, %d" (Register.show_register word reg) data
  | `Memory_to_accumulator { word; address; direction } ->
      show direction
        (Register.show_register word 0)
        (Printf.sprintf "[%s%d]" (if address >= 0 then "+" else "-") address)
  | `Immediate_to_accumulator { data; word; direction } ->
      show direction (Register.show_register word 0) (Printf.sprintf "%d" data)
  | `Address address -> format_address address

let show_asm_tag = function
  | Move_acc_to_mem _ | Move_mem_to_acc _ | Move_reg_or_mem_to_reg_or_mem _
  | Move_imm_to_reg_or_mem _ | Move_imm_to_reg _ | Move_reg_or_mem_to_seg_reg _
  | Move_seg_reg_to_reg_or_mem _ ->
      "mov"
  | Add_reg_or_mem_to_reg_or_mem _ | Add_imm_to_reg_or_mem _ | Add_imm_to_acc _
    ->
      "add"
  | Adc_reg_or_mem_to_reg_or_mem _ | Adc_imm_to_reg_or_mem _ | Adc_imm_to_acc _
    ->
      "adc"
  | Sub_reg_or_mem_to_reg_or_mem _ | Sub_imm_from_reg_or_mem _
  | Sub_imm_from_acc _ ->
      "sub"
  | Sbb_reg_or_mem_to_reg_or_mem _ | Sbb_imm_from_reg_or_mem _
  | Sbb_imm_from_acc _ ->
      "sbb"
  | Comp_reg_or_mem_with_reg_or_mem _ | Comp_imm_with_reg_or_mem _
  | Comp_imm_with_acc _ ->
      "cmp"
  | And_reg_or_mem_with_reg_or_mem _ | And_imm_with_reg_or_mem _
  | And_imm_with_acc _ ->
      "and"
  | Or_reg_or_mem_with_reg_or_mem _ | Or_imm_with_reg_or_mem _ | Or_imm_to_acc _
    ->
      "or"
  | Jump_on_equal _ -> "je"
  | Jump_on_less _ -> "jl"
  | Jump_on_less_or_equal _ -> "jle"
  | Jump_on_below _ -> "jb"
  | Jump_on_below_or_equal _ -> "jbe"
  | Jump_on_parity _ -> "jp"
  | Jump_on_overflow _ -> "jo"
  | Jump_on_sign _ -> "js"
  | Jump_on_not_equal _ -> "jne"
  | Jump_on_not_less _ -> "jnl"
  | Jump_on_not_less_or_equal _ -> "jnle"
  | Jump_on_not_below _ -> "jnb"
  | Jump_on_not_below_or_equal _ -> "jnbe"
  | Jump_on_not_parity _ -> "jnp"
  | Jump_on_not_overflow _ -> "jno"
  | Jump_on_not_sign _ -> "jns"
  | Loop_cx_times _ -> "loop"
  | Loop_while_equal _ -> "loopz"
  | Loop_while_not_equal _ -> "loopnz"
  | Jump_on_cx_zero _ -> "jcxz"

let instruction_data = function
  | Move_acc_to_mem x -> `Memory_to_accumulator x
  | Move_mem_to_acc x -> `Memory_to_accumulator x
  | Move_reg_or_mem_to_reg_or_mem x -> `Register_or_memory x
  | Move_imm_to_reg_or_mem x -> `Immediate_to_register_or_memory x
  | Move_imm_to_reg x -> `Immediate_to_register x
  | Move_reg_or_mem_to_seg_reg x -> `Register_or_memory x
  | Move_seg_reg_to_reg_or_mem x -> `Register_or_memory x
  | Add_reg_or_mem_to_reg_or_mem x -> `Register_or_memory x
  | Add_imm_to_reg_or_mem x -> `Immediate_to_register_or_memory x
  | Add_imm_to_acc x -> `Immediate_to_accumulator x
  | Adc_reg_or_mem_to_reg_or_mem x -> `Register_or_memory x
  | Adc_imm_to_reg_or_mem x -> `Immediate_to_register_or_memory x
  | Adc_imm_to_acc x -> `Immediate_to_accumulator x
  | Sub_reg_or_mem_to_reg_or_mem x -> `Register_or_memory x
  | Sub_imm_from_reg_or_mem x -> `Immediate_to_register_or_memory x
  | Sub_imm_from_acc x -> `Immediate_to_accumulator x
  | Sbb_reg_or_mem_to_reg_or_mem x -> `Register_or_memory x
  | Sbb_imm_from_reg_or_mem x -> `Immediate_to_register_or_memory x
  | Sbb_imm_from_acc x -> `Immediate_to_accumulator x
  | Comp_reg_or_mem_with_reg_or_mem x -> `Register_or_memory x
  | Comp_imm_with_reg_or_mem x -> `Immediate_to_register_or_memory x
  | Comp_imm_with_acc x -> `Immediate_to_accumulator x
  | And_reg_or_mem_with_reg_or_mem x -> `Register_or_memory x
  | And_imm_with_reg_or_mem x -> `Immediate_to_register_or_memory x
  | And_imm_with_acc x -> `Immediate_to_accumulator x
  | Or_reg_or_mem_with_reg_or_mem x -> `Register_or_memory x
  | Or_imm_with_reg_or_mem x -> `Immediate_to_register_or_memory x
  | Or_imm_to_acc x -> `Immediate_to_accumulator x
  | Jump_on_equal x -> `Address x
  | Jump_on_less x -> `Address x
  | Jump_on_less_or_equal x -> `Address x
  | Jump_on_below x -> `Address x
  | Jump_on_below_or_equal x -> `Address x
  | Jump_on_parity x -> `Address x
  | Jump_on_overflow x -> `Address x
  | Jump_on_sign x -> `Address x
  | Jump_on_not_equal x -> `Address x
  | Jump_on_not_less x -> `Address x
  | Jump_on_not_less_or_equal x -> `Address x
  | Jump_on_not_below x -> `Address x
  | Jump_on_not_below_or_equal x -> `Address x
  | Jump_on_not_parity x -> `Address x
  | Jump_on_not_overflow x -> `Address x
  | Jump_on_not_sign x -> `Address x
  | Loop_cx_times x -> `Address x
  | Loop_while_equal x -> `Address x
  | Loop_while_not_equal x -> `Address x
  | Jump_on_cx_zero x -> `Address x

let show_asm instruction =
  let tag = show_asm_tag instruction in
  let data = show_asm_data @@ instruction_data instruction in
  tag ^ " " ^ data

let decode_displacement mode rm rest =
  match (mode, rm, rest) with
  | 0, 6, b3 :: b4 :: rest -> ((b4 * 256) + b3, rest)
  | 3, _, _ -> (0, rest)
  | 0, _, _ -> (0, rest)
  | 1, _, b3 :: rest ->
      let complement = if b3 > 127 then 256 else 0 in
      (b3 - complement, rest)
  | 2, _, b3 :: b4 :: rest -> ((b4 * 256) + b3, rest)
  | _ -> failwith "Invalid displacement"

let decode_data word rest =
  match (word, rest) with
  | false, data :: rest -> (data, rest)
  | true, data_1 :: data_2 :: rest -> ((data_2 * 256) + data_1, rest)
  | _ -> failwith "Invalid data"

let decode_direct_address bytes =
  match bytes with
  | b1 :: b2 :: rest ->
      let address = (b2 * 256) + b1 in
      (address, rest)
  | _ -> failwith "Direct Address: No second byte"

let convert_to_8bit_signed value =
  let masked_value = value land 0xFF in
  if masked_value lsr 7 = 1 then -1 * (lnot (masked_value - 1) land 0xFF)
  else masked_value

let decode_instruction_pointer bytes =
  match bytes with
  | b1 :: rest -> (convert_to_8bit_signed b1, rest)
  | _ -> failwith "Instruction Pointer: No second byte"

type mode_byte = { mode : int; reg : int; rm : int }

let decode_mode_byte = function
  | byte :: rest ->
      let mode = take_bits 0 2 byte in
      let reg = take_bits 2 3 byte in
      let rm = take_bits 5 3 byte in
      ({ mode; reg; rm }, rest)
  | _ -> failwith "Mode Byte: No second byte"

let decode_register_or_memory d w bytes =
  let direction = d = 1 in
  let word = w = 1 in

  let { mode; reg; rm }, rest = decode_mode_byte bytes in
  let displacement, rest = decode_displacement mode rm rest in
  ({ direction; word; mode; reg; rm; displacement }, rest)

let decode_move_immidiate_to_register_or_memory w rest =
  match rest with
  | b2 :: rest ->
      let mode = take_bits 0 2 b2 in
      let rm = take_bits 5 3 b2 in
      let displacement, rest = decode_displacement mode rm rest in
      let data, rest = decode_data w rest in
      let info =
        { word = w; mode; rm; displacement; data; sign = 0; reg = 0 }
      in
      (info, rest)
  | _ -> failwith "Move Immediate to Register or Memory: No second byte"

let decode_instruction binary rest =
  let open Binary_parser in
  match to_binary binary with
  (* Special case where an opcode is shared *)
  | 1, 0, 0, 0, 0, 0, s, w ->
      let { mode; rm; reg }, rest = decode_mode_byte rest in
      let displacement, rest = decode_displacement mode rm rest in
      let data, rest = decode_data (s = 0 && w = 1) rest in
      let info =
        { sign = s; word = w = 1; mode; reg; rm; displacement; data }
      in
      let instruction =
        match reg with
        | 0b000 -> Add_imm_to_reg_or_mem info
        | 0b010 -> Adc_imm_to_reg_or_mem info
        | 0b101 -> Sub_imm_from_reg_or_mem info
        | 0b011 -> Sbb_imm_from_reg_or_mem info
        | 0b100 -> And_imm_with_reg_or_mem info
        | 0b001 -> Or_imm_with_reg_or_mem info
        | 0b111 -> Comp_imm_with_reg_or_mem info
        | _ -> failwith "Invalid opcode"
      in
      (instruction, rest)
  | 1, 0, 0, 0, 1, 0, d, w ->
      let info, rest = decode_register_or_memory d w rest in
      (Move_reg_or_mem_to_reg_or_mem info, rest)
  | 1, 1, 0, 0, 0, 1, 1, w ->
      let info, rest =
        decode_move_immidiate_to_register_or_memory (w = 1) rest
      in
      (Move_imm_to_reg_or_mem info, rest)
  | 1, 0, 1, 1, w, reg_1, reg_2, reg_3 ->
      let word = w = 1 in
      let reg = combine [ reg_1; reg_2; reg_3 ] in
      let data, rest = decode_data word rest in
      let info = { word; reg; data } in
      (Move_imm_to_reg info, rest)
  | 1, 0, 1, 0, 0, 0, 0, w ->
      let address, rest = decode_direct_address rest in
      let info = { direction = true; word = w = 1; address } in
      (Move_mem_to_acc info, rest)
  | 1, 0, 1, 0, 0, 0, 1, w ->
      let address, rest = decode_direct_address rest in
      let info = { direction = false; word = w = 1; address } in
      (Move_acc_to_mem info, rest)
  | 1, 0, 0, 0, 1, 1, 1, 0 ->
      failwith "Move Register or Memory to Segment Register: Not implemented"
  | 1, 0, 0, 0, 1, 1, 0, 0 ->
      failwith "Move Segment to Register or Memory: Not implemented"
  | 0, 0, 0, 0, 0, 0, d, w ->
      let info, rest = decode_register_or_memory d w rest in
      (Add_reg_or_mem_to_reg_or_mem info, rest)
  | 0, 0, 0, 0, 0, 1, 0, w ->
      let data, rest = decode_data (w = 1) rest in
      let info = { direction = true; word = w = 1; data } in
      (Add_imm_to_acc info, rest)
  | 0, 0, 1, 0, 1, 0, d, w ->
      let info, rest = decode_register_or_memory d w rest in
      (Sub_reg_or_mem_to_reg_or_mem info, rest)
  | 0, 0, 1, 0, 1, 1, 0, w ->
      let data, rest = decode_data (w = 1) rest in
      let info = { direction = true; word = w = 1; data } in
      (Sub_imm_from_acc info, rest)
  | 0, 0, 1, 1, 1, 0, d, w ->
      let info, rest = decode_register_or_memory d w rest in
      (Comp_reg_or_mem_with_reg_or_mem info, rest)
  | 0, 0, 1, 1, 1, 1, 0, w ->
      let data, rest = decode_data (w = 1) rest in
      let info = { direction = true; word = w = 1; data } in
      (Comp_imm_with_acc info, rest)
  | 0, 1, 1, 1, 0, 1, 0, 0 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_equal address, rest)
  | 0, 1, 1, 1, 1, 1, 0, 0 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_less address, rest)
  | 0, 1, 1, 1, 1, 1, 1, 0 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_less_or_equal address, rest)
  | 0, 1, 1, 1, 0, 0, 1, 0 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_below address, rest)
  | 0, 1, 1, 1, 0, 1, 1, 0 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_below_or_equal address, rest)
  | 0, 1, 1, 1, 1, 0, 1, 0 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_parity address, rest)
  | 0, 1, 1, 1, 0, 0, 0, 0 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_overflow address, rest)
  | 0, 1, 1, 1, 1, 0, 0, 0 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_sign address, rest)
  | 0, 1, 1, 1, 0, 1, 0, 1 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_not_equal address, rest)
  | 0, 1, 1, 1, 1, 1, 0, 1 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_not_less address, rest)
  | 0, 1, 1, 1, 1, 1, 1, 1 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_not_less_or_equal address, rest)
  | 0, 1, 1, 1, 0, 0, 1, 1 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_not_below address, rest)
  | 0, 1, 1, 1, 0, 1, 1, 1 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_not_below_or_equal address, rest)
  | 0, 1, 1, 1, 1, 0, 1, 1 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_not_parity address, rest)
  | 0, 1, 1, 1, 0, 0, 0, 1 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_not_overflow address, rest)
  | 0, 1, 1, 1, 1, 0, 0, 1 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_not_sign address, rest)
  | 1, 1, 1, 0, 0, 0, 1, 0 ->
      let address, rest = decode_instruction_pointer rest in
      (Loop_cx_times address, rest)
  | 1, 1, 1, 0, 0, 0, 0, 1 ->
      let address, rest = decode_instruction_pointer rest in
      (Loop_while_equal address, rest)
  | 1, 1, 1, 0, 0, 0, 0, 0 ->
      let address, rest = decode_instruction_pointer rest in
      (Loop_while_not_equal address, rest)
  | 1, 1, 1, 0, 0, 0, 1, 1 ->
      let address, rest = decode_instruction_pointer rest in
      (Jump_on_cx_zero address, rest)
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
