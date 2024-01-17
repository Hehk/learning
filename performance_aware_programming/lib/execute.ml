open Base

type flags = {
  zero : bool;
  sign : bool;
  aux_carry : bool;
  carry : bool;
  overflow : bool;
  parity : bool;
}

module MemoryMap = Map.M (Int)

type state = {
  registers : int array;
  ip : int;
  flags : flags;
  memory : int MemoryMap.t;
}

type update = {
  registers : int array;
  flags : flags;
  move : int;
  memory : (int * int) option;
}

let take_high x = x lsr 8
let take_low x = x land 0xff

let read_flags flags =
  [
    (flags.zero, "PZ");
    (flags.carry, "C");
    (flags.parity, "P");
    (flags.aux_carry, "A");
    (flags.sign, "S");
    (flags.overflow, "O");
  ]
  |> List.filter ~f:fst |> List.map ~f:snd |> String.concat ~sep:""

let int16_of_int value =
  (* Mask the value to 16 bits to simulate overflow *)
  value land 0xFFFF

let index_to_register = function
  | 0 -> Register.AX
  | 1 -> Register.BX
  | 2 -> Register.CX
  | 3 -> Register.DX
  | 4 -> Register.SI
  | 5 -> Register.DI
  | 6 -> Register.BP
  | 7 -> Register.SP
  | _ -> raise (Invalid_argument "not valid register index")

let read_register registers register =
  let open Register in
  match register with
  | AX -> registers.(0)
  | AL -> take_low registers.(0)
  | AH -> take_high registers.(0)
  | BX -> registers.(1)
  | BL -> take_low registers.(1)
  | BH -> take_high registers.(1)
  | CX -> registers.(2)
  | CL -> take_low registers.(2)
  | CH -> take_high registers.(2)
  | DX -> registers.(3)
  | DL -> take_low registers.(3)
  | DH -> take_high registers.(3)
  | SI -> registers.(4)
  | DI -> registers.(5)
  | BP -> registers.(6)
  | SP -> registers.(7)

let read_value (state : state) = function
  | `Reg register -> read_register state.registers register
  | `Mem address -> Map.find state.memory address |> Option.value_exn

let write_high old_value new_value = old_value land 0x00ff lor (new_value lsl 8)
let write_low old_value new_value = old_value land 0xff00 lor new_value

let write_register registers register value =
  let registers = Array.copy registers in
  let open Register in
  let () =
    match register with
    | AX -> registers.(0) <- value
    | AL -> registers.(0) <- write_low registers.(0) value
    | AH -> registers.(0) <- write_high registers.(0) value
    | BX -> registers.(1) <- value
    | BL -> registers.(1) <- write_low registers.(1) value
    | BH -> registers.(1) <- write_high registers.(1) value
    | CX -> registers.(2) <- value
    | CL -> registers.(2) <- write_low registers.(2) value
    | CH -> registers.(2) <- write_high registers.(2) value
    | DX -> registers.(3) <- value
    | DL -> registers.(3) <- write_low registers.(3) value
    | DH -> registers.(3) <- write_high registers.(3) value
    | SI -> registers.(4) <- value
    | DI -> registers.(5) <- value
    | BP -> registers.(6) <- value
    | SP -> registers.(7) <- value
  in
  registers

let create_update target new_value registers flags =
  match target with
  | `Reg register ->
      {
        move = 0;
        registers = write_register registers register new_value;
        flags;
        memory = None;
      }
  | `Mem address ->
      { move = 0; registers; flags; memory = Some (address, new_value) }

let calc_disp (state : state) rm =
  match rm with
  | false, Some reg, _, _ -> `Reg reg
  | false, None, _, _ -> raise (Invalid_argument "not valid instruction")
  | true, a, b, disp ->
      let a =
        Option.value_map ~default:0 ~f:(read_register state.registers) a
      in
      let b =
        Option.value_map ~default:0 ~f:(read_register state.registers) b
      in
      let disp = Option.value ~default:0 disp in
      let disp = a + b + disp in
      `Mem disp

let handle_immediate_instruction (state : state)
    (x : Instruction.immediate_instruction) =
  let rm = Register.parse_rm x.mode x.displacement x.word x.rm in
  let target = calc_disp state rm in
  (target, x.data)

let get_data (state : state) instruction =
  Instruction.(
    function
    | `Immediate_to_register { word; reg; data } ->
        let register = Register.parse word reg |> Option.value_exn in
        (`Reg register, data)
    | `Register_or_memory (x : register_or_memory) -> (
        let a = Register.parse x.word x.reg |> Option.value_exn in
        let rm = Register.parse_rm x.mode x.displacement x.word x.rm in
        let b = calc_disp state rm in
        match x.direction with
        | false -> (b, read_value state (`Reg a))
        | true -> (`Reg a, read_value state b))
    | `Immediate_to_register_or_memory (x : immediate_instruction) ->
        handle_immediate_instruction state x
    | _ -> raise (Invalid_argument ("not valid instruction" ^ show instruction)))

let log_diff a b =
  let log =
    Array.zip_exn a b
    |> Array.find_mapi ~f:(fun i (x, y) ->
           if x = y then None else Some (i, x, y))
  in
  match log with
  | None -> None
  | Some (i, x, y) ->
      let register = index_to_register i in
      Some (Printf.sprintf "%s:0x%x->0x%x" (Register.to_string register) x y)

let log_flag old_flags new_flags =
  let ( = ) = phys_equal in
  let turned_on =
    {
      zero = old_flags.zero = false && new_flags.zero = true;
      sign = old_flags.sign = false && new_flags.sign = true;
      aux_carry = old_flags.aux_carry = false && new_flags.aux_carry = true;
      parity = old_flags.parity = false && new_flags.parity = true;
      carry = old_flags.carry = false && new_flags.carry = true;
      overflow = old_flags.overflow = false && new_flags.overflow = true;
    }
  in
  let turned_off =
    {
      zero = old_flags.zero = true && new_flags.zero = false;
      sign = old_flags.sign = true && new_flags.sign = false;
      aux_carry = old_flags.aux_carry = true && new_flags.aux_carry = false;
      parity = old_flags.parity = true && new_flags.parity = false;
      carry = old_flags.carry = true && new_flags.carry = false;
      overflow = old_flags.overflow = true && new_flags.overflow = false;
    }
  in
  let turned_on = read_flags turned_on in
  let turned_off = read_flags turned_off in
  if String.is_empty turned_on && String.is_empty turned_off then None
  else Some (Printf.sprintf "flags:%s->%s" turned_off turned_on)

let log_ip old_ip new_ip =
  if old_ip = new_ip then None
  else Some (Printf.sprintf "ip:0x%x->0x%x" old_ip new_ip)

let log_step instruction (s : state) (ns : state) =
  let log = Instruction.show_asm instruction ^ " ; " in
  let diff = log_diff s.registers ns.registers in
  let ip = log_ip s.ip ns.ip in
  let flag = log_flag s.flags ns.flags in
  let logs = List.filter_opt [ diff; ip; flag ] in
  log ^ String.concat ~sep:" " logs |> String.strip

let bits_set_to_1 value =
  let rec count_bits v count =
    if v = 0 then count else count_bits (v lsr 1) (count + (v land 1))
  in
  count_bits value 0

let update_flags flags value op1 op2 operation =
  let zero = value land 0xFFFF = 0 in
  let sign = value land 0x8000 <> 0 in
  let carry =
    match operation with
    | `Add -> value land 0x10000 <> 0
    | `Sub -> op1 < op2
    | `Comp -> op1 < op2
    | _ -> flags.carry
  in
  let aux_carry =
    match operation with
    | `Add -> (op1 land 0xF) + (op2 land 0xF) > 0xF
    | `Sub -> op1 land 0xF < op2 land 0xF
    | `Comp -> op1 land 0xF < op2 land 0xF
    | _ -> flags.aux_carry
  in
  let overflow =
    match operation with
    | `Add ->
        op1 land 0x8000 = op2 land 0x8000
        && value land 0x8000 <> op1 land 0x8000
    | `Comp | `Sub ->
        op1 land 0x8000 <> op2 land 0x8000
        && value land 0x8000 = op2 land 0x8000
    | _ -> flags.overflow
  in
  let parity = value |> take_low |> bits_set_to_1 in
  let parity = parity % 2 = 0 && parity <> 0 in
  { zero; sign; aux_carry; carry; overflow; parity }

let handle_move (state : state) instruction =
  let open Instruction in
  let to_reg, from_value =
    instruction |> instruction_data |> get_data state instruction
  in
  create_update to_reg from_value state.registers state.flags

let handle_sub (state : state) instruction =
  let open Instruction in
  let to_reg, from_value =
    instruction |> instruction_data |> get_data state instruction
  in
  let to_value = read_value state to_reg in
  let result = int16_of_int (to_value - from_value) in
  let flags = update_flags state.flags result to_value from_value `Sub in
  create_update to_reg result state.registers flags

let handle_comp (state : state) instruction =
  let open Instruction in
  let to_reg, from_value =
    instruction |> instruction_data |> get_data state instruction
  in
  let to_value = read_value state to_reg in
  let result = int16_of_int (to_value - from_value) in
  let flags = update_flags state.flags result to_value from_value `Comp in
  let new_state = { state with flags } in
  { registers = new_state.registers; flags; move = 0; memory = None }

let handle_add (state : state) instruction =
  let open Instruction in
  let to_reg, from_value =
    instruction |> instruction_data |> get_data state instruction
  in
  let to_value = read_value state to_reg in
  let result = int16_of_int (to_value + from_value) in
  let flags = update_flags state.flags result to_value from_value `Add in
  create_update to_reg result state.registers flags

let handle_jump (state : state) instruction =
  { registers = state.registers; flags = state.flags; move = 0; memory = None }

let exec_instruction (state : state) instruction =
  let open Instruction in
  match instruction with
  | Jump_on_not_equal address ->
      if state.flags.zero then
        {
          registers = state.registers;
          flags = state.flags;
          move = 0;
          memory = None;
        }
      else
        {
          registers = state.registers;
          flags = state.flags;
          move = address;
          memory = None;
        }
  | Jump_on_less _ | Jump_on_sign _ | Jump_on_not_sign _ ->
      handle_jump state instruction
  | Move_reg_or_mem_to_reg_or_mem _ | Move_imm_to_reg _ | Move_acc_to_mem _
  | Move_mem_to_acc _ | Move_imm_to_reg_or_mem _ | Move_reg_or_mem_to_seg_reg _
  | Move_seg_reg_to_reg_or_mem _ ->
      handle_move state instruction
  | Sub_imm_from_acc _ | Sub_imm_from_reg_or_mem _
  | Sub_reg_or_mem_to_reg_or_mem _ ->
      handle_sub state instruction
  | Comp_reg_or_mem_with_reg_or_mem _ | Comp_imm_with_reg_or_mem _
  | Comp_imm_with_acc _ ->
      handle_comp state instruction
  | Add_imm_to_reg_or_mem _ | Add_imm_to_acc _ | Add_reg_or_mem_to_reg_or_mem _
    ->
      handle_add state instruction
  | _ -> raise (Invalid_argument ("not valid instruction" ^ show instruction))

let initial_state () =
  {
    registers = Array.init 8 ~f:(fun _ -> 0);
    ip = 0;
    memory = Map.empty (module Int);
    flags =
      {
        zero = false;
        sign = false;
        aux_carry = false;
        carry = false;
        overflow = false;
        parity = false;
      };
  }

let exec_bytes ?(max = 100) bytes =
  let len = List.length bytes in
  let rec loop iter logs state =
    let remaining = len - state.ip in
    if state.ip >= len || iter >= max then (state, List.rev logs)
    else
      let next_bytes = List.drop bytes state.ip in
      let b1, rest = (List.hd_exn next_bytes, List.tl_exn next_bytes) in
      let instruction, rest = Instruction.decode_instruction b1 rest in

      let update = exec_instruction state instruction in
      let new_state =
        {
          registers = update.registers;
          ip = len - List.length rest + update.move;
          flags = update.flags;
          memory =
            (match update.memory with
            | Some (address, value) ->
                Map.set state.memory ~key:address ~data:value
            | None -> state.memory);
        }
      in
      let log = log_step instruction state new_state in
      loop (iter + 1) (log :: logs) new_state
  in
  loop 0 [] @@ initial_state ()

let exec instructions =
  let rec loop logs state instructions =
    match instructions with
    | [] -> (state, List.rev logs)
    | instruction :: rest ->
        let update = exec_instruction state instruction in
        let new_state =
          { state with registers = update.registers; flags = update.flags }
        in
        let log = log_step instruction state new_state in
        loop (log :: logs) new_state rest
  in
  loop [] (initial_state ()) instructions
