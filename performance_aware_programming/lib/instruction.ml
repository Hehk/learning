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
      mode : bool;
      rm : bool;
      disp_1 : int option;
      disp_2 : int option;
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
  | Move_Immediate_To_Register (w, reg, data) ->
      Printf.sprintf "mov %s, %d" (Register.show_register w reg) data
  | x -> show x

let decode_move_register_or_memory_to_or_from_register d w bytes =
  let direction = d = 1 in
  let word = w = 1 in
  match bytes with
  | b2 :: rest ->
      let mode = take_bits 0 2 b2 in
      let reg = take_bits 2 3 b2 in
      let rm = take_bits 5 3 b2 in
      let displacement, rest =
        match (mode, rm, rest) with
        | 0, 6, b3 :: b4 :: rest -> ((b4 * 256) + b3, rest)
        | 3, _, _ -> (0, rest)
        | 0, _, _ -> (0, rest)
        | 1, _, b3 :: rest -> (b3, rest)
        | 2, _, b3 :: b4 :: rest -> ((b4 * 256) + b3, rest)
        | _ ->
            failwith
              "Move Register or Memory to or from Register: Invalid \
               displacement"
      in
      ( Move_Register_Or_Memory_To_Or_From_Register
          { direction; word; mode; reg; rm; displacement },
        rest )
  | _ -> failwith "Move Register or Memory to or from Register: No second byte"

let decode_move_immidiate_to_register w reg rest =
  match rest with
  | b2 :: rest ->
      let data, rest =
        match (w, rest) with
        | 0, _ -> (b2, rest)
        | 1, b3 :: rest -> ((b3 * 256) + b2, rest)
        | _ -> failwith "Move Immediate to Register: Invalid data"
      in
      (Move_Immediate_To_Register (w = 1, reg, data), rest)
  | _ -> failwith "Move Immediate to Register: No second byte"

let decode_instruction binary rest =
  match to_binary binary with
  | 1, 0, 0, 0, 1, 0, d, w ->
      decode_move_register_or_memory_to_or_from_register d w rest
  | 1, 1, 0, 0, 0, 1, 1, w ->
      failwith "Move Immediate to Register or Memory: Not implemented"
  | 1, 0, 1, 1, w, reg_1, reg_2, reg_3 ->
      let reg = combine [ reg_1; reg_2; reg_3 ] in
      decode_move_immidiate_to_register w reg rest
  | 1, 0, 1, 0, 0, 0, 0, w ->
      failwith "Move Memory to Accumulator: Not implemented"
  | 1, 0, 1, 0, 0, 0, 1, w ->
      failwith "Move Accumulator to Memory: Not implemented"
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

module Tests = struct
  let expect_s a b =
    if not (String.equal a b) then
      failwith @@ Printf.sprintf "Expected %s to equal %s" a b

  let expect_i a b =
    if not (Int.equal a b) then
      failwith @@ Printf.sprintf "Expected %d to equal %d" a b

  let test_single_instruction bytes expected =
    let decode bytes =
      let hd = List.hd_exn bytes in
      let tl = List.tl_exn bytes in
      let instruction, rest = decode_instruction hd tl in
      let consumed = List.length bytes - List.length rest in
      let consumed = List.take bytes consumed in
      (instruction, show_raw_bytes consumed)
    in
    let instruction, log = decode bytes in
    let () = Stdio.printf "%s :\n%s" expected log in
    let asm = show_asm instruction in
    expect_s asm expected

  let start n bytes = List.sub bytes ~pos:n ~len:(List.length bytes - n)

  let listing_39 =
    Util.read_bytes
      "/Users/kyle/Projects/learning/performance_aware_programming/computer_enhance/perfaware/part1/listing_0039_more_movs"

  (* ; Register-to-register *)
  let%test_unit "decode_instruction mov si, bx" =
    test_single_instruction listing_39 "mov si, bx"

  let%test_unit "decode_instruction mov dh, al" =
    let bytes = start 2 listing_39 in
    test_single_instruction bytes "mov dh, al"

  let%test_unit "decode_instruction mov cl, 12" =
    let bytes = start 4 listing_39 in
    test_single_instruction bytes "mov cl, 12"

  let%test_unit "decode_instruction mov ch, -12" =
    let bytes = start 6 listing_39 in
    test_single_instruction bytes "mov ch, 244"

  let%test_unit "decode_instruction mov cx, 12" =
    let bytes = start 8 listing_39 in
    test_single_instruction bytes "mov cx, 12"

  let%test_unit "decode_instruction mov cx, -12" =
    let bytes = start 11 listing_39 in
    test_single_instruction bytes "mov cx, 65524"

  let%test_unit "decode_instruction mov dx, 3948" =
    let bytes = start 14 listing_39 in
    test_single_instruction bytes "mov dx, 3948"

  let%test_unit "decode_instruction mov dx, -3948" =
    let bytes = start 17 listing_39 in
    test_single_instruction bytes "mov dx, 61588"

  (* mov al, [bx + si] *)
  (* mov bx, [bp + di] *)
  (* mov dx, [bp] *)
  let%test_unit "decode_instruction mov al, [bx + si]" =
    let bytes = start 20 listing_39 in
    test_single_instruction bytes "mov al, [bx + si]"

  let%test_unit "decode_instruction mov bx, [bp + di]" =
    let bytes = start 22 listing_39 in
    test_single_instruction bytes "mov bx, [bp + di]"

  let%test_unit "decode_instruction mov dx, [bp]" =
    let bytes = start 24 listing_39 in
    test_single_instruction bytes "mov dx, [bp]"

  let%test_unit "decode_instruction mov ah, [bx + si + 4]" =
    let bytes = start 27 listing_39 in
    test_single_instruction bytes "mov ah, [bx + si + 4]"

  let%test_unit "decode_instruction mov al, [bx + si + 4999]" =
    let bytes = start 30 listing_39 in
    test_single_instruction bytes "mov al, [bx + si + 4999]"

  (* ; Dest address calculation *)
  (* mov [bx + di], cx *)
  (* mov [bp + si], cl *)
  (* mov [bp], ch *)
  let%test_unit "decode_instruction mov [bx + di], cx" =
    let bytes = start 34 listing_39 in
    test_single_instruction bytes "mov [bx + di], cx"

  let%test_unit "decode_instruction mov [bp + si], cl" =
    let bytes = start 36 listing_39 in
    test_single_instruction bytes "mov [bp + si], cl"

  let%test_unit "decode_instruction mov [bp], ch" =
    let bytes = start 38 listing_39 in
    test_single_instruction bytes "mov [bp], ch"
end
