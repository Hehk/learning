type t =
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

type rm = bool * t option * t option * int option
[@@deriving show]

let parse word reg =
  match (word, reg) with
  | true, 0b000 -> Some AX
  | true, 0b001 -> Some CX
  | true, 0b010 -> Some DX
  | true, 0b011 -> Some BX
  | true, 0b100 -> Some SP
  | true, 0b101 -> Some BP
  | true, 0b110 -> Some SI
  | true, 0b111 -> Some DI
  | false, 0b000 -> Some AL
  | false, 0b001 -> Some CL
  | false, 0b010 -> Some DL
  | false, 0b011 -> Some BL
  | false, 0b100 -> Some AH
  | false, 0b101 -> Some CH
  | false, 0b110 -> Some DH
  | false, 0b111 -> Some BH
  | _ -> None

let to_string = function
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

let show_register word reg =
  match parse word reg with
  | Some reg -> to_string reg
  | None -> Printf.sprintf "reg(%d)" reg

let parse_rm mode disp word rm =
  let disp =
    match disp with
    | 0 -> None
    | disp -> Some disp
  in
  match (mode, rm) with
  | 0b11, rm -> (
      match parse word rm with
      | Some reg -> (false, Some reg, None, None)
      (* not sure about the none case *)
      | None -> failwith "Not sure on parsing rm")
  | _, 0b000 -> (true, Some BX, Some SI, disp)
  | _, 0b001 -> (true, Some BX, Some DI, disp)
  | _, 0b010 -> (true, Some BP, Some SI, disp)
  | _, 0b011 -> (true, Some BP, Some DI, disp)
  | _, 0b100 -> (true, Some SI, None, disp)
  | _, 0b101 -> (true, Some DI, None, disp)
  | 0b00, 0b110 -> (true, None, None, disp)
  | _, 0b110 -> (true, Some BP, None, disp)
  | _, 0b111 -> (true, Some BX, None, disp)
  | _ -> failwith "Not sure on parsing rm"

let show_rm rm =
  match rm with
  | false, Some base, None, None -> Printf.sprintf "%s" (to_string base)
  | true, Some base, None, None -> Printf.sprintf "[%s]" (to_string base)
  | true, Some base, Some index, None ->
      Printf.sprintf "[%s + %s]" (to_string base) (to_string index)
  | true, Some base, None, Some disp ->
      Printf.sprintf "[%s + %d]" (to_string base) disp
  | true, Some base, Some index, Some disp ->
      Printf.sprintf "[%s + %s + %d]" (to_string base) (to_string index) disp
  | true, None, None, Some disp -> Printf.sprintf "[%d]" disp
  | _ -> 
      let () = print_endline (show_rm rm) in
      failwith "Not sure on showing rm"

let show_data word data =
  match word with
  | true -> Printf.sprintf "word %d" data
  | false -> Printf.sprintf "byte %d" data
