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
  (* TODO: handle this correctly *)
  | None -> Printf.sprintf "reg(%d)" reg
