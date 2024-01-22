(* Really simple implementation so I can play with twos complement myself *)
open Base

type bit = Zero | One
[@@deriving show]
type t = bit list
[@@deriving show]

let to_bits n =
  let rec loop n =
    if n = 0 then
      []
    else
      let bit = if n land 1 = 0 then Zero else One in
      bit :: loop (n lsr 1)
  in
  let bits = loop n in
  if List.length bits < 8 then
    List.append (List.init (8 - List.length bits) ~f:(fun _ -> Zero)) bits
  else
    bits

let lnot bits =
  List.map ~f:(function Zero -> One | One -> Zero) bits

let add a b =
  let add_bit (carry, acc) (a, b) =
    match a, b, carry with
    | Zero, Zero, false -> (false, Zero :: acc)
    | Zero, Zero, true -> (false, One :: acc)
    | Zero, One, false -> (false, One :: acc)
    | Zero, One, true -> (true, Zero :: acc)
    | One, Zero, false -> (false, One :: acc)
    | One, Zero, true -> (true, Zero :: acc)
    | One, One, false -> (true, Zero :: acc)
    | One, One, true -> (true, One :: acc) in
  List.zip_exn a b |> List.rev |> List.fold ~init:(false, []) ~f:add_bit |> snd

let encode n =
  let n = n land 0xFF in
  let bits = to_bits n in
  bits

let invert bits =
  let rec loop bits =
    match bits with
    | [] -> []
    | Zero :: rest -> One :: loop rest
    | One :: rest -> Zero :: loop rest
  in
  let bits = loop bits in
  let bits = add bits @@ encode 1 in
  bits


let value bits =
  let pos = phys_equal (List.hd_exn bits) Zero in
  let tl = List.tl_exn bits in
  let value' bits =
    bits
    |> List.rev
    |> List.foldi ~init:0 ~f:(fun i acc -> function
      | Zero -> acc
      | One -> 2 ** i + acc
    ) in
  if pos then 
    value' tl
  else
    -1 * (value' (lnot tl) + 1)
