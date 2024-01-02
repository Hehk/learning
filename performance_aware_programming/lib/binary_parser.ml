open Base

type 'a cursor = { location : int; value : 'a }

let take_n n bytes cursor =
  let total_bits = List.length bytes * 8 in
  if cursor.location + n > total_bits then failwith "Not enough bits left"
  else
    let rec extract_bits acc i location =
      if i = n then acc
      else
        let byte_index = location / 8 in
        let bit_index = location % 8 in
        let byte = List.nth_exn bytes byte_index in
        let bit = (byte lsr (7 - bit_index)) land 1 in
        extract_bits ((acc lsl 1) lor bit) (i + 1) (location + 1)
    in
    let result = extract_bits 0 0 cursor.location in
    let new_cursor =
      { location = cursor.location + n; value = (cursor.value, result) }
    in
    (result, new_cursor)

let bit_string_to_int bits =
  bits |> String.to_list
  |> List.map ~f:(fun bit -> if Char.equal bit '1' then 1 else 0)
  |> List.fold ~init:0 ~f:(fun acc bit -> (acc lsl 1) lor bit)

let b bits cursor bytes =
  let n = String.length bits in
  let bits = bit_string_to_int bits in
  let next_bits, new_cursor = take_n bits bytes cursor in
  if bits = next_bits then
    let new_cursor = { new_cursor with value = (new_cursor.value, bits) } in
    Some new_cursor
  else None

let reg cursor bytes =
  let _, new_cursor = take_n 3 bytes cursor in
  Some new_cursor

let rem cursor bytes =
  let _, new_cursor = take_n 3 bytes cursor in
  Some new_cursor

let ( >>= ) f g cursor bytes =
  match f cursor bytes with Some cursor -> g cursor bytes | None -> None

let ( <|> ) f g cursor bytes =
  match f cursor bytes with Some x -> x | None -> g cursor bytes
