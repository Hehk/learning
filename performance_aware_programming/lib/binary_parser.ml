open Base

type cursor = { location : int; value : (string * int) list }

let ( let* ) x f = Option.bind ~f x

let value cursor_value key =
  let* value = List.find cursor_value ~f:(fun (k, _) -> String.equal k key) in
  Some (snd value)

let add_value cursor key value =
  { cursor with value = (key, value) :: cursor.value }

let take_n n cursor bits =
  let bits = List.sub bits ~pos:cursor.location ~len:n in
  let value = List.fold bits ~init:0 ~f:(fun acc bit -> (acc lsl 1) lor bit) in
  let new_cursor = { cursor with location = cursor.location + n } in
  (value, new_cursor)

let bit_string_to_int bits =
  bits |> String.to_list
  |> List.map ~f:(fun bit -> if Char.equal bit '1' then 1 else 0)
  |> List.fold ~init:0 ~f:(fun acc bit -> (acc lsl 1) lor bit)

let b bit_pattern cursor bits =
  let bit_value = bit_string_to_int bit_pattern in
  let n = String.length bit_pattern in
  let value, new_cursor = take_n n cursor bits in
  if value = bit_value then Some new_cursor else None

let w cursor bits =
  let value, new_cursor = take_n 1 cursor bits in
  let new_cursor = add_value new_cursor "w" value in
  Some new_cursor

let d cursor bits =
  let value, new_cursor = take_n 1 cursor bits in
  let new_cursor = add_value new_cursor "d" value in
  Some new_cursor

let s cursor bits =
  let value, new_cursor = take_n 1 cursor bits in
  let new_cursor = add_value new_cursor "s" value in
  Some new_cursor

let reg cursor bits =
  let value, new_cursor = take_n 3 cursor bits in
  let new_cursor = add_value new_cursor "reg" value in
  Some new_cursor

let rem cursor bytes =
  let value, new_cursor = take_n 2 cursor bytes in
  let new_cursor = add_value new_cursor "rem" value in
  Some new_cursor

let parser pg cursor bits =
  let parsers, f = pg in
  let cursor =
    List.fold parsers ~init:(Some cursor) ~f:(fun cursor p ->
        match cursor with Some c -> p c bits | None -> None)
  in
  Option.map cursor ~f:(fun cursor -> (cursor, f cursor.value))

let parse bytes parser_groups =
  let to_bits byte =
    let bit i = (byte lsr i) land 1 in
    let rec aux i =
      let bit = bit i in
      bit :: aux (i - 1)
    in
    aux 7
  in
  let bits = bytes |> List.map ~f:to_bits |> List.concat in
  let run_parser cursor =
    List.find_map parser_groups ~f:(fun parser_group ->
        parser parser_group cursor bits)
  in
  let cursor = { location = 0; value = [] } in
  List.fold ~init:(cursor, [])
    ~f:(fun (cursor, acc) _ ->
      match run_parser cursor with
      | Some (cursor, value) ->
          let new_cursor = { cursor with value = [] } in
          (new_cursor, value :: acc)
      | None -> failwith "No parser matched")
    bits
