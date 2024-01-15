open Base
open Stdio

(* Remove the filename stuff at the start *)
let format_expected expected =
  let lines = String.split_lines expected in
  let tail = List.drop lines 1 in
  let final_register_index =
    List.findi tail ~f:(fun _ line ->
        String.is_prefix line ~prefix:"Final register")
    |> Option.value_exn |> fst
  in
  let expected_log =
    List.take tail final_register_index
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun line -> String.( <> ) line "")
  in
  let expected_result =
    List.drop tail (final_register_index + 1)
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun line -> String.( <> ) line "")
  in
  (expected_result, expected_log)

let rec left_pad s n = if String.length s >= n then s else left_pad ("0" ^ s) n

let show_state (state : Cpu.Execute.state) =
  let show_register register =
    let value = Cpu.Execute.read_register state.registers register in
    Printf.sprintf "%s: 0x%s (%d)"
      (Cpu.Register.to_string register)
      (left_pad (Printf.sprintf "%x" value) 4)
      value
  in
  let log =
    Cpu.Register.[ AX; BX; CX; DX; SP; BP; SI; DI ]
    |> List.filter ~f:(fun register ->
           Cpu.Execute.read_register state.registers register <> 0)
    |> List.map ~f:show_register
  in
  let log =
    if state.ip <> 0 then
      log
      @ [
          Printf.sprintf "ip: 0x%s (%d)" (left_pad (Printf.sprintf "%x" state.ip) 4) state.ip;
        ]
    else log
  in
  let flags = Cpu.Execute.read_flags state.flags in
  let log =
    if String.equal flags "" then log else log @ [ "flags: " ^ flags ]
  in
  log

let tests_from_listing file_name =
  let expected_result, expected_log =
    format_expected @@ In_channel.read_all (file_name ^ ".txt")
  in
  let instructions = file_name |> Cpu.read_bytes |> Cpu.from_bytes in
  let instruction_log =
    instructions |> List.map ~f:Cpu.Instruction.show |> String.concat ~sep:"\n"
  in
  let result, logs = Cpu.exec instructions in
  let test_log () =
    let () = print_endline @@ "Instructions: \n" ^ instruction_log in
    Alcotest.(check (list string)) "same" expected_log logs
  in
  let test_result () =
    let actual_result = show_state result in
    Alcotest.(check (list string)) "same" expected_result actual_result
  in
  [
    Alcotest.test_case "Log" `Quick test_log;
    Alcotest.test_case "Result" `Quick test_result;
  ]

let tests_from_listing_with_ip file_name =
  let expected_result, expected_log =
    format_expected @@ In_channel.read_all (file_name ^ ".txt")
  in
  let bytes = file_name |> Cpu.read_bytes in
  let instruction_log =
    bytes |> Cpu.from_bytes
    |> List.map ~f:Cpu.Instruction.show
    |> String.concat ~sep:"\n"
  in
  let result, logs = Cpu.Execute.exec_bytes bytes in
  let test_log () =
    let () = print_endline @@ "Instructions: \n" ^ instruction_log in
    Alcotest.(check (list string)) "same" expected_log logs
  in
  let test_result () =
    let actual_result = show_state result in
    Alcotest.(check (list string)) "same" expected_result actual_result
  in
  [
    Alcotest.test_case "Log" `Quick test_log;
    Alcotest.test_case "Result" `Quick test_result;
  ]

let part1 = "../computer_enhance/perfaware/part1/"

let () =
  Alcotest.run "Exec"
    [
      ("Listing 43", tests_from_listing @@ part1 ^ "listing_0043_immediate_movs");
      ("Listing 44", tests_from_listing @@ part1 ^ "listing_0044_register_movs");
      ("Listing 46", tests_from_listing @@ part1 ^ "listing_0046_add_sub_cmp");
      ( "Listing 48",
        tests_from_listing_with_ip @@ part1 ^ "listing_0048_ip_register" );
      ( "Listing 49",
        tests_from_listing_with_ip @@ part1 ^ "listing_0049_conditional_jumps"
      );
    ]
