open Base
open Stdio

let parse_log log =
  let open Cpu.Execute in
  let sections = String.split ~on:';' log in
  let instruction = List.hd_exn sections |> String.strip in
  let log = List.tl_exn sections |> List.hd_exn |> String.split ~on:'|' in
  let clock, log =
    if List.length log = 1 then ("", List.hd_exn log)
    else
      let clock = List.hd_exn log |> String.strip in
      let log = List.tl_exn log |> List.hd_exn |> String.strip in
      (clock, log)
  in
  let log = String.split ~on:' ' log |> List.map ~f:String.strip in
  let ip = List.find log ~f:(fun s -> String.is_prefix s ~prefix:"ip") in
  let flag = List.find log ~f:(fun s -> String.is_prefix s ~prefix:"flags") in
  let diff =
    List.find log ~f:(fun s ->
        String.is_substring ~substring:"->0x" s
        && (not @@ String.is_prefix s ~prefix:"ip"))
  in
  { instruction; ip; flag; diff; clock; raw = Cpu.Instruction.Jump_on_less 0 }

(* Remove the filename stuff at the start *)
let format_expected expected =
  let lines = String.split_lines expected in
  let start_line =
    List.findi lines ~f:(fun _ line -> String.is_prefix line ~prefix:"--- ")
    |> Option.value_exn |> fst
  in
  (* some test data has sections with comments *)
  let last_line =
    List.findi lines ~f:(fun i line ->
        String.is_prefix line ~prefix:"***" && i > start_line)
  in
  let last_line =
    Option.map last_line ~f:fst |> Option.value ~default:(List.length lines)
  in
  let lines = List.drop lines (start_line + 1) in
  let lines = List.take lines (last_line - start_line - 1) in
  let final_register_index =
    List.findi lines ~f:(fun _ line ->
        String.is_prefix line ~prefix:"Final register")
    |> Option.value_exn |> fst
  in
  let expected_log =
    List.take lines final_register_index
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun line -> String.( <> ) line "")
    |> List.map ~f:parse_log
  in
  let expected_result =
    List.drop lines (final_register_index + 1)
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun line -> String.( <> ) line "")
  in
  (expected_result, expected_log)

let compare_logs field expected actual =
  let open Cpu.Execute in
  let selector =
    match field with
    | `Instruction ->
        let () = print_endline "Comparing instructions" in
        fun x -> x.instruction
    | `Ip ->
        let () = print_endline "Comparing ips" in
        fun x -> Option.value x.ip ~default:""
    | `Flag ->
        let () = print_endline "Comparing flags" in
        fun x -> Option.value x.flag ~default:""
    | `Diff ->
        let () = print_endline "Comparing diffs" in
        fun x -> Option.value x.diff ~default:""
    | `Clock ->
        let () = print_endline "Comparing clocks" in
        fun x -> x.clock
  in
  let actual_logs = List.map ~f:(Cpu.Execute.show_log) actual in
  let () = print_endline @@ "Actual logs: " ^ String.concat ~sep:"\n" actual_logs in
  let expected = List.map ~f:selector expected in
  let actual = List.map ~f:selector actual in
  let () = print_endline @@ "Expected: " ^ String.concat ~sep:"\n" expected in
  let () = print_endline @@ "Actual: " ^ String.concat ~sep:"\n" actual in
  Alcotest.(check (list string)) "same" expected actual

let rec left_pad s n = if String.length s >= n then s else left_pad ("0" ^ s) n

let show_state fields (state : Cpu.Execute.state) =
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
  let has_ip =
    not @@ phys_equal None (List.find ~f:(fun x -> phys_equal `Ip x) fields)
  in
  let has_flag =
    not @@ phys_equal None (List.find ~f:(fun x -> phys_equal `Flag x) fields)
  in
  let log =
    if state.ip <> 0 && has_ip then
      log
      @ [
          Printf.sprintf "ip: 0x%s (%d)"
            (left_pad (Printf.sprintf "%x" state.ip) 4)
            state.ip;
        ]
    else log
  in
  let flags = Cpu.Execute.read_flags state.flags in
  let log =
    if String.equal flags "" && has_flag then log
    else log @ [ "flags: " ^ flags ]
  in
  log

type checks = [ `Instruction | `Ip | `Flag | `Diff | `Clock ] [@@deriving show]

let tests_from_listing checks file_name =
  let expected_result, expected_log =
    format_expected @@ In_channel.read_all (file_name ^ ".txt")
  in
  let instructions = file_name |> Cpu.read_bytes in
  let result, logs = Cpu.Execute.exec_bytes instructions in
  let test_result () =
    let actual_result = show_state checks result in
    Alcotest.(check (list string)) "same" expected_result actual_result
  in
  let create_test check =
    Alcotest.test_case (show_checks check) `Quick (fun _ ->
        compare_logs check expected_log logs)
  in
  let tests = List.map ~f:create_test checks in
  tests @ [ Alcotest.test_case "Result" `Quick test_result ]

let part1 = "../computer_enhance/perfaware/part1/"

let () =
  Alcotest.run "Exec"
    [
      ( "Listing 43",
        tests_from_listing [ `Diff; `Flag ]
        @@ part1 ^ "listing_0043_immediate_movs" );
      ( "Listing 44",
        tests_from_listing [ `Diff; `Flag ]
        @@ part1 ^ "listing_0044_register_movs" );
      ( "Listing 46",
        tests_from_listing [ `Diff; `Flag ]
        @@ part1 ^ "listing_0046_add_sub_cmp" );
      ( "Listing 48",
        tests_from_listing [ `Diff; `Flag; `Ip ]
        @@ part1 ^ "listing_0048_ip_register" );
      ( "Listing 49",
        tests_from_listing [ `Diff; `Flag; `Ip ]
        @@ part1 ^ "listing_0049_conditional_jumps" );
      ( "Listing 51",
        tests_from_listing [ `Diff; `Flag; `Ip ]
        @@ part1 ^ "listing_0051_memory_mov" );
      ( "Listing 52",
        tests_from_listing [ `Diff; `Flag; `Ip ]
        @@ part1 ^ "listing_0052_memory_add_loop" );
      ( "Listing 56",
        tests_from_listing [ `Diff; `Flag; `Ip; `Clock ]
        @@ part1 ^ "listing_0056_estimating_cycles" );
    ]
