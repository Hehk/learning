open Base
open Stdio

let ( % ) f g x = f (g x)

let read_bytes file_name =
  let ic = In_channel.create file_name in
  let rec read_all acc =
    match In_channel.input_byte ic with
    | None -> List.rev acc
    | Some b -> read_all (b :: acc)
  in
  let bytes = read_all [] in
  In_channel.close ic;
  bytes

let read_asm file_name =
  file_name |> In_channel.read_lines |> List.map ~f:String.strip
  |> List.filter ~f:(not % String.is_empty)
  |> List.filter ~f:(not % String.is_prefix ~prefix:";")
  |> List.filter ~f:(not % String.is_prefix ~prefix:"bits")

let show_asm_file instructions =
  "bits 16\n" ^ String.concat ~sep:"\n" instructions

let run_command_get_output cmd =
  let ic = Unix.open_process_in cmd in
  let lines = In_channel.input_lines ic in
  lines

let generate_bytes line =
  let tmp_file = "/tmp/temp" in
  let output_file = "/tmp/temp.bin" in
  let content = show_asm_file [ line ] in
  let () = Stdio.Out_channel.write_all tmp_file ~data:content in
  let _ =
    run_command_get_output
    @@ Printf.sprintf "nasm -f bin %s -o %s" tmp_file output_file
  in
  let bytes = read_bytes output_file in
  let () = Unix.unlink tmp_file in
  let () = Unix.unlink output_file in
  bytes

let show_bytes bytes =
  let show_byte byte =
    String.init 8 ~f:(fun i -> if byte land (1 lsl (7 - i)) = 0 then '0' else '1')
  in
  String.concat ~sep:" " @@ List.map ~f:show_byte bytes


let line_to_test i line =
  let log title msg =
    Stdio.print_endline @@ title ^ ":\n" ^ msg ^ "\n" in
  let test_line () =
    let bytes = generate_bytes line in
    let () = log "Bytes" @@ show_bytes bytes in
    let instructions = Cpu.from_bytes bytes in
    let instruction = List.hd_exn instructions in
    let () = log "Instruction" (Cpu.Instruction.show instruction) in
    let asm = List.map ~f:Cpu.show_asm instructions in
    let () = log "Asm" (String.concat ~sep:"\n" asm) in
    let new_bytes = generate_bytes (List.hd_exn asm) in
    Alcotest.(check (list int)) "same" bytes new_bytes
  in
  Alcotest.test_case ("Line " ^ Int.to_string i ^ ": " ^ line) `Quick test_line

let tests_from_listing file_name =
  let lines = read_asm (file_name ^ ".asm") in
  List.mapi lines ~f:line_to_test

let part1 = "../computer_enhance/perfaware/part1/"

let () =
  Alcotest.run "Decode"
    [
      ( "listing 37",
        tests_from_listing @@ part1 ^ "listing_0037_single_register_mov" );
      ( "listing 38",
        tests_from_listing @@ part1 ^ "listing_0038_many_register_mov" );
      ("listing 39", tests_from_listing @@ part1 ^ "listing_0039_more_movs");
      ("listing 40", tests_from_listing @@ part1 ^ "listing_0040_challenge_movs");
    ]
