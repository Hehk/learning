open Base
open Stdio
module Instruction = Instruction
module Register = Register
module Execute = Execute

let read_bytes = Util.read_bytes
let show_bytes = Instruction.show_raw_bytes

let from_bytes = Instruction.from_bytes
let show_asm = Instruction.show_asm

