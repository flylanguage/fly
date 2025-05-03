open Fly_lib
module L = Llvm

type action =
  | Tokens
  | Ast
  | IR
  | Code

let act = ref Code
let set_action a () = act := a

let speclist =
  [ "--tokens", Arg.Unit (set_action Tokens), "Print Tokens to stdout"
  ; "--ast", Arg.Unit (set_action Ast), "Print Ast to stdout"
  ; "--ir", Arg.Unit (set_action IR), "Print IR to stdout"
  ]
;;

let usg = "Usage: ./fly <filename>.fly"

let rec to_token_list lexbuf =
  let tk = Fly_lib.Scanner.tokenize lexbuf in
  match tk with
  | EOF -> []
  | t -> t :: to_token_list lexbuf
;;

let read_and_print_tokens channel =
  try
    while true do
      let line = input_line channel in
      let tokens = to_token_list (Lexing.from_string line) in
      print_endline (Utils.string_of_tokens tokens)
    done
  with
  | End_of_file -> close_in_noerr channel
  | e ->
    close_in_noerr channel;
    raise e
;;

let read_and_print_ast channel =
  let lexbuf = Lexing.from_channel channel in
  let ast = Fly_lib.Parser.program_rule Fly_lib.Scanner.tokenize lexbuf in
  print_endline (Utils.string_of_program ast)
;;

let read_and_print_ir channel =
  let lexbuf = Lexing.from_channel channel in
  let ast = Fly_lib.Parser.program_rule Fly_lib.Scanner.tokenize lexbuf in
  let sast = Semant.check ast.body in
  let md = Irgen.translate sast in
  print_endline (L.string_of_llmodule md)
;;

let read_and_compile channel =
  let lexbuf = Lexing.from_channel channel in
  let ast = Fly_lib.Parser.program_rule Fly_lib.Scanner.tokenize lexbuf in
  let sast = Semant.check ast.body in
  let md = Irgen.translate sast in

  (* Inititalize triples that llvm needs to create a target *)
  Llvm_all_backends.initialize ();
  let triple = Llvm_target.Target.default_triple () in
  let tm =
    Llvm_target.TargetMachine.create ~triple (Llvm_target.Target.by_triple triple)
  in

  (* Generate asm code in a memory buffer for our target *)
  let asmbuf =
    Llvm_target.TargetMachine.emit_to_memory_buffer
      md
      Llvm_target.CodeGenFileType.AssemblyFile
      tm
  in

  let asmstr = Llvm.MemoryBuffer.as_string asmbuf in

  (* Compile asm to an executable using gcc - reading the asm from stdin *)
  let argv = [ "gcc"; "-x"; "assembler"; "/dev/stdin"; "-o"; "a.out" ] in
  let cmd = String.concat " " argv in
  let child_stdout, child_stdin = Unix.open_process cmd in

  (* Write the asm to the child's stdin *)
  output_string child_stdin asmstr;
  flush child_stdin;
  (* send EOF *)
  close_out child_stdin;

  match Unix.close_process (child_stdout, child_stdin) with
  | WEXITED 0 -> ()
  | err ->
    let msg =
      match err with
      | WEXITED n -> Printf.sprintf "exit: %d" n
      | WSIGNALED s -> Printf.sprintf "signal: %d" s
      | _ -> "unknown"
    in
    raise (Failure (Printf.sprintf "Failed to generate executable: error=%s\n" msg))
;;

let () =
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usg;
  match !act with
  | Tokens -> read_and_print_tokens !channel
  | Ast -> read_and_print_ast !channel
  | IR -> read_and_print_ir !channel
  | Code -> read_and_compile !channel
;;
