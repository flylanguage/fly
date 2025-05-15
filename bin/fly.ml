open Fly_lib
module L = Llvm

type action =
  | Tokens
  | Ast
  | IR
  | Code

(* By default, generate code *)
let act = ref Code
let set_action a () = act := a

let speclist =
  [ "--tokens", Arg.Unit (set_action Tokens), "Print Tokens to stdout"
  ; "--ast", Arg.Unit (set_action Ast), "Print Ast to stdout"
  ; "--ir", Arg.Unit (set_action IR), "Print IR to stdout"
  ]
;;

let usg = "Usage: ./fly <filename>.fly"
let exe_name = "fly.out"

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

(* Reads all the bytes from a channel (not possible in ocaml for some reason) *)
let read_all_from_channel (ic : in_channel) : bytes =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf ic 1024
     done
   with
   | End_of_file -> ());
  Buffer.to_bytes buf
;;

(* We need to access libc specific stuff that is platform dependent: 
    For example, the global "stdin" symbol in libc is actually called __stdinp on macos
    We could either do a match case on which platform we're on, but instead 
    We create c wrapper objects which allow us to use constant IR *)
let compile_wrapper =
  let wrapper = "#include <stdio.h>\nFILE *get_stdin() { return stdin; }" in
  let argv = [ "clang"; "-x"; "c"; "-"; "-c"; "-o"; "/dev/stdout" ] in

  let cmd = String.concat " " argv in
  let child_stdout, child_stdin = Unix.open_process cmd in

  (* Write into stdin *)
  output_string child_stdin wrapper;
  flush child_stdin;
  (* Write EOF and close channel *)
  close_out child_stdin;

  let contents = read_all_from_channel child_stdout in

  let filename = Filename.get_temp_dir_name () ^ "fly_wrapper.o" in
  let pipe = open_out_bin filename in
  output_bytes pipe contents;
  close_out pipe;

  filename
;;

(* Just delete the tmp file created with the wrapper object *)
let cleanup_wrapper filename = Sys.remove filename

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

  (* Compile asm to an executable using clang - reading the asm from stdin *)
  let tmp_filename = compile_wrapper in
  let argv =
    [ "gcc"; "-x"; "assembler"; "/dev/stdin"; "-x"; "none"; tmp_filename; "-o"; exe_name ]
  in
  let cmd = String.concat " " argv in
  let child_stdout, child_stdin = Unix.open_process cmd in

  (* Write the asm to the child's stdin *)
  output_string child_stdin asmstr;
  flush child_stdin;
  (* send EOF *)
  close_out child_stdin;

  (* Fun.protect ensures that the cleanup_wrapper (in the "finally" function) 
     is always called, even if the match case raises *)
  Fun.protect
    ~finally:(fun () -> cleanup_wrapper tmp_filename)
    (fun () ->
       match Unix.close_process (child_stdout, child_stdin) with
       | WEXITED 0 -> ()
       | err ->
         let msg =
           match err with
           | WEXITED n -> Printf.sprintf "exit: %d" n
           | WSIGNALED s -> Printf.sprintf "signal: %d" s
           | _ -> "unknown"
         in
         raise (Failure (Printf.sprintf "Failed to generate executable: error=%s\n" msg)))
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
