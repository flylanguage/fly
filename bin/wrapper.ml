(*
   This file stores wrappers to c lib code
*)

(* We need to access libc specific stuff that is platform dependent: 
    For example, the global "stdin" symbol in libc is actually called __stdinp on macos
    We could either do a match case on which platform we're on, but instead 
    We create c wrapper objects which allow us to use constant IR *)
let wrapper = "#include <stdio.h>\nFILE *get_stdin() { return stdin; }"

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

let compile_wrapper =
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
