open Scanner
open Prints

let read_file file = In_channel.with_open_bin file In_channel.input_all (* reads entire file into a string *)

let write_one_token out_channel token = Printf.fprintf out_channel "%s\n" (print_token token)


let write_tokens_to_file in_file out_file = 
  let input = read_file in_file in
  let lexbuf = Lexing.from_string input in
  let tokenseq = Scanner.tokenize lexbuf in
  let out_channel = open_out out_file in
  List.iter (write_one_token out_channel) tokenseq;

  close_out out_channel;


let _ = 
  if Array.length Sys.argv < 3 then
    Printf.eprintf "Usage: %s <test_program> <output_file>\n" Sys.argv.(0)
  else
    let in_file = Sys.argv.(1) in
    let out_file = Sys.argv.(2) in
    write_tokens_to_file in_file out_file;

