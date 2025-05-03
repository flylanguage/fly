open Fly_lib

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

let () =
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usg;
  (* read !channel *)
  (* let lexbuf = Lexing.from_channel !channel in *)
  match !act with
  | Tokens -> read_and_print_tokens !channel
  | Ast -> read_and_print_ast !channel
  | _ -> raise (Failure "not implemented")
;;
