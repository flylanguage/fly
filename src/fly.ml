(* open Printf

let get_token_list lexbuf =
  let rec work acc =
    match Scanner.tokenize lexbuf with
    | EOF -> acc
    | t -> work (t :: acc)
  in
  List.rev (work [])
;;

let pp_token = function
  | EQ -> "="
  | DIVIDE -> "/"
  | LITERAL i -> sprintf "%d" i
  | ID v -> sprintf ":%s:" v
  | SEMI -> ";"
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | EOF -> "EOF"
  | _ -> "??"
;;

let _ =
  let lexbuf = Lexing.from_channel stdin in
  (* Debug Tokens *)
  let token_list = get_token_list lexbuf in
  List.map pp_token token_list |> List.iter (printf "%s\n") *)

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

let () =
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usg;
  match !act with
  | Code -> print_endline "Coding"
  | Tokens -> print_endline "scanning"
  | Ast -> print_endline "Ast-ing"
  | IR -> print_endline "IR-ing"
;;
