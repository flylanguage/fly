
let get_token_list lexbuf = 
    let rec work acc = 
        match Scanner.tokenize lexbuf with 
        | EOF -> acc
        | t -> work (t :: acc)
    in List.rev (work [])

let pp_token = function
    | EQ -> "="
    | DIVIDE -> "/"
    | LITERAL(i) -> sprintf "%d" i
    | VARIABLE(v) -> sprintf ":%s:" v
    | SEMI -> ";"
    | PLUS -> "+"
    | MINUS -> "-"
    | TIMES -> "*"
    | EOF -> "EOF"


let _ =
  let lexbuf = Lexing.from_channel stdin in

  (* Debug Tokens *)
  let token_list = get_token_list lexbuf in
  List.map pp_token token_list |> List.iter (printf "%s\n")
