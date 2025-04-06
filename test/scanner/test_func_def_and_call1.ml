open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf = 
    let tk = Scanner.tokenize lexbuf in
    match tk with
    | Fly_lib.Parser.EOF -> []
    | t -> t :: to_list lexbuf

let tests = "testing_func_def_and_call1" >::: [
    
        "test1" >:: (fun _ ->
        let lexbuf = Lexing.from_string "fun countdown(timer: int) {\n	while (timer > 0) {\n		timer -= 1;\n	}\n}\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "FUN ID(countdown) LPAREN ID(timer) COLON INT RPAREN LBRACE WHILE LPAREN ID(timer) GT LITERAL(0) RPAREN LBRACE ID(timer) MINUS_ASSIGN LITERAL(1) SEMI RBRACE RBRACE" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test2" >:: (fun _ ->
        let lexbuf = Lexing.from_string "countdown(10);\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "ID(countdown) LPAREN LITERAL(10) RPAREN SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

]

let _ = run_test_tt_main tests

