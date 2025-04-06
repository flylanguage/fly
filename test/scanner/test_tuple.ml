open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf = 
    let tk = Scanner.tokenize lexbuf in
    match tk with
    | Fly_lib.Parser.EOF -> []
    | t -> t :: to_list lexbuf

let tests = "testing_tuple" >::: [
    
        "test1" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let x1 := (1, 2, 3);\nx1;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "LET ID(x1) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) RPAREN SEMI ID(x1) SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test2" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let x2 := (1, 2.5, \"hello\");\nx2;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "LET ID(x2) WALRUS LPAREN LITERAL(1) COMMA FLIT(2.500000) COMMA SLIT(hello) RPAREN SEMI ID(x2) SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test3" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let x3 := (true, false);\nx3;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "LET ID(x3) WALRUS LPAREN BLIT(true) COMMA BLIT(false) RPAREN SEMI ID(x3) SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test4" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let x4 := (1, 2, 3);\nx4[0];\nx4[1];\nx4[2];\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "LET ID(x4) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) RPAREN SEMI ID(x4) LBRACKET LITERAL(0) RBRACKET SEMI ID(x4) LBRACKET LITERAL(1) RBRACKET SEMI ID(x4) LBRACKET LITERAL(2) RBRACKET SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test5" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let x5 := (1, 2, 3);\nx5[1] := 4;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "LET ID(x5) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) RPAREN SEMI ID(x5) LBRACKET LITERAL(1) RBRACKET WALRUS LITERAL(4) SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test6" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let x6 := (1, 2, 3);\nlet x7 := (4, 5, 6);\nx6 + x7;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "LET ID(x6) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) RPAREN SEMI LET ID(x7) WALRUS LPAREN LITERAL(4) COMMA LITERAL(5) COMMA LITERAL(6) RPAREN SEMI ID(x6) PLUS ID(x7) SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test7" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let x8 := (\"hello\", \"world\");\nx8;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "LET ID(x8) WALRUS LPAREN SLIT(hello) COMMA SLIT(world) RPAREN SEMI ID(x8) SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test8" >:: (fun _ ->
        let lexbuf = Lexing.from_string "" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test9" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let x10 := (1, 2);\nlet x11 := 3;\nx10 + x11;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "LET ID(x10) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) RPAREN SEMI LET ID(x11) WALRUS LITERAL(3) SEMI ID(x10) PLUS ID(x11) SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test10" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let x12 := (1, [2, 3], 4);\nx12;  // Should pass: Tuple (1, [2, 3], 4)\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "LET ID(x12) WALRUS LPAREN LITERAL(1) COMMA LBRACKET LITERAL(2) COMMA LITERAL(3) RBRACKET COMMA LITERAL(4) RPAREN SEMI ID(x12) SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

        "test11" >:: (fun _ ->
        let lexbuf = Lexing.from_string "let x13 := ({1, 2}, 3);\nx13;\n" in
        let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
        let expected = "LET ID(x13) WALRUS LPAREN LBRACE LITERAL(1) COMMA LITERAL(2) RBRACE COMMA LITERAL(3) RPAREN SEMI ID(x13) SEMI" in
        assert_equal 
        expected actual
        ~printer:(fun s -> "\"" ^ s ^ "\""));

]

let _ = run_test_tt_main tests

