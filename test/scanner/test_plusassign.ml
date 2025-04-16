open OUnit2
open Fly_lib
open Print_lib.Prints

let rec to_list lexbuf =
  let tk = Scanner.tokenize lexbuf in
  match tk with
  | Fly_lib.Parser.EOF -> []
  | t -> t :: to_list lexbuf
;;

let tests =
  "testing_plusassign"
  >::: [ ("test1"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x1 := 5;\nx1 += 3;\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET MUT ID(x1) WALRUS LITERAL(5) SEMI ID(x1) PLUS_ASSIGN LITERAL(3) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test2"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x2 := 5.5;\nx2 += 3.5;\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET MUT ID(x2) WALRUS FLIT(5.500000) SEMI ID(x2) PLUS_ASSIGN FLIT(3.500000) \
             SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test3"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x3 := 5;\nx3 += 3.5;\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET MUT ID(x3) WALRUS LITERAL(5) SEMI ID(x3) PLUS_ASSIGN FLIT(3.500000) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test4"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let x4 := 5.5;\nx4 += 3;\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET ID(x4) WALRUS FLIT(5.500000) SEMI ID(x4) PLUS_ASSIGN LITERAL(3) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test5"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x6 := true;\nx6 += false;\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET MUT ID(x6) WALRUS BLIT(true) SEMI ID(x6) PLUS_ASSIGN BLIT(false) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test6"
          >:: fun _ ->
          let lexbuf =
            Lexing.from_string "let mut x7 := \"Hello\";\nx7 += \" World\";\n"
          in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET MUT ID(x7) WALRUS SLIT(Hello) SEMI ID(x7) PLUS_ASSIGN SLIT( World) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test7"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x8 := [1, 2, 3];\nx8 += [4, 5];\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET MUT ID(x8) WALRUS LBRACKET LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACKET SEMI ID(x8) PLUS_ASSIGN LBRACKET LITERAL(4) COMMA LITERAL(5) \
             RBRACKET SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test8"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x9 := {1, 2, 3};\nx9 += {4, 5};\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET MUT ID(x9) WALRUS LBRACE LITERAL(1) COMMA LITERAL(2) COMMA LITERAL(3) \
             RBRACE SEMI ID(x9) PLUS_ASSIGN LBRACE LITERAL(4) COMMA LITERAL(5) RBRACE \
             SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test9"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x10 := 'a';\nx10 += 'b';\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET MUT ID(x10) WALRUS CLIT(a) SEMI ID(x10) PLUS_ASSIGN CLIT(b) SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ; ("test10"
          >:: fun _ ->
          let lexbuf = Lexing.from_string "let mut x11 := (1, 2);\nx11 += (3, 4);\n" in
          let actual = List.map print_token (to_list lexbuf) |> String.concat " " in
          let expected =
            "LET MUT ID(x11) WALRUS LPAREN LITERAL(1) COMMA LITERAL(2) RPAREN SEMI \
             ID(x11) PLUS_ASSIGN LPAREN LITERAL(3) COMMA LITERAL(4) RPAREN SEMI"
          in
          assert_equal expected actual ~printer:(fun s -> "\"" ^ s ^ "\""))
       ]
;;

let _ = run_test_tt_main tests
